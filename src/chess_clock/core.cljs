(ns chess-clock.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan >! <! alts! timeout put!]]))

(defn minus-sec [time]
  (let [min (:min time)
        sec (:sec time)]
    (if (zero? sec)
      {:min (dec min) :sec 59}
      {:min min :sec (dec sec)})))

(defn time->string [time]
  (let [min (:min time)
        sec (:sec time)]
    (str min ":" (when (< sec 10) "0")  sec)))

(def app-state
  (atom {:white-clock {:time {:min 2 :sec 30}
                       :tag :white}
         :black-clock {:time {:min 2 :sec 30}
                       :tag :black}
         :msg "Ready"}))

(defn counter [app control]
  (go
   (loop [clock-state (cycle [:off :on])]
     (do
       (let [t (timeout 1000)
             [v c] (alts! [t control])]
         (cond
          (and (= (first clock-state) :on)
               (= c t))
          (do
            (om/transact! app :time minus-sec)
            (recur clock-state))
          (and (= (first clock-state) :off)
               (= c control)
               (= v :start))
          (recur (next clock-state))
          (and (= (first clock-state) :on)
               (= c control)
               (= v :stop))
          (recur (next clock-state))
          (and (= c control)
               (= v :end))
          (.log js/console "game over")
          :else
          (recur clock-state)))))))

(defn switch-clock [tag wc bc msgchan]
  (go
   (cond
    (= tag :white)
    (do
      (>! wc :stop)
      (>! bc :start)
      (>! msgchan "Black to move"))
    (= tag :black)
    (do
      (>! wc :start)
      (>! bc :stop)
      (>! msgchan "White to move"))
    (= tag :end)
    (do
      (>! wc :end)
      (>! bc :end)
      (>! msgchan "Game over")))))

(defn clock-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [input (om/get-state owner :input)
            ctrl (counter app input)]))
    om/IRenderState
    (render-state [this {:keys [master]}]
      (let [tag (:tag app)]
        (dom/div #js {:className "clock"}
                 (dom/h3 nil (str "Player " (name tag)))
                 (dom/h3 #js {:className "clockface"} (time->string (:time app)))
                 (dom/button #js {:onClick
                                  (fn [e] (put! master tag))} "Move"))))))

(defn board-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:white-control (chan)
       :black-control (chan)
       :message (chan)})
    om/IWillMount
    (will-mount [_]
      (let [main (om/get-state owner :main-control)
            message (om/get-state owner :message)
            wc (om/get-state owner :white-control)
            bc (om/get-state owner :black-control)]
        (go (loop []
              (let [tag (<! main)]
                (switch-clock tag wc bc message)
                (recur))))
        (go (loop []
              (let [msg (<! message)]
                (om/update! app [:msg] msg)
                (recur))))))
    om/IRenderState
    (render-state [this {:keys [white-control black-control main-control message]}]
      (dom/div nil
               (dom/h2 #js {:className "header"} "Chess Clocks")
               (dom/h3 #js {:className "message"} (:msg app))
               (dom/button #js {:onClick
                                (fn [e] (put! main-control :end))}
                           "End Game")
               (dom/div #js {:className "clocks"}
                        (om/build clock-view (:white-clock app)
                                  {:init-state {:master main-control
                                                :input white-control}})
                        (om/build clock-view (:black-clock app)
                                  {:init-state {:master main-control
                                                :input black-control}}))))))

(om/root
  board-view
  app-state
  (let [main (chan)]
    {:target (. js/document (getElementById "app"))
     :init-state {:main-control main}
     :tx-listen (fn [update new-state]
                  (when (= (:new-value update) {:min 0 :sec 0})
                   (put! main :end)))}))
