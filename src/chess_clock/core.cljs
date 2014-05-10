(ns chess-clock.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan >! <! alts! timeout put!]]))

(enable-console-print!)

(defn minus-sec [time]
  (let [min (:min time)
        sec (:sec time)]
    (if (zero? sec)
      {:min (dec min) :sec 59}
      {:min min :sec (dec sec)})))

(defn time->string [time]
  (let [min (:min time)
        sec  (subs (str (:sec time) "0") 0 2)]
    (str min ":" sec)))

(defn counter [app control msg-chan]
  (let [clock-state (cycle [:off :on])
        start-time (:time app)]
    (go
     (loop [time-left (:time @app) clock-state clock-state]
       (do
         (let [t (timeout 1000)
               [v c] (alts! [t control])]
           (cond
            (and (= (first clock-state) :on)
                 (= c t))
            (do
              (om/update! app [:time] time-left)
              (recur (minus-sec time-left) clock-state))
            (and (= (first clock-state) :off)
                 (= c control)
                 (= v :start))
            (do
              (put! msg-chan (str (name (:tag @app)) " to move"))
              (recur time-left (next clock-state)))
            (and (= (first clock-state) :on)
                 (= c control)
                 (= v :stop))
            (recur time-left (next clock-state))
            (and (= c control)
                 (= v :end))
            (.log js/console "time at end: " time-left)
            :else
            (recur time-left clock-state))))))))

(def app-state
  (atom {:white-clock {:time {:min 2 :sec 30}
                       :tag :white}
         :black-clock {:time {:min 2 :sec 30}
                       :tag :black}
         :msg "Ready"}))

(defn new-msg [app msg]
  (om/update! app [:msg] msg))

(defn switch-clock [tag wc bc msgchan]
  (go
   (cond
    (= tag :white)
    (do
      (>! wc :stop)
      (>! bc :start))
    (= tag :black)
    (do
      (>! wc :start)
      (>! bc :stop))
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
            msg-chan (om/get-state owner :msg-chan)
            ctrl (counter app input msg-chan)]))
    om/IRenderState
    (render-state [this {:keys [master input]}]
      (let [tag (:tag app)]
        (dom/div #js {:className "clock"}
                 (dom/h3 nil (str "Player " (name tag)))
                 (dom/h2 #js {:className "clockface"} (time->string (:time app)))
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
               (om/build clock-view (:white-clock app)
                         {:init-state {:master main-control
                                       :input white-control
                                       :msg-chan message}})
               (om/build clock-view (:black-clock app)
                         {:init-state {:master main-control
                                       :input black-control
                                       :msg-chan message}})))))

(om/root
  board-view
  app-state
  (let [main (chan)]
    {:target (. js/document (getElementById "app"))
     :init-state {:main-control main}
     :tx-listen (fn [update new-state]
                  (when (= (:new-value update) {:min 0 :sec 0})
                   (put! main :end)))}))
