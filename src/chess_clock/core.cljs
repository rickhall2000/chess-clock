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

#_(defn counter [app control]
  (let [clock-state (cycle [:off :on])
        ;control (:control app)
        time-left (:time app)]
    (go
     (loop [time-left time-left clock-state clock-state]
       (do
         (let [t (timeout 1000)
               [v c] (alts! [t #_control])]
           (cond
            (and (= (first clock-state) :on)
                 (= c t))
            (do
              (om/transact! app :time (fn [_] time-left))
              (recur (minus-sec time-left) clock-state))
            (and (= (first clock-state) :off)
                 (= c control)
                 (= v :start))
            (recur time-left (next clock-state))
            (and (= (first clock-state) :on)
                 (= c control)
                 (= v :stop))
            (do
              (recur time-left (next clock-state)))
            (and (= c control)
                 (= c :end))
            (.log js/console "time at end: " time-left)
            :else
            (recur time-left clock-state))))))))

(def app-state
  (atom {:white-clock {:time {:min 2 :sec 30}
                       :tag :white}
         :black-clock {:time {:min 2 :sec 30}
                       :tag :black}}))

#_(defn switch-clock [msg]
  (let [wc (:control (:white-clock @app-state))
        bc (:control (:black-clock @app-state))]
    (.log js/console msg)
    (go
     (cond
      (= msg :white)
      (do
        (>! wc :stop)
        (>! bc :start))
      (= msg :black)
      (do
        (>! wc :start)
        (>! bc :stop))
      (= msg :end)
      (do
        (>! wc :end)
        (>! bc :end))))))

(defn clock-view [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [control (om/get-state owner :control)]
        (go (loop []
              (let [msg (<! control)]
                (.log js/console msg))))))
    om/IRenderState
    (render-state [this {:keys [control]}]
      (let [tag (:tag app)]
        (dom/div #js {:className "clock"}
                 (dom/h2 nil (time->string (:time app)))
                 (dom/button #js {:onClick
                                  (fn [e] (put! control tag))} "Move"))))))

(defn board-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:white-control (chan)
       :black-control (chan)
       :main-control (chan)})
    om/IWillMount
    (will-mount [_]
      (let [mc (om/get-state owner :main-control)]
        (go (loop []
              (let [tag (<! mc)]
                (.log js/console tag)
                (recur))))))
    om/IRenderState
    (render-state [this {:keys [white-control black-control main-control]}]
      (dom/div nil
               (dom/h2 nil "Chess Clocks")
               (dom/button #js {:onClick
                                (fn [e] (put! main-control "main"))}
                           "End Game")
               (om/build clock-view (:white-clock app)
                         {:init-state {:control white-control}})
               (om/build clock-view (:black-clock app)
                         {:init-state {:control black-control}})))))

(om/root
  board-view
  app-state
  {:target (. js/document (getElementById "app"))})
