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

(defn counter [app]
  (let [clock-state (cycle [:off :on])
        control (:control app)
        time-left (:time app)]
    (go
     (loop [time-left time-left clock-state clock-state]
       (do
         (let [t (timeout 1000)
               [v c] (alts! [t control])]
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
  (atom {:white-clock {:control (chan)
                       :time {:min 2 :sec 30}
                       :tag :white}
         :black-clock {:control (chan)
                       :time {:min 2 :sec 30}
                       :tag :black}}))

(defn switch-clock [msg]
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

(defn draw-clock [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:counter (counter app)})
    om/IRenderState
    (render-state [this state]
      (let [tag (:tag app)]
        (dom/div #js {:className "clock"}
                 (dom/h2 nil (time->string (:time app)))
                 (dom/button #js {:onClick
                                  (fn [e] (switch-clock tag))} "Move"))))))

(defn draw-board [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
               (dom/h2 nil (:text app))
               (dom/button #js {:onClick (fn [e] (switch-clock :end))} "End Game")
               (om/build draw-clock (:white-clock app))
               (om/build draw-clock (:black-clock app))))))

(om/root
  draw-board
  app-state
  {:target (. js/document (getElementById "app"))})
