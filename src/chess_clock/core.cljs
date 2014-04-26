(ns chess-clock.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan >! <! alts! timeout]]))

(enable-console-print!)

(def app-state (atom {:text "Hello world!"
                      :white-time {:min 4 :sec 59}
                      :black-time {:min 5 :sec 00}}))

(defn minus-sec [time]
  (let [min (:min time)
        sec (:sec time)]
    (if (zero? sec)
      {:min (dec min) :sec 59}
      {:min min :sec (dec sec)})))

(defn counter [clock control]
  (let [out (chan)
        clock-state (cycle [:off :on])]
    (go
     (loop [time-left clock clock-state clock-state]
       (let [t (timeout 1000)
             [v c] (alts! [t control])
             (cond
              (and (= (first clock-state) :on)
                   (= c t))
              (recur (minus-sec time-left) clock-state)
              (and (= (first clock-state) :off)
                   (= c control)
                   (= v :start))
              (recur time-left (rest clock-state))
              (and (= c control)
                   (= c :end))
              (.log js/console "time at end: " time-left)
              :else (recur time-left clock-state))])))
    out))

(defn time->string [time]
  (let [min (:min time)
        sec  (subs (str (:sec time) "0") 0 2)]
    (str min ":" sec)))

(defn draw-clock [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "clock"}
               (dom/h2 nil (time->string app))))))

(defn draw-board [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
               (dom/h2 nil (:text app))
               (om/build draw-clock (:white-time app))
               (om/build draw-clock (:black-time app))))))

(om/root
  draw-board
  app-state
  {:target (. js/document (getElementById "app"))})
