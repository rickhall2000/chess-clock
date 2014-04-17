(ns chess-clock.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

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
