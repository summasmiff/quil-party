(ns quil-party.fun-mode-sketch
  "Here is a sketch that uses fun-mode.
   The idea of the sketch is to draw a circle which follows the mouse. 
   It expands on every frame and shrinks when mouse is moved. 
   All functions except draw are pure functions and draw only draws (yay!).
   copied from quil fun-mode documentation"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; the fun zone
(def min-r 10)

(defn setup []
  ; initial state
  {:x 0 :y 0 :r min-r})

(defn update-state [state]
  ; increase radius of the circle by 1 on each frame
  (update-in state [:r] inc))

(defn draw [state]
  (q/background 255)
  (q/ellipse (:x state) (:y state) (:r state) (:r state)))

; decrease radius by 1 but keeping it not less than min-r
(defn shrink [r]
  (max min-r (dec r)))

(defn mouse-moved [state event]
  (-> state
      ; set circle position to mouse position
      (assoc :x (:x event) :y (:y event))
      ; decrease radius
      (update-in [:r] shrink)))

(q/defsketch example
  :size [200 200]
  :setup setup
  :draw draw
  :update update-state
  :mouse-moved mouse-moved
  :middleware [m/fun-mode]
  :features [:keep-on-top])
