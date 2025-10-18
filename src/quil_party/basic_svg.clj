(ns quil-party.basic-svg
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def sketch-width 800)
(def sketch-height 600)
(def center-x 400)
(def center-y 300)

(defn setup 
  "Initialize state"
  []
  (q/frame-rate 10)
  {:size 10})

(defn update-state 
  "Called for each frame"
  [state]
  {:size (+ (:size state) 2)})

(defn draw-shapes 
  "MAIN DRAWING FUNCTION: update this to generate main images"
  [size]
  (q/stroke 0)
  (q/fill nil)
  (q/ellipse center-x center-y size size))

(defn draw 
  "Function to render to preview window"
  [state]
  (let [size (:size state)]
    (q/stroke nil)
    (q/fill 255 255 255)
    (q/rect 0 0 sketch-width sketch-height)
    (draw-shapes size)))

(defn export 
  "Function to export SVG"
  [state]
  (let [name "circle"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)
        size (:size state)]
    (q/with-graphics gr
      (draw-shapes size))
    (q/save gr)))

(defn key-pressed [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch quil-party
  :title "press up arrow to save svg"
  :size [sketch-width sketch-height]
  :setup setup
  :draw draw
  :update update-state
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
