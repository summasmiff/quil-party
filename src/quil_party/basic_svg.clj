(ns quil-party.basic-svg
  "Sketch suitable for AxiDraw pen plotter"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display
(def center-x 400)
(def center-y 300)

(defn setup 
  "init state"
  []
  (q/frame-rate 10)
  {:size 10})

(defn update-state 
  [state]
  {:size (+ (:size state) 2)})

(defn draw-vectors 
  "all vectors should have (q/stroke-weight 1.5) (q/stroke 0) and (q/fill nil) for pen plotter"
  [size]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)
  (q/ellipse center-x center-y size size))

(defn preview
  "preview window"
  [state]
  (let [size (:size state)]
    (q/background 255)
    (draw-vectors size)

    (q/stroke 200)
    (q/line 0 sketch-height sketch-width sketch-height)
    (q/stroke 0)
    (q/fill 0)
    (q/text-size 14)
    (q/text "current state:" 20 (+ sketch-height 12))
    (q/text (str "size: " size) 20 (+ sketch-height 30))))

(defn export 
  "svg export"
  [state]
  (let [name "circle"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)
        size (:size state)]
    (q/with-graphics gr
      (draw-vectors size))
    (q/save gr)))

(defn key-pressed [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch quil-party
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :update update-state
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
