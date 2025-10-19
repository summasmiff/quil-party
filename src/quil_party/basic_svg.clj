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
  (q/frame-rate 30)
  {:size 10
   :num-points 36})

(defn update-state 
  [state]
  {:size (+ (:size state) 1)
   :num-points (:num-points state)})

(defn draw-circle 
  [size num-points]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)
  
  (q/begin-shape)
  (dotimes [i num-points]
    (let [angle (* 2 Math/PI (/ i num-points))
          x (+ center-x (* size (Math/cos angle)))
          y (+ center-y (* size (Math/sin angle)))]
      (q/vertex x y)))
  (q/end-shape :close))

(defn preview
  "preview window"
  [state]
  (let [size (:size state)
        num-points (:num-points state)]
    (q/background 255)
    (draw-circle size num-points)

    (q/stroke 200)
    (q/line 0 sketch-height sketch-width sketch-height)
    (q/stroke 0)
    (q/fill 0)
    (q/text-size 14)
    (q/text "current state:" 20 (+ sketch-height 12))
    (q/text (str "size: " size) 20 (+ sketch-height 30))
    (q/text (str "points: " num-points) 20 (+ sketch-height 50))))

(defn export 
  "svg export"
  [state]
  (let [name "circle"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)
        size (:size state)
        num-points (:num-points state)]
    (q/with-graphics gr
      (q/stroke-weight 1.5)
      (q/stroke 0)
      (q/fill nil)
      (draw-circle size num-points))
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
