(ns sketchbook.cataract-3
  "quil sketch to replicate Bridget Riley's Cataract 3. Animated!"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def sketch-name "cataract-3")
(def sketch-width 800)
(def sketch-height 600)

(def min-size 0)
(def max-size 40)
(def oscillation-speed 0.05)

(defn setup 
  "Initialize state"
  []
  (q/frame-rate 30)
  {:phase 0.0})

(defn update-state 
  [state]
  (let [new-phase (+ (:phase state) oscillation-speed)]
    {:phase new-phase}))

(defn draw-shapes 
  "main drawing fn"
  [size]
  (q/stroke 0 0 0)
  (q/fill nil)
  
  ;; Parameters from cataract-3
  (let [width sketch-width
        height sketch-height
        num-lines 75
        line-spacing (/ height num-lines)
        base-amplitude 5
        frequency 0.055
        phase-shift 0.45
        ;; Scale amplitude based on size parameter, but cap it to prevent excessive distortion
        amplitude-scale (min 4 (/ size 10))]
    
    ;; Draw each line
    (doseq [i (range num-lines)]
      (let [y (* i line-spacing)
            amplitude (* base-amplitude 
                        amplitude-scale
                        (+ 0.8 (* 0.4 (Math/sin (* i 0.2)))))]
        
        (q/begin-shape)
        
        (doseq [x (range 0 width 2)]
          (let [offset (* amplitude 
                          (Math/sin (+ (* frequency x) 
                                       (* phase-shift i))))]
            (q/vertex x (+ y offset))))
        
        (q/end-shape)))))

(defn draw 
  "preview window"
  [state]
  (let [phase (:phase state)
        ;; Calculate oscillating size using sine wave
        size (+ min-size (* (- max-size min-size) 
                           (+ 0.5 (* 0.5 (Math/sin phase)))))]
    (q/stroke nil)
    (q/fill 255 255 255)
    (q/rect 0 0 sketch-width sketch-height)
    (draw-shapes size)))

(defn export 
  "svg export"
  [state]
  (let [frame-num (q/frame-count)
        svg (str "svg/" sketch-name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)
        phase (:phase state)
        ;; Calculate oscillating size using sine wave
        size (+ min-size (* (- max-size min-size) 
                           (+ 0.5 (* 0.5 (Math/sin phase)))))]
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
