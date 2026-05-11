(ns quil-party.sketchbook.fern
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; boring constants
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions

;; fern parameters
(def frond-length (* 2 (/ sketch-height 3))) ;; 2/3 of screen

(defn debug
  [value]
  (q/fill 255 0 0)
  (q/text-size 16)
  (q/text (str value) 5 -5))

(defn setup
  "Initialize state"
  []
  (q/frame-rate 30)
  {:leaf-size 40
   :base-spacing 9
   :num-leaves 32})

(defn draw-leaf [starting-x starting-y leaf-size]
  (let [leaf-width (/ leaf-size 4)]
    (q/no-fill)
    (q/with-translation [starting-x starting-y]
      (q/begin-shape)
      ;; Start at the stem
      (q/vertex 0 0)

      ;; Draw Left Side (From stem curving out to the tip)
      ;; Control Point 1: Pull left from tip to middle of the leaf height
      ;; Control Point 2: Arrive at the tip (centered)
      (q/bezier-vertex (- leaf-width) (- (/ leaf-size 2))
                       0                 (- leaf-size)
                       0                 (- leaf-size))

      ;; Draw Right Side (Curving back to the stem)
      ;; Control Point 1: Pull right from tip to the middle of the leaf height
      ;; Control Point 2: Arrive at the stem (centered)
      (q/bezier-vertex leaf-width  (- (/ leaf-size 2))
                       0           0
                       0           0)

      (q/end-shape :close))))

(defn draw-fern [state]
  (q/stroke 0)
  (q/stroke-weight 1.5)

  (let [half-height (/ frond-length 2)
        leaf-size (:leaf-size state)
        base-spacing (:base-spacing state)
        num-leaves (:num-leaves state)]

    ;; Center drawing in preview
    (q/with-translation [(/ (q/width) 2) (/ sketch-height 2)]

      ;; Draw main frond
      (q/line 0 (- half-height) 0 half-height)
      ;; Loop to draw leaves down the sides
      (loop [i 0
             current-y half-height]
        (when (and (< i num-leaves) (> current-y (- half-height)))
          (let [;; Alternate sides: even i = Right, odd i = Left
                is-right (even? i)
                ;; Angles
                ;; Calculate degrees first
                angle-deg (- 90 (* i 3))
                ;; Clamp it between 0 and 90
                clamped-angle (max 0 (min 90 angle-deg))
                ;; Convert to radians
                leaf-angle (q/radians clamped-angle)
                rotation (if is-right leaf-angle (- leaf-angle))
                ;; Scale calculation
                ;; Calculate how far up the stem we are (0.0 at bottom, 1.0 at top)
                ;; half-height is 200, frond-length is 400
                dist-from-bottom (- half-height current-y)
                y-progress (/ dist-from-bottom frond-length)
                sine-wave-scale (q/sin (* y-progress q/PI))
                current-scale (+ 0.1 (* 1.3 sine-wave-scale))
                scaled-leaf-size (* leaf-size current-scale)
                dynamic-spacing (* base-spacing (+ 0.75 sine-wave-scale))]

            ;; Move to the spot on the stem
            (q/with-translation [0 current-y]
              ;; Rotate the canvas so the leaf points outward
              (q/with-rotation [rotation]
                ;; Draw the leaf at (0,0) because we already translated here
                (draw-leaf 0 0 scaled-leaf-size)
                #_(debug i)))

            ;; Recurse: Move up the stem by the dynamic spacing
            ;; We subtract because y=0 is the center and y decreases going up
            (recur (inc i) (- current-y dynamic-spacing))))))))

(defn preview
  [state]
  (q/background 255 255 255) ;; white bg
  (draw-fern state)

  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)

  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)

  (q/text "Press UP arrow to save SVG" 20 (+ sketch-height 20))

  (q/text (str "Leaf Size: " (:leaf-size state) " [ / ]") 300 (+ sketch-height 20))
  (q/text (str "Spacing: "   (:base-spacing state) " - / =") 300 (+ sketch-height 40))
  (q/text (str "Count: "     (:num-leaves state) " , / .") 500 (+ sketch-height 20)))

(defn export
  [state]
  (let [name "fern"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (q/stroke 0)
      (draw-fern state))
    (q/save gr)))

(defn key-pressed [state event]
  (let [k (:key event)]
    (cond
      ;; Export
      (= k :up) (do (export state) state)

      ;; Adjust Leaf Size
      (= k (keyword "]")) (update state :leaf-size + 5)
      (= k (keyword "[")) (update state :leaf-size (fn [x] (max 5 (- x 5))))

      ;; Adjust Base Spacing
      (= k (keyword "=")) (update state :base-spacing + 1)
      (= k (keyword "-")) (update state :base-spacing (fn [x] (max 1 (- x 1))))

      ;; Adjust Number of Leaves
      (= k (keyword ".")) (update state :num-leaves + 1)
      (= k (keyword ",")) (update state :num-leaves (fn [x] (max 0 (- x 1))))

      ;; Default
      :else state)))

(q/defsketch fern
  :title "fern"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
