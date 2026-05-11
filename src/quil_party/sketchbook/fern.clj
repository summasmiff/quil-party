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
  (q/frame-rate 1)
  {})

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

(defn draw-fern
  []
  (q/stroke 0)
  (q/stroke-weight 1.5)

  ;; Center drawing in preview
  (q/with-translation [(/ (q/width) 2) (/ sketch-height 2)]
    (let [half-height (/ frond-length 2)
          leaf-size 40
          leaf-spacing 16
          num-leaves leaf-spacing]

      ;; Draw main frond
      (q/line 0 (- half-height) 0 half-height)

      ;; Draw top leaf
      (draw-leaf 0 (- half-height) leaf-size)

      ;; Loop to draw leaves down the sides
      (doseq [i (range num-leaves)]
        (let [;; Calculate Y: Start near the top and move down
              y (- (/ half-height 3)
                   (* i leaf-spacing))
              ;; Alternate sides: even i = Right, odd i = Left
              is-right (even? i)
              ;; Decrease angle as range increases to make angle more horizontal
              radians (- 90 (* i 3))
              leaf-angle (min (q/radians radians) (q/radians 90))
              ;; Rotation = side and angle
              rotation (if is-right leaf-angle (- leaf-angle))
              ;; Scale leaf linearly
              progress (/ (float i) (max 1 (dec num-leaves)))
              scale (- 2.0 progress)
              scaled-leaf-size (* leaf-size scale)]

          ;; Move to the spot on the stem
          (q/with-translation [0 y]
            ;; Rotate the canvas so the leaf points outward
            (q/with-rotation [rotation]
              ;; Draw the leaf at (0,0) because we already translated here
              (draw-leaf 0 0 scaled-leaf-size)
              (debug i))))))))

(defn preview
  [_]
  (draw-fern)
  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)
  (q/text "Press UP arrow to save SVG" 20 (+ sketch-height 20)))

(defn export
  [_]
  (let [name "fern"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (q/stroke 0)
      (draw-fern))
    (q/save gr)))

(defn key-pressed [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch fern
  :title "fern"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
