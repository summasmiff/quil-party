(ns quil-party.sketchbook.fern
  (:require [quil.core :as q]
            [quil.middleware :as m]))
;; boring constants
(def sketch-width 600)
(def sketch-height 700)

(def frond-length (- sketch-height 20))
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions

(defn debug
  [value]
  (q/fill 255 0 0)
  (q/text-size 16)
  (q/text (str value) 5 -5))

(defn setup
  "Initialize state"
  []
  (q/frame-rate 30)
  {:leaf-size 25
   :base-spacing 5
   :num-leaves 32})

(defn bezier-point
  "Helper for hatching"
  [t p0 p1 p2 p3]
  (let [u (- 1 t)
        tt (* t t)
        uu (* u u)]
    (+ (* uu u p0)
       (* 3 uu t p1)
       (* 3 u tt p2)
       (* tt t p3))))

(defn find-x-at-y
  "Helper for hatching"
  [target-y y0 y1 y2 y3 x0 x1 x2 x3]
  (loop [low 0.0
         high 1.0]
    (let [mid (/ (+ low high) 2.0)
          curr-y (bezier-point mid y0 y1 y2 y3)]
      (if (< (Math/abs (- curr-y target-y)) 0.1)
        (bezier-point mid x0 x1 x2 x3)
        (if (> curr-y target-y)
          (recur mid high)
          (recur low mid))))))

(defn draw-leaf [starting-x starting-y leaf-size]
  (let [leaf-width (/ leaf-size 4)]
    (q/no-fill)
    (q/with-translation [starting-x starting-y]

      ;; 1. Draw Outline
      (q/begin-shape)
      (q/vertex 0 0)
      (q/bezier-vertex (- leaf-width) (- (/ leaf-size 2))
                       0                 (- leaf-size)
                       0                 (- leaf-size))
      (q/bezier-vertex leaf-width  (- (/ leaf-size 2))
                       0           0
                       0           0)
      (q/end-shape :close)

      ;; 2. Draw Hatching
      (doseq [y (range (- leaf-size) 0 hatch-spacing)]
        (let [x-left (find-x-at-y y
                                  0 (- (/ leaf-size 2)) (- leaf-size) (- leaf-size)
                                  0 (- leaf-width) 0 0)
              x-right (- x-left)
              width (- x-right x-left)]

          ;; Only draw if the line is wider than the spacing
          (when (> width hatch-spacing)
            (q/line x-left y x-right y)))))))

(defn draw-fern [state]
  (q/stroke 0)
  (q/stroke-weight 1.5)

  (let [half-height (/ frond-length 2)
        leaf-size (:leaf-size state)
        base-spacing (:base-spacing state)
        num-leaves (:num-leaves state)]

    ;; Draw main frond
    (q/line 0 (- half-height) 0 half-height)
    ;; Loop to draw leaves down the sides
    (loop [i 0
           current-y half-height]
      (when (and (< i num-leaves) (> current-y (- half-height)))
        (let [;; Alternate sides: even i = Right, odd i = Left
              is-right (even? i)

              ;; 1. Calculate leaf position
              ;; y-progress is 0.0 at bottom, 1.0 at top
              dist-from-bottom (- half-height current-y)
              y-progress (/ dist-from-bottom frond-length)

              ;; 2. Leaf angles based on stem position
              ;; 90 degrees at bottom (progress 0) -> horizontal, 0 degrees at top (progress 1) -> vertical
              angle-deg (* 90 (- 1 y-progress))

              ;; Clamp angle between 0 and 90
              clamped-angle (max 10 (min 90 angle-deg))

              ;; Convert to radians
              leaf-angle (q/radians clamped-angle)
              rotation (if is-right leaf-angle (- leaf-angle))

              ;; 3. Leaf scale calculation
              sine-wave-scale (q/sin (* y-progress q/PI))
              current-scale (+ 0.1 (* 1.3 sine-wave-scale))
              scaled-leaf-size (* leaf-size current-scale)
              dynamic-spacing (* base-spacing (+ 0.75 sine-wave-scale))]

          ;; Move to the spot on the stem
          (q/with-translation [0 current-y]
            ;; Rotate the canvas so the leaf points outward
            (q/with-rotation [rotation]
              ;; Draw the leaf
              (draw-leaf 0 0 scaled-leaf-size)
              #_(debug i)))

          ;; Recurse
          (recur (inc i) (- current-y dynamic-spacing)))))))

(defn preview
  [state]
  (q/background 255 255 255) ;; white bg
  (q/with-translation [(/ sketch-width 2) (/ sketch-height 2)] (draw-fern state))

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
      (draw-wallpaper state))
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
