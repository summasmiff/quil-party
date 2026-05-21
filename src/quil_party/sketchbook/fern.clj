(ns quil-party.sketchbook.fern
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-party.lib.debug :as d]))

;; boring constants
(def sketch-width 600)
(def sketch-height 700)

;; FERN INITIAL STATE
(def frond-length (- sketch-height 20))
(def leaf-size 50)
(def max-pinna-size 20)
(def leaf-spacing 15)
(def num-leaves 50)
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions

(defn setup
  "Initialize state"
  []
  (q/frame-rate 30)
  {:leaf-size leaf-size
   :base-spacing leaf-spacing
   :num-leaves num-leaves})

;; Leaflet Drawing
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
  (let [leaf-width (/ leaf-size 4)
        hatch-spacing 3]
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

;; Fern Drawing
(defn angle-attrs
  "Angle calculation"
  [y-progress]
  ;; 90 degrees at bottom (progress 0) -> horizontal, 0 degrees at top (progress 1) -> vertical
  ;; Uses 'exponent' to define a curve to define how quickly the leaflets "falloff" towards horizontal
  ;; 1.0 = Linear (current behavior)
  ;; 0.5 = Square Root (stays flat longer)
  ;; 0.2 = Very flat (almost 90 degrees until the very tip)
  (let [exponent 0.3
        bottom-factor (q/pow (- 1 y-progress) exponent)
        angle-deg (* 90 bottom-factor)]
    ;; Apply clamping
    (max 5 (min 90 angle-deg))))

(defn scale-attrs [y-progress leaf-size base-spacing]
  (let [;; SCALE CALCULATION
        ;; We map y-progress from [0.5, 1.0] to [0.0, 1.0] and apply a power curve.
        ;; Using Math/pow with <1.0 creates a concave curve, >1.0 creates a convex curve
        sine-wave (if (<= y-progress 0.5)
                    1.0
                    (let [norm-t (- (* 2.0 y-progress) 1.0)] ;; Normalize 0.5->1.0 becomes 0.0->1.0
                      (- 1.0 (Math/pow norm-t 1))))
        scale-curve-factor 2
        scale (+ 0.1 (* scale-curve-factor sine-wave))
        actual-leaf-size (* leaf-size scale)
        spacing (* base-spacing (+ 0.75 sine-wave))]
    {:size actual-leaf-size
     :spacing spacing}))

(defn leaflet-attrs [y-progress leaf-size base-spacing]
  (let [scale-attrs (scale-attrs y-progress leaf-size base-spacing)]
    (merge scale-attrs
           {:angle (angle-attrs y-progress)})))

(defn in-bounds? [current-y end-y direction]
  (if (neg? direction)
    (> current-y end-y)
    (< current-y end-y)))

(defn should-continue-loop? [i current-y end-y direction]
  (and (< i 50)
       (in-bounds? current-y end-y direction)))

(defn compute-segment-geometry [i start-y current-y length bend leaf-size base-spacing]
  (let [dist-traveled (Math/abs (- start-y current-y))
        progress (/ dist-traveled length)
        curve-x (* bend 4 progress (- 1 progress))
        attrs (leaflet-attrs progress leaf-size base-spacing)
        leaf-radians (q/radians (:angle attrs))
        rotation (if (even? i) leaf-radians (- leaf-radians))]
    {:curve-x curve-x
     :size (:size attrs)
     :rotation rotation
     :spacing (:spacing attrs)}))

(defn should-recurse? [size depth]
  (and (> size max-pinna-size)
       (< depth 1)))

(defn get-sub-frond-args [size rotation depth curve-dir]
  ;; Calculate arguments for the next draw-frond call
  (let [next-curve-dir (if (pos? rotation) 1 -1)]
    [size
     (* size 0.06)
     (* size 0.02)
     0
     (- size)
     -1
     (inc depth)
     next-curve-dir]))

(declare draw-frond)

(defn draw-attachment [x y rotation size depth curve-dir]
  (q/with-translation [x y]
    (q/with-rotation [rotation]
      (if (should-recurse? size depth)
        (apply draw-frond (get-sub-frond-args size rotation depth curve-dir))
        (draw-leaf 0 0 size)))))

(defn draw-frond [length leaf-size base-spacing start-y end-y direction depth curve-dir]
  ;; 1. Setup Drawing Style
  (q/stroke 0)
  (q/stroke-weight (if (zero? depth) 1.5 0.8))

  (let [bendiness 0.07
        bend (* length bendiness curve-dir)
        offset (* length 0.05)]

    ;; 2. Loop
    (loop [i 0
           current-y (+ start-y (* direction offset))
           prev-x 0.0
           prev-y (float start-y)]

      (when (should-continue-loop? i current-y end-y direction)

        ;; 3. Calculate Geometry for this segment
        (let [{:keys [curve-x size rotation spacing]}
              (compute-segment-geometry i start-y current-y length bend leaf-size base-spacing)]

          ;; 4. Draw Stem Segment
          (q/line prev-x prev-y curve-x current-y)

          ;; 5. Draw Branch/Leaf
          (draw-attachment curve-x current-y rotation size depth curve-dir)

          ;; 6. Recur
          (recur (inc i)
                 (+ current-y (* direction spacing))
                 curve-x
                 current-y))))))

(defn draw-fern [state]
  (let [half-height (/ frond-length 2)
        leaf-size (:leaf-size state)
        base-spacing (:base-spacing state)]
    ;; Draw Main Fern
    (draw-frond frond-length leaf-size base-spacing half-height (- half-height) -1 0 1)))

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
      (q/with-translation [(/ sketch-width 2) (/ sketch-height 2)] (draw-fern state)))
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
