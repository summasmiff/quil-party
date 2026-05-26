(ns quil-party.sketchbook.fern
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-party.lib.debug :as d]))

;; boring constants
(def sketch-width 600)
(def sketch-height 700)
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions

;; fern parameters
(def frond-length (- sketch-height 20))
(def max-pinna-size 10)
(def pinna-leaf-ratio 0.085)
(def pinna-spacing 0.025)
(def scale-curve 0.85) ;; <1.0 creates a concave curve, >1.0 creates a convex curve.

;; FERN INITIAL STATE / EDITABLE PARAMS
(def leaf-size 50)
(def leaf-spacing 15)

(defn setup
  "Initialize state"
  []
  (q/frame-rate 30)
  ;; Expose params for live editing
  {:leaf-size leaf-size
   :base-spacing leaf-spacing})

;; Leaflet Drawing
(defn draw-leaf [starting-x starting-y leaf-size]
  (let [leaf-width (/ leaf-size 2)]
    (q/no-fill)
    (q/with-translation [starting-x starting-y]
      ;; 1. Draw leaf outlines
      (q/begin-shape)
      (q/vertex 0 0)
      (q/bezier-vertex (- leaf-width) (- (/ leaf-size 2))
                       0                 (- leaf-size)
                       0                 (- leaf-size))
      (q/bezier-vertex leaf-width  (- (/ leaf-size 2))
                       0           0
                       0           0)
      (q/end-shape :close))))

;; Fern Drawing
(def curve-formulas
  {:parabola   (fn [p] (* 4 p (- 1 p)))                 ; Classic Arch (C-curve)
   :sine-arch  (fn [p] (Math/sin (* Math/PI p)))        ; Smoother, rounder Arch
   :s-curve    (fn [p] (Math/sin (* 2 Math/PI p)))      ; Standard S-curve
   :tall-s     (fn [p]
                 (let [taper 0.8 ;; Adjust this: higher = bigger difference
                       scale-factor (+ 0.5 (* taper p))]
                   (* (Math/sin (* 2 Math/PI p)) scale-factor)))
   :double-s   (fn [p] (Math/sin (* 4 Math/PI p)))      ; two S-shapes
   :asymmetric-s-smooth                                 ; S-curve where top and bottom curve are adjustable
   (fn [p]
     (let [breakpoint 0.7 ;; 70% the length of the stem
           k (/ (Math/log 0.5) (Math/log breakpoint))]
       (Math/sin (* 2 Math/PI (Math/pow p k)))))})

(defn angle-attrs
  "Angle calculation: Find attachment angle of leaflet or subfrond to stem.
  90 degrees -> horizontal, 0 degrees  -> vertical.\n
  'exponent' defines a curve to define how quickly the leaflets falloff towards horizontal:\n
  1.0 = Linear\n
  0.5 = Square Root (stays flat longer)\n
  0.2 = Very flat (almost 90 degrees until the very tip)"
  [y-progress]
  (let [exponent 0.3
        bottom-factor (q/pow (- 1 y-progress) exponent)
        angle-deg (* 90 bottom-factor)]
    ;; Apply clamping
    (max 5 (min 90 angle-deg))))

(defn scale-attrs
  "Scale calculation: How big should the leaflet or subfrond be.
   Tapers the top end of frond or sub-frond on a curve.
   `curve-factor` <1.0 creates a concave curve, >1.0 creates a convex curve."
  [y-progress leaf-size base-spacing]
  (let [curve-factor scale-curve
        sine-wave (if (<= y-progress 0.5)
                    1.0
                    (let [norm-t (- (* 2.0 y-progress) 1.0)] ;; Normalize 0.5->1.0 becomes 0.0->1.0
                      (- 1.0 (Math/pow norm-t curve-factor))))
        scale-curve-factor 2
        scale (+ 0.1 (* scale-curve-factor sine-wave))
        actual-leaf-size (* leaf-size scale)
        spacing (* base-spacing (+ 0.5 sine-wave))]
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

(defn compute-segment-geometry [i start-y current-y length bend leaf-size base-spacing depth]
  (let [dist-traveled (Math/abs (- start-y current-y))
        progress (/ dist-traveled length)
        curve-fn (if (= depth 0) (get curve-formulas :asymmetric-s-smooth) (get curve-formulas :sine-arch))
        curve-x (* bend (curve-fn progress))
        attrs (leaflet-attrs progress leaf-size (* 2 base-spacing))
        leaf-radians (q/radians (:angle attrs))
        rotation (if (even? i) leaf-radians (- leaf-radians))]
    {:curve-x curve-x
     :size (:size attrs)
     :rotation rotation
     :spacing (:spacing attrs)}))

(defn should-recurse? [size depth]
  (and (> size max-pinna-size)
       (< depth 1)))

(declare draw-frond)

(defn draw-attachment [x y rotation size depth]
  (q/with-translation [x y]
    (q/with-rotation [rotation]
      (if (should-recurse? size depth)
        (let [next-curve-dir (if (pos? rotation) 1 -1)]
          (draw-frond size ;; length
                      (* size pinna-leaf-ratio) ;; leaf-size
                      (* size pinna-spacing) ;; base-spacing
                      0
                      (- size)
                      -1
                      (inc depth)
                      next-curve-dir))
        (draw-leaf 0 0 size)))))

(defn draw-frond [length leaf-size base-spacing start-y end-y direction depth curve-dir]
  (q/stroke 0)
  (q/stroke-weight (if (zero? depth) 1.5 0.8))
  (let [;; Stem bendiness
        bendiness 0.05
        bend (* length bendiness curve-dir)
        offset (* length 0.09)
        ;; Dynamic leaf sizing + spacing
        max-leaves-by-spacing (int (/ length base-spacing))
        min-pixels-per-leaf 2.0 ;; Minimum leaf size
        max-leaves-by-size (int (/ length min-pixels-per-leaf))
        effective-num-leaves (max 2 (min max-leaves-by-spacing max-leaves-by-size))
        dynamic-spacing (/ length (max 1 (dec effective-num-leaves)))]

    (loop [i 0
           current-y (+ start-y (* direction offset))
           prev-x 0.0
           prev-y (float start-y)]
      (when (and (< i effective-num-leaves)
                 (in-bounds? current-y end-y direction))
        (let [{:keys [curve-x size rotation]}
              (compute-segment-geometry i start-y current-y length bend leaf-size dynamic-spacing depth)]

          ;; Draw Stem Segment
          (q/line prev-x prev-y curve-x current-y)

          ;; Draw Leaf or Subfrond
          (draw-attachment curve-x current-y rotation size depth)

          ;; Next
          (recur (inc i)
                 (+ current-y (* direction dynamic-spacing))
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
  (when-let [filename (:last-saved state)]
    (q/fill 0 150 0) ;; Make the text green
    (q/text (str "Saved SVG as: " filename) 20 (+ sketch-height 40)))

  (q/fill 0)
  (q/text (str "Leaf Size: " (:leaf-size state) " [ / ]") 300 (+ sketch-height 20))
  (q/text (str "Spacing: "   (:base-spacing state) " - / =") 300 (+ sketch-height 40))
  (q/text (str "Count: "     (:num-leaves state) " , / .") 500 (+ sketch-height 20)))

(defn export
  [state]
  (let [name "fern"
        frame-num (q/frame-count)
        filename (str "svg/" name "-" frame-num ".svg")
        ;; The :svg argument handles the file creation automatically
        gr (q/create-graphics sketch-width sketch-height :svg filename)]
    (q/with-graphics gr
      (q/with-translation [(/ sketch-width 2) (/ sketch-height 2)] (draw-fern state)))
    (assoc state :last-saved filename)))

(defn key-pressed [state event]
  (let [k (:key event)]
    (cond
      ;; Export
      (= k :up) (export state)

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
