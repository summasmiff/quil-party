(ns quil-party.sketchbook.fern
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-party.lib.debug :as d]))

;; boring constants
(def sketch-width 600)
(def sketch-height 700)
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions

;; fern parameters
(def frond-length (- sketch-height 100))
(def max-pinna-size 7)
(def pinna-leaf-ratio 0.125)
(def pinna-spacing 0.05)
(def frond-spacing 0.5)
(def leaflet-spacing 0.5)
(def scale-curve 0.8) ;; <1.0 creates a concave curve, >1.0 creates a convex curve.

;; FERN INITIAL STATE / EDITABLE PARAMS
(def leaf-size 25)
(def leaf-spacing 11)

(defn setup
  "Initialize state"
  []
  (q/frame-rate 30)
  ;; Expose params for live editing
  {:leaf-size leaf-size
   :base-spacing leaf-spacing
   :stem-curve :smooth-s-flipped})

(defn get-spacing-ratio [depth]
  (if (zero? depth)
    frond-spacing
    leaflet-spacing))

;; Leaflet Drawing
(defn draw-leaf [starting-x starting-y leaf-size]
  (let [leaf-width (* leaf-size 0.5)  ;; 0.8 for a wider shape
        top-y      (- leaf-size)
        belly-y    (- (* leaf-size 0.35)) ;; Widest point down to 35% of the height
        adjusted-y (- top-y 5)]
    (q/no-fill)
    (q/with-translation [starting-x starting-y]
      (q/begin-shape)

      ;; 1. Pointed bottom
      (q/vertex 0 0)

      ;; 2. Left side curve going up
      (q/bezier-vertex (- leaf-width)       belly-y
                       (- (/ leaf-width 2)) adjusted-y
                       0                    top-y)

      ;; 3. Right side curve going down
      (q/bezier-vertex (/ leaf-width 2)     adjusted-y
                       leaf-width           belly-y
                       0                    0)

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
   :double-s   (fn [p] (Math/sin (* 2 Math/PI p)))      ; two S-shapes
   :asymmetric-s-smooth                                 ; S-curve where top and bottom curve are adjustable
   (fn [p]
     (let [breakpoint 0.7 ;; 70% the length of the stem
           k (/ (Math/log 0.5) (Math/log breakpoint))]
       (Math/sin (* 2 Math/PI (Math/pow p k)))))
   :smooth-s-flipped
   (fn [p]
     (let [breakpoint 0.5
           k (/ (Math/log 0.5) (Math/log breakpoint))
           mirrored-p (- 1.0 p)]
       (Math/sin (* 2.0 Math/PI (Math/pow mirrored-p k)))))})

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

(defn smooth-envelope
  "Creates a bell-shaped envelope peaking at `peak-pos` (0-1).
   Lower power = more gradual, higher = more abrupt."
  [t peak-pos rise-power fall-power]
  (cond
    (<= t 0.0) 0.0
    (>= t 1.0) 0.0
    (zero? peak-pos) 0.0
    :else (let [rise (Math/pow (min 1.0 (/ t peak-pos)) rise-power)
                fall (Math/pow (max 0.0 (/ (- 1.0 t) (- 1.0 peak-pos))) fall-power)]
            (min 1.0 (* rise fall)))))

(defn scale-attrs
  "Size calculation using smooth bell curve.
   Peak at ~35% from base (like real ferns).
   `scale-curve` controls tip taper abruptness."
  [y-progress leaf-size]
  (let [peak-position 0.35     ;; Peak closer to base
        rise-power 0.5         ;; Gradual rise from base
        fall-power scale-curve ;; Tip taper (global param)
        envelope (smooth-envelope y-progress peak-position rise-power fall-power)
        ;; Map envelope 0→1 to scale 0.08→2.0
        scale (+ 0.08 (* 1.92 envelope))
        actual-leaf-size (* leaf-size scale)]
    {:size actual-leaf-size
     :envelope envelope}))

(defn angle-aware-spacing
  "Spacing that accounts for leaf angle.
   Horizontal leaves (low angle) need more space to avoid overlap."
  [size angle-deg spacing-ratio]
  (let [;; Vertical leaves (90°) can be tight; horizontal (0°) need room
        angle-factor (+ 0.4 (* 0.6 (q/sin (q/radians angle-deg))))
        computed (* size spacing-ratio angle-factor)
        min-spacing 2.0]
    (max min-spacing computed)))

(defn leaflet-attrs [y-progress leaf-size sr]
  (let [{:keys [size envelope]} (scale-attrs y-progress leaf-size)
        angle-deg (angle-attrs y-progress)
        spacing (angle-aware-spacing size angle-deg sr)]
    {:size size
     :angle angle-deg
     :spacing spacing
     :envelope envelope}))

(defn in-bounds? [current-y end-y direction]
  (if (neg? direction)
    (> current-y end-y)
    (< current-y end-y)))

(defn compute-segment-geometry [i start-y current-y length bend leaf-size depth sr state]
  (let [dist-traveled (Math/abs (- start-y current-y))
        progress (/ dist-traveled length)
        curve-fn (if (= depth 0)
                   (get curve-formulas (:stem-curve state))
                   (get curve-formulas :sine-arch))
        curve-x (* bend (curve-fn progress))
        attrs (leaflet-attrs progress leaf-size sr)
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

(defn draw-attachment [x y rotation size depth state]
  (q/with-translation [x y]
    (q/with-rotation [rotation]
      (if (should-recurse? size depth)
        (let [next-curve-dir (if (pos? rotation) 1 -1)
              sub-sr (get-spacing-ratio (inc depth))]
          (draw-frond size ;; length
                      (* size pinna-leaf-ratio) ;; leaf-size
                      (* size pinna-spacing) ;; base-spacing
                      0
                      (- size)
                      -1
                      (inc depth)
                      next-curve-dir
                      sub-sr
                      state))
        (draw-leaf 0 0 size)))))

(defn draw-frond [length leaf-size base-spacing start-y end-y direction depth curve-dir sr state]
  (q/stroke 0)
  (q/stroke-weight 1)
  (let [;; Stem bendiness
        bendiness 0.05
        bend (* length bendiness curve-dir)
        offset (* length 0.09)

        ;; this calculation acts as a hard upper-limit for the loop counter 'i' to prevent infinite loops
        max-leaves-by-spacing (int (/ length base-spacing))
        min-pixels-per-leaf 3.0
        max-leaves-by-size (int (/ length min-pixels-per-leaf))
        effective-num-leaves (max 2 (min max-leaves-by-spacing max-leaves-by-size))
        min-local-spacing 2]

    (loop [i 0
           current-y (+ start-y (* direction offset))
           prev-x 0.0
           prev-y (float start-y)]
      (when (and (< i effective-num-leaves)
                 (in-bounds? current-y end-y direction))
        (let [{:keys [curve-x size rotation spacing]}
              (compute-segment-geometry i start-y current-y length bend leaf-size depth sr state)
              local-spacing (max min-local-spacing spacing)]

          ;; Draw Stem Segment
          (q/line prev-x prev-y curve-x current-y)
          ;; Draw Leaf or Subfrond
          (draw-attachment curve-x current-y rotation size depth state)
          ;; Next
          (recur (inc i)
                 (+ current-y (* direction local-spacing))
                 curve-x
                 current-y))))))

(defn draw-fern [state]
  (let [;; 40px up from bottom of screen
        ;; In translated coords (origin at sketch center), bottom = sketch-height/2
        emergence-y (- (/ sketch-height 2) 40)

        leaf-size (:leaf-size state)
        base-spacing (:base-spacing state)

        ;; Define 4 fronds as named maps
        fronds [{:name         :leftmost
                 :length-ratio 0.56
                 :rotation-deg -20
                 :x-offset     -60
                 :curve        :asymmetric-s-smooth}

                {:name         :left-center
                 :length-ratio 0.85
                 :rotation-deg -3
                 :x-offset     -22
                 :curve        :asymmetric-s-smooth}

                {:name         :right-center
                 :length-ratio 1.0
                 :rotation-deg 9
                 :x-offset     45
                 :curve        :smooth-s-flipped}

                {:name         :rightmost
                 :length-ratio 0.48
                 :rotation-deg 25
                 :x-offset     60
                 :curve        :smooth-s-flipped}]]

    (doseq [{:keys [length-ratio rotation-deg x-offset curve]} fronds]
      (let [frond-len (* frond-length length-ratio)
            ;; Inject the specific curve into state for this frond
            local-state (assoc state :stem-curve curve)]

        (q/with-translation [x-offset emergence-y]
          (q/with-rotation [(q/radians rotation-deg)]
            (draw-frond frond-len leaf-size base-spacing 0 (- frond-len) -1 0 1 (get-spacing-ratio 0) local-state)))))))

(defn preview
  [state]
  (q/background 255 255 255) ;; white bg
  (q/with-translation [(/ sketch-width 2) (/ sketch-height 2)] (draw-fern state))

  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)

  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)

  (q/text "Press UP to save SVG" 20 (+ sketch-height 20))
  (when-let [filename (:last-saved state)]
    (q/fill 0 150 0) ;; Make the text green
    (q/text (str "Saved SVG as: " filename) 20 (+ sketch-height 40)))

  (q/fill 0)
  (q/text (str "Leaf Size: "  (:leaf-size state) " [ / ]") 300 (+ sketch-height 20))
  (q/text (str "Spacing: "    (:base-spacing state) " - / =") 300 (+ sketch-height 40))
  (q/text (str "Stem Curve: " (:stem-curve state)) 400 (+ sketch-height 20)))

(defn export
  [state]
  (let [name "fern"
        frame-num (q/frame-count)
        filename (str "svg/" name "-" frame-num ".svg")
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
