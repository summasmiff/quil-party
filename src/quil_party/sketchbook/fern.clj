(ns quil-party.sketchbook.fern
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; boring constants
(def sketch-width 600)
(def sketch-height 700)
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions

;; the fun zone
(def frond-length sketch-height)
(def max-pinna-size 3)
(def leaf-to-subfrond-ratio 0.2)
(def subfrond-density 0.001)
(def frond-spacing 0.47)
(def leaflet-spacing 0.25)
(def scale-curve 0.85) ;; <1.0 creates a concave curve, >1.0 creates a convex curve.
(def subfrond-length-multiplier 3.2)
(def bendiness 0.4)
(def max-angle 85) ;; 90: perpendicular to main stem
;; AVAILABLE CURVES :parabola :sine-arch :s-curve :tall-s :double-s :asymmetric-s-smooth :smooth-s-flipped
(def main-frond-curve :sine-arch)
(def stem-thickness 4)

;; FERN INITIAL STATE / EDITABLE PARAMS
(def leaf-size 25)
(def leaf-spacing 11)

(defn get-spacing-ratio [depth state]
  (if (zero? depth)
    (:frond-spacing state)
    (:leaflet-spacing state)))

;; Leaflet Drawing
(defn draw-heart-leaf [starting-x starting-y leaf-size]
  (let [leaf-width (* leaf-size 0.5)
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

(defn draw-oval-leaf [starting-x starting-y leaf-size]
  (let [leaf-width (* leaf-size 0.3)      ;; 0.8 for a wider shape
        top-y      (- leaf-size)
        belly-y    (- (* leaf-size 0.35)) ;; Widest point down to 35% of the height
        ]
    (q/no-fill)
    (q/with-translation [starting-x starting-y]
      (q/begin-shape)

      ;; 1. Pointed bottom
      (q/vertex 0 0)

      ;; 2. Left side curve going up
      (q/bezier-vertex (- leaf-width)       belly-y
                       (- (/ leaf-width 2)) top-y
                       0                    top-y)

      ;; 3. Right side curve going down
      (q/bezier-vertex (/ leaf-width 2)     top-y
                       leaf-width           belly-y
                       0                    0)

      (q/end-shape :close))))

(defn draw-blade-leaf
  "Skinny leaf with organic pointed tip and flat bottom"
  [starting-x starting-y leaf-size]
  (let [leaf-width (* leaf-size 0.2)
        top-y      (- leaf-size)
        ;; Lower the tip slightly to round it a lil
        tip-y      (- top-y (* leaf-size 0.1))
        ;; Move the top control points out to bulge a lil
        top-cp-x   (* leaf-width 0.75)]

    (q/no-fill)
    (q/with-translation [starting-x starting-y]
      (q/begin-shape)

      ;; Start at the bottom-left corner
      (q/vertex (- leaf-width) 0)

      ;; Draw the left side curve up to the tip
      (q/bezier-vertex (- leaf-width) (- (/ leaf-size 2)) ;; CP1
                       (- top-cp-x)   top-y               ;; CP2
                       0              tip-y)              ;; Vertex

      ;; Draw the right side curve back down to the bottom-right corner
      (q/bezier-vertex top-cp-x       top-y               ;; CP1
                       leaf-width     (- (/ leaf-size 2)) ;; CP2
                       leaf-width     0)                  ;; Vertex

      (q/end-shape :close))))

(defn draw-asymmetrical-leaf [_starting-x _starting-y _leaf-size]
  ;; TODO
  )

(def leaf-shapes
  {:heart draw-heart-leaf
   :oval  draw-oval-leaf
   :blade draw-blade-leaf
   :asymmetrical draw-asymmetrical-leaf})

;; Fern Drawing
(def curve-formulas
  {:parabola   (fn [p] (* 4 p (- 1 p)))                 ; Classic Arch
   :neg-parabola (fn [p]
                   (- (* 4.0 p (- 1.0 p))))             ; Backwards
   :s-curve    (fn [p] (Math/sin (* 2 Math/PI p)))      ; Standard S-curve
   :c-curve    (fn [p] (Math/sin (* Math/PI p)))
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
     (let [breakpoint 0.6 ;; 70% the length of the stem
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
  [y-progress state]
  (let [exponent 0.3
        max-angle (:max-angle state)
        bottom-factor (q/pow (- 1 y-progress) exponent)
        angle-deg (* max-angle bottom-factor)
        randomized-deg (+ angle-deg (rand 15))]
    ;; Apply clamping
    (max 5 (min max-angle randomized-deg))))

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
  [y-progress leaf-size state]
  (let [peak-position 0.25     ;; Peak closer to base
        rise-power 0.5         ;; Gradual rise from base
        fall-power (:scale-curve state) ;; Tip taper (global param)
        envelope (smooth-envelope y-progress peak-position rise-power fall-power)
        ;; Map envelope 0→1 to scale 0.08→2.0
        scale (+ 0.08 (* 2 envelope))
        actual-leaf-size (* leaf-size scale)]
    {:size actual-leaf-size
     :envelope envelope}))

(defn angle-aware-spacing
  "Spacing that accounts for leaf angle.
   Horizontal leaves (low angle) need more space to avoid overlap."
  [size angle-deg spacing-ratio _state]
  (let [;; Vertical leaves (90°) can be tight; horizontal (0°) need room
        angle-factor (+ 0.4 (* 0.6 (q/sin (q/radians angle-deg))))
        computed (* size spacing-ratio angle-factor)
        min-spacing 2.0]
    (max min-spacing computed)))

(defn leaflet-attrs [y-progress leaf-size sr state]
  (let [{:keys [size envelope]} (scale-attrs y-progress leaf-size state)
        angle-deg (angle-attrs y-progress state)
        spacing (angle-aware-spacing size angle-deg sr state)]
    {:size size
     :angle angle-deg
     :spacing spacing
     :envelope envelope}))

(defn in-bounds? [current-y end-y direction]
  (if (neg? direction)
    (> current-y end-y)
    (< current-y end-y)))

(defn compute-segment-geometry [i start-y current-y length bend leaf-size _depth sr state curve-fn]
  (let [dist-traveled (Math/abs (- start-y current-y))
        progress (/ dist-traveled length)
        curve-x (* bend (curve-fn progress))
        attrs (leaflet-attrs progress leaf-size sr state)
        leaf-radians (q/radians (:angle attrs))
        rotation (if (even? i) leaf-radians (- leaf-radians))]
    {:curve-x curve-x
     :size (:size attrs)
     :rotation rotation
     :spacing (:spacing attrs)}))

(defn should-recurse? [size depth state]
  (and (> size (:max-pinna-size state))
       (< depth 1)))

(defn draw-stem-segment [x1 y1 x2 y2 taper state]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        length (Math/sqrt (+ (* dx dx) (* dy dy)))
        ux (/ dx length)
        uy (/ dy length)
        px (- uy) ;; perpendicular
        py ux ;; perpendicular
        thickness (* (:stem-thickness state) (- 1 taper))
        offset-x (* 0.5 thickness px)
        offset-y (* 0.5 thickness py)]
    ;; Draw two parallel lines
    (q/line (+ x1 offset-x) (+ y1 offset-y) (+ x2 offset-x) (+ y2 offset-y))
    (q/line (- x1 offset-x) (- y1 offset-y) (- x2 offset-x) (- y2 offset-y))))

(declare draw-frond)

(defn draw-attachment [x y rotation size depth state]
  (q/with-translation [x y]
    (q/with-rotation [rotation]
      (if (should-recurse? size depth state)
        (let [next-curve-dir (if (pos? rotation) 1 -1)
              sub-sr (get-spacing-ratio (inc depth) state)
              subfrond-len (* size (:subfrond-length-multiplier state))]
          (draw-frond subfrond-len ;; total length
                      (* size (:leaf-to-subfrond-ratio state)) ;; leaf-size
                      (* size (:subfrond-density state)) ;; base-spacing
                      0
                      (- subfrond-len)
                      -1
                      (inc depth)
                      next-curve-dir
                      sub-sr
                      state))
        (let [leaf-fn (get leaf-shapes (:leaf-shape state) draw-oval-leaf)]
          (leaf-fn 0 0 size))))))

(defn draw-frond [length leaf-size base-spacing start-y end-y direction depth curve-dir sr state]
  (q/stroke 0)
  (q/stroke-weight 1)
  (let [bend (if (= 0 depth) (* length (:bendiness state) curve-dir) (* length 0.08 curve-dir))
        empty-stem (if (= depth 0) (* length 0.1) (* length 0.09)) ;; TODO add top level defs for this
        empty-step-size 5.0

        ;; hard upper-limit for the loop counter 'i' to prevent infinite loops
        empty-stem-steps (int (/ empty-stem empty-step-size))
        max-leaves-by-spacing (int (/ length base-spacing))
        min-pixels-per-leaf 3.0
        max-leaves-by-size (int (/ length min-pixels-per-leaf))
        effective-loops (+ empty-stem-steps (max 2 (min max-leaves-by-spacing max-leaves-by-size)))
        min-local-spacing 2
        curve-fn (if (= depth 0)
                   (get curve-formulas (:stem-curve state))
                   (get curve-formulas (rand-nth [:s-curve :parabola :tall-s :asymmetric-s-smooth])))]
    (loop [i 0
           current-y (+ start-y (* direction empty-step-size))
           prev-x 0.0
           prev-y (float start-y)]
      (when (and (< i effective-loops)
                 (in-bounds? current-y end-y direction))
        (let [dist-traveled (Math/abs (- start-y current-y))
              {:keys [curve-x size rotation spacing]}
              (compute-segment-geometry i start-y current-y length bend leaf-size depth sr state curve-fn)
              local-spacing (max min-local-spacing spacing)
              taper (/ i (double (dec effective-loops)))]

          ;; Draw Stem Segment
          (if (= depth 0) (draw-stem-segment prev-x prev-y curve-x current-y taper state)
              (q/line prev-x prev-y curve-x current-y))

          ;; Draw Leaf or Subfrond
          (when (> dist-traveled empty-stem)
            (draw-attachment curve-x current-y rotation size depth state))

          ;; Next
          (recur (inc i)
                 (+ current-y (* direction local-spacing))
                 curve-x
                 current-y))))))

(defn draw-fern [state]
  (let [;; 0 = center??
        emergence-y 0

        leaf-size (:leaf-size state)
        base-spacing (:base-spacing state)

        fronds [{:length-ratio 0.75
                 :rotation-deg 0
                 :x-offset     0
                 :curve        :neg-parabola} ;; 1
                {:length-ratio 0.75
                 :rotation-deg 40
                 :x-offset     0
                 :curve        :neg-parabola} ;; 2
                {:length-ratio 0.85
                 :rotation-deg 80
                 :x-offset     0
                 :curve        :neg-parabola} ;; 3
                {:length-ratio 0.78
                 :rotation-deg 120
                 :x-offset     0
                 :curve        :neg-parabola} ;; 4
                {:length-ratio 0.78
                 :rotation-deg 160
                 :x-offset     0
                 :curve        :neg-parabola} ;; 5
                {:length-ratio 0.78
                 :rotation-deg 200
                 :x-offset     0
                 :curve        :neg-parabola} ;; 6
                {:length-ratio 0.78
                 :rotation-deg 240
                 :x-offset     0
                 :curve        :neg-parabola} ;; 7
                {:length-ratio 0.78
                 :rotation-deg 280
                 :x-offset     0
                 :curve        :neg-parabola} ;; 8
                {:length-ratio 0.78
                 :rotation-deg -40
                 :x-offset     0
                 :curve        :neg-parabola} ;; 9
                ]]

    (doseq [{:keys [length-ratio rotation-deg x-offset curve]} fronds]
      (let [frond-len (* (:frond-length state) length-ratio)
            local-state (assoc state :stem-curve curve)]

        (q/with-translation [x-offset emergence-y]
          (q/with-rotation [(q/radians rotation-deg)]
            (draw-frond
             frond-len
             leaf-size
             base-spacing
             0
             (- frond-len)
             -1
             0
             1
             (get-spacing-ratio 0 local-state)
             local-state)))))))

(defn redraw-fern
  "Regenerate the fern graphics buffer from the current state"
  [state]
  (let [g (q/create-graphics sketch-width sketch-height)]
    (q/with-graphics g
      (q/background 255)
      (q/with-translation [(/ sketch-width 2) (/ sketch-height 2)]
        (draw-fern state)))
    (assoc state :fern-g g)))

(defn setup
  "Initialize state"
  []
  (q/frame-rate 1)
  (let [g (q/create-graphics sketch-width sketch-height)
        state {:leaf-size leaf-size
               :base-spacing leaf-spacing
               :stem-curve :smooth-s
               :leaf-shape :blade
               :frond-length frond-length
               :max-pinna-size max-pinna-size
               :leaf-to-subfrond-ratio leaf-to-subfrond-ratio
               :subfrond-density subfrond-density
               :frond-spacing frond-spacing
               :leaflet-spacing leaflet-spacing
               :scale-curve scale-curve
               :subfrond-length-multiplier subfrond-length-multiplier
               :bendiness bendiness
               :max-angle max-angle
               :main-frond-curve main-frond-curve
               :stem-thickness stem-thickness}]
    (q/with-graphics g
      (q/background 255)
      (q/with-translation [(/ sketch-width 2) (/ sketch-height 2)]
        (draw-fern state)))
    (merge state {:fern-g g})))

(defn preview
  [state]
  (q/background 255 255 255) ;; white bg
  (when-let [g (:fern-g state)]
    (q/image g 0 0))

  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)

  (q/stroke 0)
  (q/fill 0)
  (q/text-size 11)

  (q/text "UP: Export SVG  L: Cycle Leaves" 10 (+ sketch-height 15))

  ;; Row 1: Short, Medium, Medium
  (q/text (str "Leaf Size: "  (:leaf-size state) " []") 10 (+ sketch-height 30))
  (q/text (str "Max Angle: "  (:max-angle state) " az") 180 (+ sketch-height 30))
  (q/text (str "Bendiness: "  (format "%.3f" (:bendiness state)) " bv") 350 (+ sketch-height 30))

  ;; Row 2: Short, Medium, Medium
  (q/text (str "Spacing: "    (:base-spacing state) " =-") 10 (+ sketch-height 45))
  (q/text (str "Scale Curve: " (format "%.1f" (:scale-curve state)) " sx") 180 (+ sketch-height 45))
  (q/text (str "Frond Spacing: " (format "%.2f" (:frond-spacing state)) " tg") 350 (+ sketch-height 45))

  ;; Row 3: Long, Long, Medium
  (q/text (str "Subfrond Length: " (format "%.1f" (:subfrond-length-multiplier state)) " dc") 10 (+ sketch-height 60))
  (q/text (str "Leaf to Subfrond Ratio: " (format "%.2f" (:leaf-to-subfrond-ratio state)) " ew") 180 (+ sketch-height 60))
  (q/text (str "Leaflet Spacing: " (format "%.2f" (:leaflet-spacing state)) " yh") 350 (+ sketch-height 60))

  ;; Row 4: Long
  (q/text (str "Subfrond Density: " (format "%.4f" (:subfrond-density state)) " rf") 10 (+ sketch-height 75))

  (when-let [filename (:last-saved state)]
    (q/fill 0 150 0)
    (q/text-size 10)
    (q/text (str "✓ " filename) 10 (+ sketch-height 75))))

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
  (let [k (:key event)
        ;; Helper to update a numeric value with bounds checking
        inc-val (fn [v delta min-v max-v] (max min-v (min max-v (+ v delta))))]
    (cond
      ;; Export
      (= k :up) (export state)

      ;; Leaf Size
      (= k (keyword "]")) (redraw-fern (update state :leaf-size (fn [v] (inc-val v 5 5 100))))
      (= k (keyword "[")) (redraw-fern (update state :leaf-size (fn [v] (inc-val v -5 5 100))))

      ;; Base Spacing
      (= k (keyword "=")) (redraw-fern (update state :base-spacing (fn [v] (inc-val v 1 1 50))))
      (= k (keyword "-")) (redraw-fern (update state :base-spacing (fn [v] (inc-val v -1 1 50))))

      ;; Leaf Shape (l)
      (= k (keyword "l")) (let [all-leaves (keys leaf-shapes)
                                current-leaf (:leaf-shape state)
                                [_before after] (split-with #(not= % current-leaf) all-leaves)
                                new-leaf (first (rest after))]
                            (redraw-fern (assoc state :leaf-shape (or new-leaf (first all-leaves)))))

      ;; Bendiness (b/v for bend)
      (= k (keyword "b")) (redraw-fern (update state :bendiness (fn [v] (inc-val v 0.01 0.001 1.0))))
      (= k (keyword "v")) (redraw-fern (update state :bendiness (fn [v] (inc-val v -0.01 0.001 0.2))))

      ;; Max Angle (a/z)
      (= k (keyword "a")) (redraw-fern (update state :max-angle (fn [v] (inc-val v 5 5 90))))
      (= k (keyword "z")) (redraw-fern (update state :max-angle (fn [v] (inc-val v -5 5 90))))

      ;; Scale Curve (s/x)
      (= k (keyword "s")) (redraw-fern (update state :scale-curve (fn [v] (inc-val v 0.1 0.1 2.0))))
      (= k (keyword "x")) (redraw-fern (update state :scale-curve (fn [v] (inc-val v -0.1 0.1 2.0))))

      ;; Subfrond Length Multiplier (d/c)
      (= k (keyword "d")) (redraw-fern (update state :subfrond-length-multiplier (fn [v] (inc-val v 0.1 1.0 5.0))))
      (= k (keyword "c")) (redraw-fern (update state :subfrond-length-multiplier (fn [v] (inc-val v -0.1 1.0 5.0))))

      ;; Leaf to Subfrond Ratio (e/w)
      (= k (keyword "e")) (redraw-fern (update state :leaf-to-subfrond-ratio (fn [v] (inc-val v 0.05 0.05 1.0))))
      (= k (keyword "w")) (redraw-fern (update state :leaf-to-subfrond-ratio (fn [v] (inc-val v -0.05 0.05 1.0))))

      ;; Subfrond Density (r/f)
      (= k (keyword "r")) (redraw-fern (update state :subfrond-density (fn [v] (inc-val v 0.0005 0.0001 0.005))))
      (= k (keyword "f")) (redraw-fern (update state :subfrond-density (fn [v] (inc-val v -0.0005 0.0001 0.005))))

      ;; Frond Spacing (t/g)
      (= k (keyword "t")) (redraw-fern (update state :frond-spacing (fn [v] (inc-val v 0.05 0.1 1.0))))
      (= k (keyword "g")) (redraw-fern (update state :frond-spacing (fn [v] (inc-val v -0.05 0.1 1.0))))

      ;; Leaflet Spacing (y/h)
      (= k (keyword "y")) (redraw-fern (update state :leaflet-spacing (fn [v] (inc-val v 0.05 0.1 1.0))))
      (= k (keyword "h")) (redraw-fern (update state :leaflet-spacing (fn [v] (inc-val v -0.05 0.1 1.0))))

      ;; Stem Thickness (q/j)
      (= k (keyword "q")) (redraw-fern (update state :stem-thickness (fn [v] (inc-val v 1 1 20))))
      (= k (keyword "j")) (redraw-fern (update state :stem-thickness (fn [v] (inc-val v -1 1 20))))

      ;; Max Pinna Size (p/o)
      (= k (keyword "p")) (redraw-fern (update state :max-pinna-size (fn [v] (inc-val v 1 1 20))))
      (= k (keyword "o")) (redraw-fern (update state :max-pinna-size (fn [v] (inc-val v -1 1 20))))

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
