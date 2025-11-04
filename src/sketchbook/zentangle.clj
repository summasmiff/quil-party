(ns sketchbook.zentangle
  "zentangle for AxiDraw pen plotter"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; the fun zone
(def num-layers 60)         ;; Total number of layers (outer + inner)
(def base-size 120)         ;; Base size of the central star
(def segments 7)            ;; Number of segments in each star
(def base-distortion 2)     ;; Base distortion amount
(def size-step 0.04)        ;; Step size between layers (smaller = more layers)
(def rotation-step 0.02)    ;; Rotation increment per layer
(def phase-step 0.02)       ;; Phase shift increment per layer

;; boring constants
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 80))  ;; Add 80 pixels for instructions
(def center-x 400)
(def center-y 300)

(defn setup 
  "Initialize state"
  []
  (q/frame-rate 1)
  {})

;; Helper functions for coordinate calculations
(defn calculate-point
  "Calculate a point on the star with distortion"
  [base-angle size distortion phase-shift]
  (let [angle (+ base-angle phase-shift)
        distorted-size (+ size (* distortion (Math/sin (* 5 base-angle))))]
    [(+ center-x (* distorted-size (Math/cos angle)))
     (+ center-y (* distorted-size (Math/sin angle)))]))

(defn calculate-control-point
  "Calculate control point for bezier curve"
  [base-angle size distortion angle-step phase-shift]
  (let [angle (+ base-angle phase-shift)
        control-radius (* size 0.6)
        control-distortion (* distortion 0.7 (Math/cos (* 3 base-angle)))
        control-radius-distorted (+ control-radius control-distortion)
        mid-angle (+ angle (/ angle-step 2))]
    [(+ center-x (* control-radius-distorted (Math/cos mid-angle)))
     (+ center-y (* control-radius-distorted (Math/sin mid-angle)))]))

(defn draw-curved-star
  "Draw a single curved star with specified parameters"
  [size distortion rotation phase-shift]
  (q/stroke 0)
  (q/stroke-weight 1.5) 
  (q/no-fill)
  
  (let [angle-step (/ (* 2 Math/PI) segments)]
    (q/begin-shape)
    (doseq [i (range (inc segments))]
      (let [base-angle (+ rotation (* i angle-step))
            [x1 y1] (calculate-point base-angle size distortion phase-shift)
            [x2 y2] (calculate-point (+ rotation (* (inc i) angle-step)) 
                                    size distortion phase-shift)
            [cx cy] (calculate-control-point base-angle size distortion angle-step phase-shift)]
        
        ;; For the first point, just move to it
        (when (= i 0)
          (q/vertex x1 y1))
        
        ;; Draw bezier curve segment
        (q/bezier-vertex cx cy cx cy x2 y2)))
    (q/end-shape)))

(defn calculate-layer-params
  "Calculate parameters for a specific layer"
  [layer-index is-outer?]
  (if is-outer?
    {:size (* base-size (+ 1.0 (* layer-index size-step)))
     :distortion (* base-distortion (+ 1.0 (* layer-index 0.15)))
     :rotation (* layer-index rotation-step)
     :phase-shift (* layer-index phase-step)}
    {:size (* base-size (- 1.0 (* layer-index size-step)))
     :distortion (* base-distortion (+ 0.8 (* layer-index 0.1)))
     :rotation (* layer-index -1.0 rotation-step)
     :phase-shift (* layer-index -1.0 phase-step)}))

(defn draw-zentangle
  "Draw the complete zentangle pattern with nested stars"
  []
  (q/background 255)

  ;; Calculate layers
  (let [outer-layers (quot num-layers 2)
        inner-layers (- num-layers outer-layers)]

    ;; Draw outer layers (progressively larger)
    (doseq [i (range outer-layers)]
      (let [params (calculate-layer-params i true)]
        (apply draw-curved-star (vals params))))

    ;; Draw inner layers (progressively smaller)
    (doseq [i (range 1 inner-layers)]
      (let [params (calculate-layer-params i false)]
        (when (> (:size params) 10)  ;; Only draw if size is reasonable
          (apply draw-curved-star (vals params)))))))

(defn preview
  [_]
  (draw-zentangle)

  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)
  (q/text "Press UP arrow to save SVG" 20 (+ sketch-height 20)))

(defn export 
  [_]
  (let [name "curved-star-zentangle"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (draw-zentangle))
    (q/save gr)))

(defn key-pressed [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch zentangle
  :title "curved star zentangle"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
