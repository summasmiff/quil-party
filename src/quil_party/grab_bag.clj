(ns quil-party.grab-bag
 "grab bag of functions invented for other sketches but ultimately not used"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; the fun zone
(def outer-radius 240) ;; radius of outer container circle (not rendered)
(def num-arcs 6) ;; number of subdivisions for the outer container circle

;; basic file parameters
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display
(def center-x 400)
(def center-y 300)

(defn phi
  "the golden ratio"
  []
  (/ (+ 1 (Math/sqrt 5)) 2))

(defn calc-distance [point1 point2]
  (let [[x1 y1] point1
        [x2 y2] point2]
    (Math/sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))))

(defn draw-golden-spiral
  "Draws a golden spiral from start through the end point"
  [start end]
  (let [[x1 y1] start
        [x2 y2] end
        dx (- x2 x1)
        dy (- y2 y1)
        distance (calc-distance start end)
        angle (Math/atan2 dy dx)
        golden-ratio (phi)
        b (* 2 (Math/log golden-ratio) (/ Math/PI))
        ; Start with a small radius
        start-radius 1
        ; Calculate the angle needed to reach the end point
        theta-end (/ (Math/log (/ distance start-radius)) b)
        ; Calculate the starting angle
        start-angle (- angle theta-end)
        steps 600]  ; Adjust for smoother curves
    (q/begin-shape)
    (dotimes [i steps]
      (let [t (/ i steps)
            theta (* t theta-end)
            r (* start-radius (Math/exp (* b theta)))
            x (+ (first start) (* r (Math/cos (+ start-angle theta))))
            y (+ (second start) (* r (Math/sin (+ start-angle theta))))]
        (q/vertex x y)))
    (q/end-shape)))

(defn subdivide-circle
  "Returns a vector of points ([x y] vectors) representing equal subdivisions of the circumference of a circle centered at center-point"
  [center-point radius subdivisions]
  (let [[x1 y1] center-point
        angle-step (/ (* 2 Math/PI) subdivisions)]
    (vec
      (for [i (range subdivisions)]
        (let [angle (* i angle-step)
              x (+ x1 (* radius (Math/cos angle)))
              y (+ y1 (* radius (Math/sin angle)))]
          [x y])))))

;; quil sketch boilerplate

(defn setup 
  "init state"
  []
  (q/frame-rate 30)
  {:size 1
   :num-points 100})

(defn update-state 
  [state]
  {:size (+ (:size state) 1)
   :num-points (:num-points state)})

(defn draw
  "main drawing function"
  []
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)

  (let [points (subdivide-circle [center-x center-y] outer-radius num-arcs)]
    (doseq [point points]
      (draw-golden-spiral [center-x center-y] point)
      )))

(defn preview
  "preview window"
  [state]
  (let [size (:size state)
        num-points (:num-points state)]
    (q/background 255)
    (draw)
    
    ;; parameter review section
    ;; create a white rectangle as a background for text
    (q/fill 255)
    (q/rect 0 sketch-height sketch-width (- preview-height sketch-height))
    (q/stroke 200)
    (q/line 0 sketch-height sketch-width sketch-height)
    
    ;; write parameter name and current value for any defs in "the fun zone"
    (q/stroke 0)
    (q/fill 0)
    (q/text-size 14)
    (q/text "current parameters:" 20 (+ sketch-height 12))
    (q/text (str "num-arcs: " num-arcs) 20 (+ sketch-height 30))
    (q/text (str "outer-radius: " outer-radius) 20 (+ sketch-height 50))))

(defn export 
  "saves svg to a file"
  [state]
  (let [name "grab-bag"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)
        size (:size state)
        num-points (:num-points state)]
    (q/with-graphics gr
      (draw))
    (q/save gr)))

(defn key-pressed
  "trigger export by pressing up"
  [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch grab-bag
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :update update-state
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
