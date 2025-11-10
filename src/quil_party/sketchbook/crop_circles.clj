(ns quil-party.sketchbook.crop-circles
  "inspired by John Lundberg's works"
  (:require
   [clojure.math :as math]
   [quil-party.lib.preview :refer [display-params]]
   [quil.core :as q]
   [quil.middleware :as m]))

;; basic file parameters
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display
(def center-x 400)
(def center-y 300)

(defn vertexize
  "draw a shape using vertexes from a list of points"
  [points]
  (q/begin-shape)
  (doseq [[x y] points]
    (q/vertex x y))
  (q/end-shape :close))

(defn calculate-third-point
  "Calculate the third point of an equilateral triangle given two points."
  [[x1 y1] [x2 y2]]
  (let [sqrt (/ (Math/sqrt 3) 2)]
    [(+ (/ (+ x1 x2) 2) (* sqrt (- y1 y2)))
     (+ (/ (+ y1 y2) 2) (* sqrt (- x2 x1)))]))

(defn calculate-circumcircle-center
  "Calculate the center of the circumcircle of a triangle given its three vertices."
  [[x1 y1] [x2 y2] [x3 y3]]
  [(/ (+ x1 x2 x3) 3)
   (/ (+ y1 y2 y3) 3)])

(defn calculate-circumcircle-radius
  "Calculate the radius of the circumcircle of an equilateral triangle given two vertices."
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        d (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (/ d (Math/sqrt 3))))

(defn calculate-start-angle
  "Calculate the starting angle for the arc from the start point to the circumcircle center."
  [[x1 y1] [cx cy]]
  (Math/atan2 (- y1 cy) (- x1 cx)))

(defn determine-arc-direction
  "Determine the direction of the arc based on the position of the third point relative to the line from start to end."
  [[x1 y1] [x2 y2] [x3 y3]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        position (- (* (- x3 x1) dy) (* (- y3 y1) dx))]
    (if (> position 0) -1 1)))

(defn draw-circle
  "draws a circle shape"
  [state center-point radius]
  (let [resolution (:resolution state)]
    (q/begin-shape)
    (dotimes [i resolution]
      (let [[x y] center-point
            angle (* 2 Math/PI (/ i resolution))
            x1 (+ x (* radius (Math/cos angle)))
            y1 (+ y (* radius (Math/sin angle)))]
        (q/vertex x1 y1)))
    (q/end-shape :close)))

(defn draw-spiral
  "draws a tight space-filling spiral suitable for a pen plotter to use as a filled circle"
  [center-point radius & {:keys [stroke-width] :or {stroke-width 3}}]
  (let [[cx cy] center-point
        sw (max 0.1 stroke-width)
        ;; Calculate spiral constant to achieve desired spacing
        a (/ sw (* 2 Math/PI))
        ;; Calculate maximum angle based on radius and stroke-width
        θ-max (* 2 Math/PI (/ radius sw))
        ;; Determine number of points based on radius and stroke-width
        n (if (zero? radius)
            1
            (max 10 (int (Math/ceil (* 100 (/ radius sw))))))
        ;; Calculate angular step
        dθ (if (<= n 1) 0 (/ θ-max (dec n)))]
    (q/begin-shape)
    (dotimes [i n]
      (let [θ (* i dθ)
            r (* a θ)
            x (+ cx (* r (Math/cos θ)))
            y (+ cy (* r (Math/sin θ)))]
        (q/vertex x y)))
    (q/end-shape)))

(defn draw-arc-circles
  "Draws 13 circles along the arc path, with the apex circle in the middle.
   The circles decrease in size as they move away from the apex circle.
   Each circle's circumference touches the next circle's circumference without overlapping."
  [state [cx cy] r start-angle direction]
  (let [num-circles (:num-circles state)
        min-radius (:min-radius state)
        max-radius (:max-radius state)
        apex-index (quot num-circles 2)  ;; Middle index (6 for 13 circles)

        ;; Calculate the radius step between circles
        radius-step (/ (- max-radius min-radius) apex-index)

        ;; Calculate the radii for all circles
        radii (vec (for [i (range num-circles)]
                     (let [distance-from-apex (Math/abs (- i apex-index))]
                       (max min-radius (- max-radius (* distance-from-apex radius-step))))))

        ;; Ensure the apex circle has exactly max-radius
        radii (assoc radii apex-index max-radius)

        ;; Calculate the angles for all circles
        angles (loop [i 0
                      current-angle start-angle
                      angles [start-angle]]
                 (if (>= i (dec num-circles))
                   angles
                   (let [;; Calculate the distance between centers of current and next circle
                         distance (+ (radii i) (radii (inc i)))

                         ;; Calculate the angle step based on the arc length and arc radius
                         angle-step (* direction (/ distance r))

                         ;; Calculate the next angle
                         next-angle (+ current-angle angle-step)]
                     (recur (inc i) next-angle (conj angles next-angle)))))]

    ;; Draw all circles
    (doseq [i (range num-circles)]
      (let [angle (angles i)
            radius (radii i)
            x (+ cx (* r (Math/cos angle)))
            y (+ cy (* r (Math/sin angle)))]
        (draw-spiral [x y] radius)))))

(defn draw-arc
  "Draws an arc from start to end that describes 2/3rds the circumference of a circle formed around an equilateral triangle formed of the start, end, and a third point equidistant to both"
  [state start end]
  (let [third-point (calculate-third-point start end)
        [cx cy] (calculate-circumcircle-center start end third-point)
        r (calculate-circumcircle-radius start end)
        start-angle (calculate-start-angle start [cx cy])
        direction (determine-arc-direction start end third-point)]

    (draw-arc-circles state [cx cy] r start-angle direction)))

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

(defn draw
  "main drawing function"
  [state]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)

  ;; create arcs
  (let [outer-radius (:outer-radius state)
        num-arcs (:num-arcs state)
        points (subdivide-circle [center-x center-y] outer-radius num-arcs)]
    (doseq [point points]
      (draw-arc state [center-x center-y] point))))


;; .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-. quil boilerplate .-"-.     .-"-.     .-"-.     .-"-.     .-"-.
;;      "-.-"     "-.-"     "-.-"     "-.-"     "-.-"                            "-.-"     "-.-"     "-.-"     "-.-"    

(defn setup
  "init state"
  []
  (q/frame-rate 30)
  {:resolution 100
   :outer-radius 120
   :num-arcs 9
   :num-circles 13
   :min-radius 4
   :max-radius 16})

(defn draw-parameter-background
  "Draws the background for the parameter display section"
  []
  (q/fill 255)
  (q/rect 0 sketch-height sketch-width (- preview-height sketch-height))
  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height))

(defn get-fun-zone-params
  "Returns a vector of parameter name-value pairs from the 'fun zone' section"
  [state]
  #_[[:resolution (:resolution state)]
     [:outer-radius (:outer-radius state)]
     [:num-arcs (:num-arcs state)]
     [:num-circles (:num-circles state)]
     [:min-radius (:min-radius state)]
     [:max-radius (:max-radius state)]]
  [[:num-arcs (:num-arcs state)]
   [:max-radius (:max-radius state)]
   [:outer-radius (:outer-radius state)]])

(defn preview
  "preview window"
  [state]
  (q/background 255)
  (draw state)

  ;; parameter review section
  (draw-parameter-background)

  ;; write parameter name and current value for any defs in "the fun zone"
  (let [fun-zone-params (get-fun-zone-params state)]
    (display-params fun-zone-params 20 (+ sketch-height 12) 20)))

(defn export
  "saves svg to a file"
  [state]
  (let [name "crop-circle"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (draw state))
    (q/save gr)))

(defn key-pressed
  "Handle key press events"
  [state event]
  (let [raw-key (:raw-key event)]
    (cond
      ;; save svg: UP arrow
      (= (:key event) :up)
      (do
        (export state)
        state)

      ;; decrease num-arcs: LEFT arrow
      (= (:key event) :left)
      (update state :num-arcs dec)

      ;; increase num-arcs: RIGHT arrow 
      (= (:key event) :right)
      (update state :num-arcs inc)

      ;; increase biggest spiral size: period
      (= raw-key \.)
      (update state :max-radius inc)

      ;; decrease: biggest spiral: comma
      (= raw-key \,)
      (update state :max-radius dec)

      ;; decrease overall size: A
      (= raw-key \a)
      (update state :outer-radius inc) 

      ;; decrease overall size: S
      (= raw-key \s)
      (update state :outer-radius inc)

      :else
      state)))

(q/defsketch quil-party
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
