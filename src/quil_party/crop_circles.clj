(ns quil-party.crop-circles
  "inspired by John Lundberg's works"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; "the fun zone"
(def resolution 100) ;; how many points the circles/spirals are made of
(def outer-radius 240) ;; radius of outer container circle (not rendered)
(def num-arcs 6) ;; number of subdivisions for the outer container circle
(def num-circles 13) ;; circles per arc
(def min-radius 12)
(def max-radius 36)

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

(defn- circles-overlap?
  "Check if two circles are exactly the same (same center and radius)"
  [x1 y1 r1 x2 y2 r2]
  (and (= x1 x2)
       (= y1 y2)
       (= r1 r2)))

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
  [center-point radius]
  (q/begin-shape)
  (dotimes [i resolution]
    (let [[x y] center-point
          angle (* 2 Math/PI (/ i resolution))
          x1 (+ x (* radius (Math/cos angle)))
          y1 (+ y (* radius (Math/sin angle)))]
      (q/vertex x1 y1)))
  (q/end-shape :close))

(defn draw-spiral
  "draws a tight space-filling spiral suitable for a pen plotter to use as a filled circle"
  [center-point radius & {:keys [stroke-width] :or {stroke-width 3}}]
  (let [[cx cy] center-point
        ;; Ensure stroke-width is positive
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

(defn draw-arc-path
  "Draw an arc path around a circumcircle."
  [[cx cy] r start-angle direction steps]
  (let [angle-inc (* direction (/ (* 4 Math/PI) 3 steps))]
    (q/begin-shape)
    (dotimes [i steps]
      (let [angle (+ start-angle (* i angle-inc))
            x (+ cx (* r (Math/cos angle)))
            y (+ cy (* r (Math/sin angle)))]
        (q/vertex x y)))
    (q/end-shape)))

(defn find-tangent-pairs
  "Find the centers of a pair of circles that are tangent to an apex circle at the point where the apex circle is tangent to an arc.
   The apex circle is centered at apex-circle-center with radius max-radius.
   The pair circles have radius radius.
   The arc is centered at arc-center.
   Returns a vector of two points: [inner-center outer-center]"
  [apex-circle-center radius arc-center]
  (let [[ax ay] apex-circle-center
        [cx cy] arc-center
        ;; Calculate the direction from the arc center to the apex circle center
        dx (- ax cx)
        dy (- ay cy)
        ;; Normalize this direction
        length (Math/sqrt (+ (* dx dx) (* dy dy)))
        ndx (/ dx length)
        ndy (/ dy length)
        ;; Calculate the distance between the centers of the apex circle and the pair circles
        distance (+ max-radius radius)
        ;; Calculate the centers of the pair circles
        inner-x (- ax (* distance ndx))
        inner-y (- ay (* distance ndy))
        outer-x (+ ax (* distance ndx))
        outer-y (+ ay (* distance ndy))]
    [[inner-x inner-y] [outer-x outer-y]]))

(defn draw-tangent-pair
  "Draw a pair of circles with their center point on the arc.
   The circles should be on the inside and the outside of the apex circle, relative to the arc.
   Each circle should have its circumference touching the apex circle's circumference at the point where the apex circle's circumference touches the arc.
   "
  [apex-circle-center radius arc-center]
  (let [[[inner-x inner-y] [outer-x outer-y]] (find-tangent-pairs apex-circle-center radius arc-center)]
    (draw-circle [inner-x inner-y] radius)
    (draw-circle [outer-x outer-y] radius)))

(defn draw-arc-circles
  "Draws 13 circles along the arc path, with the apex circle in the middle.
   The circles decrease in size as they move away from the apex circle.
   Each circle's circumference touches the next circle's circumference without overlapping."
  [[cx cy] r start-angle direction middle-point]
  (let [num-circles 13
        apex-index (quot num-circles 2)  ;; Middle index (6 for 13 circles)
        
        ;; Calculate the angle of the middle point
        middle-angle (Math/atan2 (- (second middle-point) cy) 
                                 (- (first middle-point) cx))
        
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
  [start end]
  (let [steps 600
        third-point (calculate-third-point start end)
        [cx cy] (calculate-circumcircle-center start end third-point)
        r (calculate-circumcircle-radius start end)
        start-angle (calculate-start-angle start [cx cy])
        direction (determine-arc-direction start end third-point)
        
        ;; Calculate the middle angle of the arc (1/3 of the way around the circle, since we're drawing 2/3 of the circumference)
        middle-angle (+ start-angle (* direction (* 1/3 Math/PI)))
        
        ;; Calculate the middle point of the arc
        middle-point [(+ cx (* r (Math/cos middle-angle)))
                      (+ cy (* r (Math/sin middle-angle)))]]

    ;; Draw the arc
    #_(draw-arc-path [cx cy] r start-angle direction steps)

    ;; Draw all circles along the arc, including the apex circle in the middle
    (draw-arc-circles [cx cy] r start-angle direction middle-point)))

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
  []
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)

  ;; draw center circle
  #_(draw-spiral [center-x center-y] max-radius)

  ;; create arcs
  (let [points (subdivide-circle [center-x center-y] outer-radius num-arcs)]
    (doseq [point points]
      (draw-arc [center-x center-y] point))))


;; .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-. quil boilerplate .-"-.     .-"-.     .-"-.     .-"-.     .-"-.
;;      "-.-"     "-.-"     "-.-"     "-.-"     "-.-"                            "-.-"     "-.-"     "-.-"     "-.-"    
(defn setup 
  "init state"
  []
  (q/frame-rate 30)
  {:size min-radius
   :num-points resolution})

(defn update-state 
  [state]
  {:size (+ (:size state) 1)
   :num-points (:num-points state)})

(defn draw-parameter-background 
  "Draws the background for the parameter display section"
  []
  (q/fill 255)
  (q/rect 0 sketch-height sketch-width (- preview-height sketch-height))
  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height))

(defn display-param 
  "Displays a single parameter at the specified position"
  [param-name param-value x y]
  (q/text (str (name param-name) ": " param-value) x y))

(defn display-params 
  "Displays multiple parameters starting at (x-start, y-start) with specified line height"
  [params x-start y-start line-height]
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)
  (q/text "current parameters:" x-start y-start)
  
  (doseq [[i [param-name param-value]] (map-indexed vector params)]
    (let [y-pos (+ y-start 20 (* i line-height))]
      (display-param param-name param-value x-start y-pos))))

(defn get-fun-zone-params 
  "Returns a vector of parameter name-value pairs from the 'fun zone' section"
  []
  [[:resolution resolution]
   [:outer-radius outer-radius]
   [:num-arcs num-arcs]
   [:num-circles num-circles]
   [:min-radius min-radius]
   [:max-radius max-radius]])

(defn preview
  "preview window"
  [state]
  (let [size (:size state)
        num-points (:num-points state)]
    (q/background 255)
    (draw)
    
    ;; parameter review section
    (draw-parameter-background)
    
    ;; write parameter name and current value for any defs in "the fun zone"
    (let [fun-zone-params (get-fun-zone-params)]
      (display-params fun-zone-params 20 (+ sketch-height 12) 20))))

(defn export 
  "saves svg to a file"
  [state]
  (let [name "crop-circle"
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

(q/defsketch quil-party
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :update update-state
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
