(ns quil-party.sketchbook.midcentury-ornaments
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-party.lib.preview :refer [display-params parameter-background]]))

(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display

;; ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ Helper functions ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧

(defn calculate-axis-positions
  "Calculate the y positions of the axis lines between start-y and end-y"
  [start-y end-y num-axis]
  (if (pos? num-axis)
    (map #(-> (- end-y start-y)
              (/ (inc num-axis))
              (* (inc %))
              (+ start-y))
         (range num-axis))
    []))

(defn generate-bezier-curve
  "Generate points along a bezier curve between start and end points with given control points"
  [start-x start-y control-x1 control-y1 control-x2 control-y2 end-x end-y num-points]
  (map (fn [t]
         (let [t (/ t (dec num-points))]
           [(q/bezier-point start-x control-x1 control-x2 end-x t)
            (q/bezier-point start-y control-y1 control-y2 end-y t)]))
       (range num-points)))

(defn generate-curve-to-axis
  "Generate a curve from current position to the next axis"
  [center-x current-y next-axis-y max-width waist-width slope]
  (let [mid-y (/ (+ current-y next-axis-y) 2)
        
        ;; First curve: from current point to the widest point
        control-y1 (+ current-y (* slope 10))
        control-y2 (- mid-y (* slope 5))
        control-x1 (+ center-x (* max-width 0.7))
        control-x2 (+ center-x max-width)
        curve1-points (generate-bezier-curve 
                        center-x current-y 
                        control-x1 control-y1 
                        control-x2 control-y2 
                        (+ center-x max-width) mid-y 
                        11)
        
        ;; Second curve: from the widest point to the axis
        control-y3 (+ mid-y (* slope 5))
        control-y4 (- next-axis-y (* slope 10))
        control-x3 (+ center-x max-width)
        control-x4 (+ center-x waist-width)
        curve2-points (generate-bezier-curve 
                        (+ center-x max-width) mid-y 
                        control-x3 control-y3 
                        control-x4 control-y4 
                        center-x next-axis-y 
                        11)]
    (concat (rest curve1-points) (rest curve2-points))))

(defn generate-curve-to-end
  "Generate a curve from current position to the end point"
  [center-x current-y end-y waist-width slope]
  (let [control-y1 (+ current-y (* slope 10))
        control-y2 (- end-y (* slope 10))
        control-x1 (+ center-x waist-width)
        control-x2 (+ center-x waist-width)]
    (generate-bezier-curve 
     center-x current-y 
     control-x1 control-y1 
     control-x2 control-y2 
     center-x end-y 
     21)))

(defn generate-right-half
  "Generate the right half of the ornament"
  [center-x start-y end-y axis-y-positions max-width waist-width slope]
  (loop [points [[center-x start-y]]  ; Start at the top of the vertical pole
         remaining-axes axis-y-positions
         current-y start-y]
    (if (empty? remaining-axes)
      ;; If no more axes, create a curve to the end point
      (let [curve-points (generate-curve-to-end center-x current-y end-y waist-width slope)]
        (concat points (rest curve-points)))
      
      ;; Otherwise, create a curve to the next axis
      (let [next-axis-y (first remaining-axes)
            curve-points (generate-curve-to-axis center-x current-y next-axis-y max-width waist-width slope)]
        (recur (concat points curve-points)
               (rest remaining-axes)
               next-axis-y)))))

(defn mirror-points
  "Create the left half by mirroring the right half across the center-x line"
  [center-x points]
  (map (fn [[x y]] [(- (* 2 center-x) x) y]) (reverse points)))

;; °。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。

(defn generate-ornament 
  "A Midcentury Ornament drawn as a vector of [x y] points. 
   An ornament is drawn with bilateral symmetry on a vertical drawn in the center of the sketch.
   An ornament is created by generating a curve connecting a start-point on the vertical pole to an end-point on the vertical pole. 
   Between start and end, the line curves out to accomodate one or more horizontal axis lines. 
   The number of axis lines is kept in state as num-axis.
   The axis should be placed with their centers on the vertical pole at more or less equilateral distance between start-point and end-point.
   The line should curve in again after accomodating each axis line but never get closer to the vertical pole than the value kept in state as waist-width.
   The curve should create a full cubic bezier between the start point and the axis before connecting to an end point. "
  [state]
  (let [start-y (second (:start-point state))
        end-y (second (:end-point state))
        center-x (/ sketch-width 2)
        num-axis (:num-axis state)
        max-width (:max-width state)
        waist-width (:waist-width state)
        slope (:slope state)
        
        ;; Calculate the y positions of the axis lines
        axis-y-positions (calculate-axis-positions start-y end-y num-axis)
        
        ;; Generate the right half of the ornament
        right-half (generate-right-half center-x start-y end-y axis-y-positions max-width waist-width slope)
        
        ;; Create the left half by mirroring the right half
        left-half (mirror-points center-x right-half)]
    
    ;; Combine the left and right halves
    (concat left-half right-half)))

(defn draw-ornament [state]
  (let [points (generate-ornament state)]
    (q/begin-shape)
    (doseq [[x y] points]
      (q/vertex x y))
    (q/end-shape :close)))

(defn draw
  "main drawing function"
  [state]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)
  (draw-ornament state))

;; ..••°°°°••....••°°°°••....••°°°°••....••°°°°••.. boilerplate ..••°°°°••....••°°°°••....••°°°°••....••°°°°••....••°°°°••..

(defn setup
  "init state"
  []
  (q/frame-rate 30)
  {:start-point [(/ sketch-width 2) 100]
   :num-axis 2
   :end-point [(/ sketch-width 2) 500]
   :max-width 36
   :waist-width 12
   :slope 4})

(defn preview
  "preview window"
  [state]
  (q/background 255)
  (draw state)

  ;; parameter review section
  (parameter-background sketch-height sketch-width preview-height)

  ;; write parameter name and current value for any defs in "the fun zone"
  (let [fun-zone-params state]
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

      ;; LEFT arrow
      (= (:key event) :left)
      (update state :num-axis (fn [x] (max 1 (dec x))))

      ;; RIGHT arrow 
      (= (:key event) :right)
      (update state :num-axis inc)

      ;; period
      (= raw-key \.)
      (update state :max-width inc)

      ;; comma
      (= raw-key \,)
      (update state :max-width (fn [x] (max (:waist-width state) (dec x))))

      ;; A
      (= raw-key \a)
      (update state :waist-width (fn [x] (min (:max-width state) (inc x)))) 

      ;; S
      (= raw-key \s)
      (update state :waist-width (fn [x] (max 1 (dec x))))

      ;; W
      (= raw-key \w)
      (update state :slope inc)

      ;; Q
      (= raw-key \q)
      (update state :slope (fn [x] (max 1 (dec x))))

      :else
      state)))

(q/defsketch ornaments
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
