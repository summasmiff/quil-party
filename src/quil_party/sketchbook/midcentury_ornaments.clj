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

(defn generate-hemispherical-curve
  "Generate points along a more rounded, nearly hemispherical curve"
  [center-x start-y end-y axis-y-positions max-width waist-width slope]
  (let [axis-positions (concat [start-y] axis-y-positions [end-y])
        total-segments (dec (count axis-positions))]
    (loop [points (transient [])
           current-idx 0
           current-y start-y]
      (if (>= current-idx total-segments)
        (persistent! points)
        (let [next-y (nth axis-positions (inc current-idx))
              segment-height (- next-y current-y)]
          (if (zero? segment-height)
            (recur points (inc current-idx) next-y)
            (let [;; Determine amplitude based on segment position
                  amplitude (if (or (= current-y start-y) (= next-y end-y))
                              waist-width
                              max-width)
                  adjusted-amplitude (* amplitude (+ 1 (* slope 0.1)))
                  ;; Generate points for this segment
                  segment-points (map (fn [i]
                                        (let [t (* q/PI (/ i segment-height))  ; t from 0 to π
                                              y (+ current-y i)
                                              x-offset (* adjusted-amplitude (q/sin t))]
                                          [(+ center-x x-offset) y]))
                                      ;; Skip first point for non-start segments
                                      (if (zero? current-idx)
                                        (range (inc segment-height))
                                        (range 1 (inc segment-height))))]
              (recur (reduce conj! points segment-points)
                     (inc current-idx)
                     next-y))))))))

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
  (let [start-y (:start-y state)
        end-y (:end-y state)
        center-x (/ sketch-width 2)
        num-axis (:num-axis state)
        max-width (:max-width state)
        waist-width (:waist-width state)
        slope (:slope state)
        
        ;; Calculate the y positions of the axis lines
        axis-y-positions (calculate-axis-positions start-y end-y num-axis)
        
        ;; Generate the right half of the ornament
        right-half (generate-hemispherical-curve center-x start-y end-y axis-y-positions max-width waist-width slope)
        
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
  {:start-y 100
   :num-axis 2
   :end-y 500
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
