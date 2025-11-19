(ns quil-party.sketchbook.midcentury-ornaments
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil-party.lib.preview :refer [display-params parameter-background]]))

(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display

;; ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ Helper functions ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧ ✧⋄⋆⋅⋆⋄✧⋄⋆⋅⋆⋄✧

(defn axis-positions
  "Calculate the y positions of the axis lines between start-y and end-y"
  [start-y end-y num-axis]
  (if (pos? num-axis)
    (map #(-> (- end-y start-y)
              (/ (inc num-axis))
              (* (inc %))
              (+ start-y))
         (range num-axis))
    []))

(defn seg-amplitude
  "Determine the amplitude for a segment based on its position and parameters"
  [max-width waist-width slope is-end-segment]
  (let [base-amplitude (if is-end-segment waist-width max-width)]
    (* base-amplitude (+ 1 (* slope 0.1)))))

(defn segment-points
  "Generate points for a single segment of the curve"
  [center-x current-y next-y amplitude is-first-segment]
  (let [segment-height (- next-y current-y)]
    (if (zero? segment-height)
      []
      (map (fn [i]
             (let [t (* q/PI (/ i segment-height))  ; t from 0 to π
                   y (+ current-y i)
                   x-offset (* amplitude (q/sin t))]
               [(+ center-x x-offset) y]))
           ;; Skip first point for non-start segments
           (if is-first-segment
             (range (inc segment-height))
             (range 1 (inc segment-height)))))))

(defn segment-props
  "Determine properties for a segment based on its position"
  [current-idx total-segments]
  (let [is-end-segment (or (zero? current-idx) (= current-idx (dec total-segments)))
        is-first-segment (zero? current-idx)]
    {:is-end-segment is-end-segment
     :is-first-segment is-first-segment}))

(defn process-segs
  "Process all segments and combine their points"
  [center-x axis-positions max-width waist-width slope]
  (let [total-segments (dec (count axis-positions))]
    (loop [points (transient [])
           current-idx 0
           current-y (first axis-positions)]
      (if (>= current-idx total-segments)
        (persistent! points)
        (let [next-y (nth axis-positions (inc current-idx))
              seg-props (segment-props current-idx total-segments)
              amplitude (seg-amplitude max-width waist-width slope (:is-end-segment seg-props))
              seg-points (segment-points center-x current-y next-y amplitude (:is-first-segment seg-props))]
          (recur (reduce conj! points seg-points)
                 (inc current-idx)
                 next-y))))))

(defn mirror-points
  "Create the left half by mirroring the right half across the center-x line"
  [center-x points]
  (map (fn [[x y]] [(- (* 2 center-x) x) y]) (reverse points)))

;; °。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。°。
(defn draw-axis
  "Draw a horizontal axis line at the specified y position with length max-width"
  [center-x y max-width]
  (let [half-width (/ max-width 2)
        start-x (- center-x half-width)
        end-x (+ center-x half-width)]
    (q/line start-x y end-x y)))

(defn ornament 
  "A Midcentury Ornament drawn as a vector of [x y] points. 
   An ornament is drawn with bilateral symmetry on a vertical drawn in the center of the sketch.
   An ornament is created by generating a curve connecting a start-point on the vertical pole to an end-point on the vertical pole. 
   Between start and end, the line curves out to accomodate one or more horizontal axis lines. 
   The number of axis lines is kept in state as num-axis.
   The line should curve in again after accomodating each axis line but never get closer to the vertical pole than the value kept in state as waist-width."
  [state]
  (let [start-y (:start-y state)
        end-y (:end-y state)
        center-x (/ sketch-width 2)
        num-axis (:num-axis state)
        max-width (:max-width state)
        waist-width (:waist-width state)
        slope (:slope state)
        
        ;; Calculate the y positions of the axis lines
        axis-y-positions (axis-positions start-y end-y num-axis)
        
        ;; Generate the right half of the ornament by combining all axis positions
        ;; and processing them directly
        right-half (process-segs center-x 
                                 (concat [start-y] axis-y-positions [end-y]) 
                                 max-width 
                                 waist-width 
                                 slope)
        
        ;; Create the left half by mirroring the right half
        left-half (mirror-points center-x right-half)]
    
    ;; Combine the left and right halves
    (concat left-half right-half)))

(defn draw-ornament [state]
  (let [points (ornament state)]
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
  
  ;; Draw the ornament
  (draw-ornament state)
  
  ;; Draw the axis lines
  (let [start-y (:start-y state)
        end-y (:end-y state)
        center-x (/ sketch-width 2)
        num-axis (:num-axis state)
        max-width (:max-width state)
        axis-y-positions (axis-positions start-y end-y num-axis)]
    (doseq [y axis-y-positions]
      (draw-axis center-x y max-width))))

;; Boilerplate

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

;; Helper for key-pressed to reduce duplication
(defn update-state-with-bounds
  "Update a state value with bounds checking"
  [state key min-val max-val update-fn]
  (update state key (fn [x] 
                     (let [new-val (update-fn x)]
                       (cond
                         (and min-val (< new-val min-val)) min-val
                         (and max-val (> new-val max-val)) max-val
                         :else new-val)))))

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
      (update-state-with-bounds state :num-axis 1 nil dec)

      ;; RIGHT arrow 
      (= (:key event) :right)
      (update state :num-axis inc)

      ;; period
      (= raw-key \.)
      (update-state-with-bounds state :max-width (:waist-width state) nil inc)

      ;; comma
      (= raw-key \,)
      (update-state-with-bounds state :max-width (:waist-width state) nil dec)

      ;; A
      (= raw-key \a)
      (update-state-with-bounds state :waist-width 1 (:max-width state) inc) 

      ;; S
      (= raw-key \s)
      (update-state-with-bounds state :waist-width 1 (:max-width state) dec)

      ;; W
      (= raw-key \w)
      (update-state-with-bounds state :slope 1 nil inc)

      ;; Q
      (= raw-key \q)
      (update-state-with-bounds state :slope 1 nil dec)

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
