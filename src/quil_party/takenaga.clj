(ns quil-party.takenaga
  "recreation of Barbara Takenaga's Ozma(2009)"
  (:require [quil.core :as q]))

;; the fun zone
(def ring-thickness 40)           ;; init size for ring diameter
(def ring-layers [2 4 6 8 10 12]) ;; ring layering: should be strictly increasing but play with the exact ints
(def num-rays 5)                 ;; number of rays exploding from center
(def amplitude 10)
(def curl-factor 1)               ;; depth of each individual sine wave
(def max-circle-radius 4)         ;; maximum size of space filling circle (value used for middlest ring & scaled for external / internal)

;; file parameters
(def file-name "takenaga")
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))
(def center-x 400)
(def center-y 300)
(def num-points 36) ;; for circle rendering

(defn setup
  "init"
  []
  (println "setting up...")
  (q/no-loop))

(defn draw-circle 
  ;; (for preview if needed)
  [size num-points]
  (q/begin-shape)
  (dotimes [i num-points]
    (let [angle (* 2 Math/PI (/ i num-points))
          x (+ center-x (* size (Math/cos angle)))
          y (+ center-y (* size (Math/sin angle)))]
      (q/vertex x y)))
  (q/end-shape :close))

(defn draw-rays
  ;; (for preview if needed)
  [num-rays]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (let [angle-step (/ (* 2 Math/PI) num-rays)
        max-dist (Math/sqrt (+ (* sketch-width sketch-width) (* sketch-height sketch-height)))]
    (dotimes [i num-rays]
      (let [angle (* i angle-step)
            end-x (+ center-x (* max-dist (Math/cos angle)))
            end-y (+ center-y (* max-dist (Math/sin angle)))]
        (q/line center-x center-y end-x end-y)))))

(defn draw-sine-waves [size num-rays]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  
  ;; Calculate the angle step between rays
  (let [angle-step (/ (* 2 Math/PI) num-rays)
        ;; Create a list of ring distances including the center
        ring-distances (cons 0 (map #(* size %) ring-layers))
        max-dist (last ring-distances)]
    
    ;; For each ray
    (dotimes [i num-rays]
      (let [angle (* i angle-step)]
        (q/begin-shape)
        
        ;; Draw points along the ray from center to max distance
        (doseq [j (range 0 max-dist 0.5)]  ;; Step by 0.5 pixels for smooth curves
          ;; Find which ring segment we're in
          (let [segment-index (loop [idx 0]
                               (if (>= idx (dec (count ring-distances)))
                                 (dec (count ring-distances))
                                 (if (<= j (nth ring-distances (inc idx)))
                                   idx
                                   (recur (inc idx)))))
                start-radius (nth ring-distances segment-index)
                end-radius (nth ring-distances (inc segment-index))
                segment-length (- end-radius start-radius)
                
                ;; Calculate position within the current segment (0 to 1)
                t (/ (- j start-radius) segment-length)
                
                ;; Calculate the sine wave offset with period matching the segment length
                wave-offset (* amplitude (Math/sin (* curl-factor 2 Math/PI t)))
                
                ;; Calculate the base position along the ray
                base-x (+ center-x (* j (Math/cos angle)))
                base-y (+ center-y (* j (Math/sin angle)))
                
                ;; Apply the offset perpendicular to the ray direction
                x (+ base-x (* wave-offset (Math/cos (+ angle (/ Math/PI 2)))))
                y (+ base-y (* wave-offset (Math/sin (+ angle (/ Math/PI 2)))))]
            
            (q/vertex x y)))
        
        (q/end-shape)))))

(defn draw
  "main vector manipulation"
  [size]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)

  ;; circles
  #_(doseq [multiplier ring-layers]
      (draw-circle (* size multiplier) num-points))
  ;; rays
  #_(draw-rays num-rays)
  ;; sine waves
  (draw-sine-waves size num-rays))

(defn preview
  "preview window"
  []
  (q/background 255)
  (draw ring-thickness)
  ;; parameter preview
  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)
  (q/text "current state:" 20 (+ sketch-height 12))
  (q/text (str "size: " ring-thickness) 20 (+ sketch-height 30))
  (q/text (str "points: " num-points) 20 (+ sketch-height 50))
  (q/text (str "rays: " num-rays) 20 (+ sketch-height 70)))

(defn export 
  "svg export"
  []
  (let [svg (str "svg/" file-name ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (println "exporting...")
    (q/with-graphics gr
      (draw ring-thickness))
    (q/save gr)))

(defn key-pressed [event]
  (println (str "key pressed: " event))
  (when (= (:key event) :up)
    (export)))

(q/defsketch quil-party
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :features [:keep-on-top])
