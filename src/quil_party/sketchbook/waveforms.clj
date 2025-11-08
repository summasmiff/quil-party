(ns quil-party.sketchbook.waveforms
  "quil sketch that started as a copy of Bridget Riley's Fall (1963) and became something else"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Params to play with
(def num-lines (+ 60 (rand-int 61)))       ;; default: 100 range: (60 - 120)
(def phase-shift 0.3)                      ;; default: 0.3
(def base-frequency 0.01)                  ;; default: 0.01 range: (0.005 - 0.03) (q/random 0.005 0.03)
(def base-amplitude (+ 2 (rand-int 4)))    ;; default: 3 range: (2 - 5)
(def amplitude-scale 1.0)                  ;; default: 1.0
(def exp-grow 1.05)                        ;; default 1.05 range: (1.01 - 1.09) (q/random 1.01 1.09)
(def exp-shrink 0.995)                     ;; default 0.995 range: (0.99 - 0.999) (q/random 0.99 0.999)

;; File params
(def sketch-name "waveforms")
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display

(defn setup
  []
  (q/frame-rate 10)) ;; need to have a frame-rate for the export filename

(defn draw-shapes 
  "main drawing function"
  []
  (q/stroke 0)
  (q/fill nil)
  (q/stroke-weight 1.5)
  
  (let [width sketch-width
        height sketch-height
        num-lines num-lines
        line-spacing (/ height num-lines)
        base-amplitude base-amplitude
        base-frequency base-frequency
        phase-shift phase-shift
        amplitude-scale amplitude-scale]
    
    ;; Draw each line
    (doseq [i (range num-lines)]
      (let [y (* i line-spacing)
            compression-factor (Math/pow exp-grow i)        ;; create exponential increase towards bottom
            frequency (* base-frequency compression-factor) ;; frequency increases towards bottom
            amplitude (* base-amplitude amplitude-scale     ;; amplitude decreases toward bottom to prevent disintegration
                          (Math/pow exp-shrink i))]         
        
        (q/begin-shape)
        
        (doseq [x (range 0 width 2)]
          (let [offset (* amplitude 
                          (Math/sin (+ (* frequency x) 
                                       (* phase-shift i)    ;; Add a phase shift that creates the "falling" effect
                                       (* 0.1 i))))]
            (q/vertex x (+ y offset))))
        
        (q/end-shape)))))

(defn preview
  "preview window : shows svg plus parameters used to make it"
  [_state]
  ;; Clear the background
  (q/background 255)
  
  ;; draw draw draw
  (draw-shapes)
  
  ;; Line separating the render from the parameters
  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)
  (q/stroke 0)
  
  ;; Parameter information at the bottom
  (q/fill 0)
  (q/text-size 14)
  (q/text "Parameters to play with:" 20 (+ sketch-height 12))
  (q/text (str "num-lines: " num-lines " (randomized)") 20 (+ sketch-height 30))
  (q/text (str "phase-shift: " phase-shift) 20 (+ sketch-height 50))
  (q/text (str "base-frequency: " base-frequency " (randomized)") 20 (+ sketch-height 70))
  (q/text (str "base-amplitude: " base-amplitude " (randomized)") 20 (+ sketch-height 90))
  (q/text (str "amplitude-scale: " amplitude-scale) 20 (+ sketch-height 110))
  (q/text (str "exp-grow: " exp-grow " (randomized)") 20 (+ sketch-height 130))
  (q/text (str "exp-shrink: " exp-shrink " (randomized)") 20 (+ sketch-height 150)))

(defn export 
  "svg export"
  [_state]
  (let [frame-num (q/frame-count)
        svg (str "svg/" sketch-name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (q/stroke 0)
      (draw-shapes))
    (q/save gr)))

(defn key-pressed [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch quil-party
  :title "press up arrow to save svg"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
