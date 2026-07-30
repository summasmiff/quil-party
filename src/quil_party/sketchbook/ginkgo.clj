(ns quil-party.sketchbook.ginkgo
  "Sketch of the branching veins of a ginkgo leaf suitable for AxiDraw pen plotter"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; basic file parameters
(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 100))
(def start-x 400)
(def start-y 450)

(def default-state
  {:initial-length 30
   :branch-ratio 0.95         ;; Changed to a 0.0 - 1.0 ratio for perfect space-filling
   :length-decay 1.44
   :max-depth 5
   :max-global-angle 1.35
   :num-initial-veins 7})

(defn setup
  "init state with ginkgo parameters"
  []
  (q/frame-rate 30)
  default-state)

(defn update-state
  "No automatic updates - all parameters controlled by keys"
  [state]
  state)

(defn draw-branch
  "Recursively draws a binary branch. Uses relative spacing so gaps match at all scales."
  [x y angle length depth state left-bound right-bound]
  (let [{:keys [branch-ratio length-decay max-depth max-global-angle]} state]
    (when (and (< depth max-depth)
               (> length 1.5)
               (<= (Math/abs angle) max-global-angle))
      (let [x2 (+ x (* length (Math/sin angle)))
            y2 (- y (* length (Math/cos angle)))]
        (q/line x y x2 y2)
        (let [new-length (* length length-decay)
              new-depth (inc depth)

              ;; Calculate the maximum angle this branch could turn without overlapping
              max-turn-left (- angle left-bound)
              max-turn-right (- right-bound angle)

              ;; Apply the ratio to turn a fraction of the available space
              desired-left (- angle (* branch-ratio max-turn-left))
              desired-right (+ angle (* branch-ratio max-turn-right))

              ;; Tiny threshold to prevent zero-width branches
              eps 0.001]

          (when (> (- angle desired-left) eps)
            (draw-branch x2 y2 desired-left new-length new-depth state left-bound angle))

          (when (> (- desired-right angle) eps)
            (draw-branch x2 y2 desired-right new-length new-depth state angle right-bound)))))))

(defn draw
  "main drawing function"
  [state]
  (let [{:keys [initial-length max-global-angle num-initial-veins]} state]
    (q/stroke-weight 1.5)
    (q/stroke 0)
    (q/no-fill)
    (if (< num-initial-veins 2)
      ;; If only 1 vein, give it the entire global fan as its territory
      (draw-branch start-x start-y 0 initial-length 0 state (- max-global-angle) max-global-angle)
      (let [step (/ (* 2 max-global-angle) (dec num-initial-veins))]
        (doseq [i (range num-initial-veins)]
          (let [angle (- max-global-angle (* i step))
                ;; Divide the fan equally. Each vein owns a sector of 'step' width.
                left-bound (max (- max-global-angle) (- angle (/ step 2)))
                right-bound (min max-global-angle (+ angle (/ step 2)))]
            (draw-branch start-x start-y angle initial-length 0 state left-bound right-bound)))))))

(defn format-val
  "Format a parameter value for display"
  [key val]
  (if (#{:max-depth :num-initial-veins} key)
    (str (int val))
    (format "%.2f" (double val))))

(defn preview
  "preview window"
  [state]
  (q/background 255)
  (draw state)

  (let [params [["Q/W" :initial-length "initial-length"]
                ["A/S" :branch-ratio "branch-ratio"]      ;; Updated label
                ["Z/X" :length-decay "length-decay"]
                ["E/R" :max-depth "max-depth"]
                ["D/F" :max-global-angle "max-global-angle"]
                ["C/V" :num-initial-veins "veins"]]]

    (q/fill 245)
    (q/no-stroke)
    (q/rect 0 sketch-height sketch-width (- preview-height sketch-height))
    (q/stroke 200)
    (q/line 0 sketch-height sketch-width sketch-height)

    (q/text-size 12)

    (doseq [[keys param label] params
            :let [idx (.indexOf (mapv second params) param)]
            :when (some? idx)]
      (let [col (mod idx 3)
            row (quot idx 3)
            x (+ 20 (* col 260))
            y (+ sketch-height 22 (* row 28))]
        (q/fill 120)
        (q/text (str keys " " label ":") x y)
        (q/fill 0)
        (q/text (format-val param (get state param)) (+ x 140) y)))

    (q/fill 120)
    (q/text-size 10)
    (q/text "UP = export SVG  |  G = reset to defaults" 20 (- preview-height 12))))

(defn export
  "saves svg to a file"
  [state]
  (let [name "ginkgo"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (draw state))
    (q/save gr)))

(defn key-pressed
  "Handle keypresses for parameter adjustment and export"
  [state event]
  (let [k (:key event)]
    (condp = k
      (keyword "q") (update state :initial-length #(max 1 (- % 5)))
      (keyword "w") (update state :initial-length + 5)
      (keyword "a") (update state :branch-ratio #(max 0.05 (- % 0.05))) ;; Clamped 0.05 - 0.95
      (keyword "s") (update state :branch-ratio #(min 0.95 (+ % 0.05))) ;; Clamped 0.05 - 0.95
      (keyword "z") (update state :length-decay #(max 0.1 (- % 0.02)))
      (keyword "x") (update state :length-decay + 0.02)
      (keyword "e") (update state :max-depth #(max 1 (- % 1)))
      (keyword "r") (update state :max-depth + 1)
      (keyword "d") (update state :max-global-angle #(max 0.1 (- % 0.1)))
      (keyword "f") (update state :max-global-angle + 0.1)
      (keyword "c") (update state :num-initial-veins #(max 0 (- % 1)))
      (keyword "v") (update state :num-initial-veins + 1)
      (keyword "g") default-state
      :up (do (export state) state)
      state)))

(q/defsketch quil-party
  :title "ginkgo"
  :size [sketch-width preview-height]
  :setup setup
  :draw preview
  :update update-state
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
