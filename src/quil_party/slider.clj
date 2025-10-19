(ns quil-party.slider
  "basic interactivity via a slider"
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def sketch-width 800)
(def sketch-height 600)
(def preview-height (+ sketch-height 160))  ;; Add 160 pixels for parameter display
(def center-x 400)
(def center-y 300)

(defn setup 
  "init state for slider"
  []
  (q/frame-rate 10)
  {:size 100
   :slider-x 20
   :slider-y (+ sketch-height 60)
   :slider-width (- sketch-width 100)
   :slider-min 10
   :slider-max (- sketch-width 100)
   :slider-dragging? false})

(defn draw-vectors 
  "all vectors should have (q/stroke-weight 1.5) (q/stroke 0) and (q/fill nil) for pen plotter"
  [size]
  (q/stroke-weight 1.5)
  (q/stroke 0)
  (q/fill nil)
  (q/ellipse center-x center-y size size))

(defn draw-slider
  "Draw a slider and handle its interaction"
  [state]
  (let [{:keys [size slider-x slider-y slider-width slider-min slider-max slider-dragging?]} state]
    ;; slider track
    (q/stroke 150)
    (q/stroke-weight 2)
    (q/line slider-x slider-y (+ slider-x slider-width) slider-y)

    (let [slider-pos (+ slider-x (* (/ (- size slider-min) (- slider-max slider-min)) slider-width))]
      ;; slider handle
      (q/fill (if slider-dragging? [50 50 200] [100 100 100]))
      (q/no-stroke)
      (q/ellipse slider-pos slider-y 20 20)

      ;; slider labels
      (q/fill 0)
      (q/text-size 12)
      (q/text (str "Min: " slider-min) slider-x (+ slider-y 25))
      (q/text (str "Max: " slider-max) (- (+ slider-x slider-width) 50) (+ slider-y 25)))))

(defn preview
  "preview window"
  [state]
  (let [size (:size state)]
    (q/background 255)
    (draw-vectors size)

    ;; interactivity zone
    (q/stroke 200)
    (q/line 0 sketch-height sketch-width sketch-height)
    (q/stroke 0)
    (q/fill 0)
    (q/text-size 14)
    (q/text "current state:" 20 (+ sketch-height 12))
    (q/text (str "size: " size) 20 (+ sketch-height 30))
    (draw-slider state)))

(defn mouse-pressed
  "Handle mouse press to check if slider is clicked"
  [state event]
  (let [{:keys [slider-x slider-y slider-width slider-min slider-max]} state
        mx (:x event)
        my (:y event)
        ;; Calculate slider position based on size
        slider-pos (+ slider-x (* (/ (- (:size state) slider-min) (- slider-max slider-min)) slider-width))]
    (if (and (<= (- slider-pos 15) mx (+ slider-pos 15))
             (<= (- slider-y 15) my (+ slider-y 15)))
      (assoc state :slider-dragging? true)
      state)))

(defn mouse-dragged
  "Handle mouse drag to update slider value"
  [state event]
  (let [{:keys [slider-x slider-width slider-min slider-max slider-dragging?]} state
        mx (:x event)]
    (if slider-dragging?
      ;; Calculate new size based on mouse position
      (let [new-size (int (+ slider-min (* (/ (- mx slider-x) slider-width) (- slider-max slider-min))))]
        (assoc state :size (max slider-min (min slider-max new-size))))
      state)))

(defn mouse-released
  "Handle mouse release to stop dragging"
  [state _event]
  (assoc state :slider-dragging? false))

(defn export 
  "svg export"
  [state]
  (let [name "circle"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)
        size (:size state)]
    (q/with-graphics gr
      (q/stroke-weight 1.5)
      (q/stroke 0)
      (q/fill nil)
      (draw-vectors size))
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
  :mouse-pressed mouse-pressed
  :mouse-dragged mouse-dragged
  :mouse-released mouse-released
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])