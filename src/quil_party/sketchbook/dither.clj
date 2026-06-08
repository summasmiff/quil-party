(ns quil-party.sketchbook.dither
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; boring constants
(def sketch-width 600)
(def sketch-height 700)
(def preview-height (+ sketch-height 100))  ;; Add 80 pixels for instructions
(def tile-size 30)
(def angle (/ Math/PI 4)) ;; 45 degrees

;; the fun zone
(def source-img-path "test.jpg") ;; image paths must be located in the data directory of the current sketch

(defn hatch
  "Draw hatching lines:
   Inverts the value: 0.0 brightness (black) becomes 1.0 ink density with small spacing (tight lines).
   1.0 brightness (white) becomes 0.0 ink density with huge spacing (few lines)."
  [brightness angle]
  (let [ink-density (- 1.0 brightness)]
    (when (> ink-density 0.0)
      (let [max-iter (* 2 tile-size)
            spacing (max 1.0 (/ 1.0 ink-density))]
        (q/stroke 0)
        (q/rotate angle)
        (q/translate (- tile-size) (- tile-size))
        (loop [i 0]
          (when (< i max-iter)
            (q/line i 0 i max-iter)
            (recur (+ i spacing))))))))

(defn compute-brightness-map
  "Returns nested vector of brightness values for each tile.
   0.0 = black, 1.0 = white
   AI optimized for performance"
  [source-img]
  (let [w (.width source-img)
        h (.height source-img)
        cols (quot w tile-size)
        rows (quot h tile-size)
        ;; Hint that pixels is a primitive Java int array
        ^ints pixels (q/pixels source-img)
        ;; Precalculate the maximum possible raw sum per tile to divide only once
        tile-area (* tile-size tile-size)
        divisor (float (* 255 3 tile-area))]
    ;; Use transients for mutable data structures until we're done
    (loop [r 0
           res (transient [])]
      (if (< r rows)
        (let [y-start (* r tile-size)
              row-vec (loop [c 0
                             row-res (transient [])]
                        (if (< c cols)
                          (let [x-start (* c tile-size)
                                ;; Deep inner loops use primitive math to sum pixel values
                                tile-sum (loop [dy 0
                                                sum 0]
                                           (if (< dy tile-size)
                                             (let [y-idx (* (+ y-start dy) w)
                                                   next-sum (loop [dx 0
                                                                   s sum]
                                                              (if (< dx tile-size)
                                                                (let [idx (+ y-idx x-start dx)
                                                                      ;; Fast native array lookup
                                                                      pixel (aget pixels idx)
                                                                      ;; Fast bit-shifting for RGB channels
                                                                      r-val (bit-and (bit-shift-right pixel 16) 0xff)
                                                                      g-val (bit-and (bit-shift-right pixel 8) 0xff)
                                                                      b-val (bit-and pixel 0xff)
                                                                      pixel-sum (+ r-val g-val b-val)]
                                                                  (recur (unchecked-inc dx) (+ s pixel-sum)))
                                                                s))]
                                               (recur (unchecked-inc dy) next-sum))
                                             sum))
                                ;; Only divide once
                                brightness (/ (float tile-sum) divisor)]
                            (recur (unchecked-inc c) (conj! row-res brightness)))
                          (persistent! row-res)))]
          (recur (unchecked-inc r) (conj! res row-vec)))
        (persistent! res)))))

(defn get-tile-brightness
  "Returns value at x and y in brightness map"
  [brightness-map x y]
  (let [col (quot x tile-size)
        row (quot y tile-size)
        rows (count brightness-map)
        cols (if (pos? rows) (count (first brightness-map)) 0)
        val (when (and brightness-map
                       (<= 0 row) (< row rows)
                       (<= 0 col) (< col cols))
              (get-in brightness-map [row col]))]
    (or val 1.0)))

(defn setup []
  (q/frame-rate 1)
  (let [img (q/load-image source-img-path)]
    {:source-img img
     :brightness-map nil}))

(defn update-state [state]
  (let [src (:source-img state)]
    (if (and src (q/loaded? src) (nil? (:brightness-map state)))
      (assoc state :brightness-map (compute-brightness-map src))
      state)))

(defn draw [state]
  (q/background 255)

  (let [cols (quot (q/width) tile-size)
        rows (quot (q/height) tile-size)]

    (dotimes [row rows]
      (dotimes [col cols]
        (let [x (* col tile-size)
              y (* row tile-size)
              brightness (get-tile-brightness (:brightness-map state) x y)]
          (q/push-matrix)
          (q/translate x y)

          ;; Draw grid for debugging
          (q/no-fill) (q/stroke 200) (q/rect 0 0 tile-size tile-size)

          ;; Call the specific tile function
          ;; TODO make more fun tile functions
          (hatch brightness angle)

          (q/pop-matrix))))))

(defn preview
  "preview window"
  [state]
  (q/background 255)
  (q/push-matrix)
  (when (:brightness-map state)
    (draw state))
  (q/pop-matrix) ;; Reset any transforms from draw

  ;; parameter review section
  (q/fill 255)
  (q/rect 0 sketch-height sketch-width (- preview-height sketch-height))
  (q/stroke 200)
  (q/line 0 sketch-height sketch-width sketch-height)
  (q/stroke 0)
  (q/fill 0)
  (q/text-size 14)
  (let [brightness-map (:brightness-map state)
        base-y (+ sketch-height 20)]
    (if (:brightness-map state)
      (do
        (q/fill 0 150 0) ; green for loaded
        (q/text (str "State: :brightness-map Loaded (" (* (count brightness-map) (count (first brightness-map))) " tiles)") 20 base-y)
        (q/fill 0))
      (do
        (q/fill 150 0 0) ; red for warning
        (q/text "State: :brightness-map loading..." 20 base-y)
        (q/fill 0))))
  state)

(defn export
  "saves svg to a file"
  [state]
  (let [name "dither"
        frame-num (q/frame-count)
        svg (str "svg/" name "-" frame-num ".svg")
        gr (q/create-graphics sketch-width sketch-height :svg svg)]
    (q/with-graphics gr
      (draw state))
    (q/save gr)))

(defn key-pressed
  "trigger export by pressing up"
  [state event]
  (when (= (:key event) :up)
    (export state))
  state)

(q/defsketch dither
  :title "dither"
  :size [sketch-width preview-height]
  :setup setup
  :update update-state
  :draw preview
  :key-pressed key-pressed
  :middleware [m/fun-mode]
  :features [:keep-on-top])
