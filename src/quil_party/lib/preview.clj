(ns quil-party.lib.preview
  (:require [quil.core :as q]))

(defn parameter-background
  "Draws the background for the parameter display section"
  [sketch-height sketch-width preview-height]
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
  (q/text "current params:" x-start y-start)
  
  (doseq [[i [param-name param-value]] (map-indexed vector params)]
    (let [y-pos (+ y-start 20 (* i line-height))]
      (display-param param-name param-value x-start y-pos))))
