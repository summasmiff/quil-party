(ns quil-party.lib.preview
  (:require [quil.core :as q]))

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
