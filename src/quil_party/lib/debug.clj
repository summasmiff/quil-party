(ns quil-party.lib.debug
  (:require [quil.core :as q]))

(defn debug
  [value]
  (q/fill 255 0 0)
  (q/text-size 16)
  (q/text (str value) 5 -5))

