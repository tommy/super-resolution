(ns sr.util
  (:require [incanter.core :as i]))

(defn map-vals
  "Maps f over the values of m and returns a map of the result."
  [f m]
  {:pre [(map? m) (fn? f)]
   :post [(map? %)
          (= (set (keys m))
             (set (keys %)))]}
  (reduce
    #(update-in %1 [%2] f)
    m
    (keys m)))

(defn safe-nth
  [coll n i default]
  (if
    (<= 0 i (dec n)) (nth coll i)
    default))

(defn safe-nth-2
  [mat [ix iy] default]
  (let [m' (safe-nth mat (i/nrow mat) iy default)]
    (if (coll? m')
      (safe-nth m' (i/ncol mat) ix default)
      m')))

