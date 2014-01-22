(ns sr.util
  (:require [incanter.core :as i]
            [sr.logging :refer [note]]))

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
  "Like nth, but returns the specified default value if i is outside of [0,n)"
  [coll n i default]
  (if
    (<= 0 i (dec n)) (nth coll i)
    default))

(defn safe-nth-2
  "Two-dimensional version of safe-nth. Returns default value if x or y is
  ouside the matrix."
  [mat [ix iy] default]
  (if (and (<= 0 ix (i/ncol mat))
           (<= 0 iy (i/nrow mat)))
    (note (nth (nth mat iy) ix))
    default))

(defn to-row-major
  "Takes a width. Returns a function that converts (x, y) coordinates
  into the index of that element in a row-major ordered vector."
  [width]
  (fn [[ix iy]]
    (+ ix (* width iy))))

(defn from-row-major
  "Takes a width. Returns a function that converts a row-major
  index to the (x, y) coordinates."
  [width]
  (fn [idx]
    [(mod idx width)
     (quot idx width)]))
