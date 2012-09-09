(ns sr.util)

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

