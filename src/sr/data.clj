(ns sr.data)

(defn create
  [fnames]
  {:pre [(not (empty? fnames))]}
  (ref {:fnames fnames}))

(defn make
  [data ks v]
  {:pre [(coll? ks) (not (empty? ks))]}
  (dosync
    (alter data update-in ks (constantly v))))

(defn change
  [data & args]
  (dosync
    (apply alter data update-in args)))

(defn count-imgs
  [data]
  {:post [(< 0 %)]}
  (count (:imgs @data)))

(def valid-states
  #{nil
    :feature-match
    :transform
    :show-transformation})

(defn the-step
  [data]
  {:pre [(not (nil? data))]
         ;(not (nil? (:step @data)))]
   :post [(contains? valid-states %)]}
  (:step @data))
