(ns sr.data)

(defn ref?
  [x]
  (= clojure.lang.Ref
     (class x)))

(defn create
  [fnames]
  {:pre [(not (empty? fnames))]}
  (ref {:fnames fnames}))

(defn make
  [data ks v]
  {:pre [(coll? ks) (not (empty? ks))
         (ref? data)]}
  (dosync
    (alter data update-in ks (constantly v))))

(defn change
  [data & args]
  {:pre [(ref? data)]}
  (dosync
    (apply alter data update-in args)))

(defn count-imgs
  [data]
  {:pre [(ref? data)]
   :post [(< 0 %)]}
  (count (:imgs @data)))

(def valid-states
  #{nil
    :feature-match
    :transform
    :show-transformation})

(defn the-step
  [data]
  {:pre [(not (nil? data))]
   :post [(contains? valid-states %)]}
  (:step @data))

(defn do-data
  "Applies function f to data when (pred @data) evaluates
  to false. (Threadsafe.)"
  [pred f data]
  {:pre [(ref? data)]}
  (let [d @data]
    (when-not (pred d)
      (f d))))
