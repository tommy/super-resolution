(ns sr.data)

(def ref?
  (comp (partial = clojure.lang.Ref) class))

(defn create
  [fnames d]
  {:pre [(<= 1 d 2)
         (not (empty? fnames))]}
  (ref {:fnames fnames :dimension d}))

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

(def ^:private valid-states
  #{nil
    :feature-match
    :transform
    :show-transformation})

(defn the-step
  [data]
  {:pre [(not (nil? data))]
   :post [(valid-states %)]}
  (:step @data))

(defn get-image
  [data fname]
  {:pre [(not (ref? data))]}
  ((:imgs data) fname))
