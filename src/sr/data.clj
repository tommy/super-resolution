(ns sr.data)

(defn create
  [fnames]
  (ref {:fnames fnames}))

(defn make
  [data ks v]
  (dosync
    (alter data update-in ks (constantly v))))

(defn change
  [data & args]
  (dosync
    (apply alter data update-in args)))

(defn count-imgs
  [data]
  (count (:imgs @data)))


