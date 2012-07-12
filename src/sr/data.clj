(ns sr.data)

(defn create
  [fnames]
  (ref {:fnames fnames}))

(defn change
  [data & args]
  (dosync
    (apply alter data update-in args)))

(defn count-imgs
  [data]
  (count (:imgs @data)))


