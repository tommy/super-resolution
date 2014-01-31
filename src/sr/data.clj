(ns sr.data
  (:require [clojure.java.io :as jio]))

(def ref?
  (comp (partial = clojure.lang.Ref) class))

(defn create
  "Create a new data ref, initialized with the given
  image filenames."
  
  ([fnames]
   (create fnames 2))

  ([fnames d]
   {:pre [(<= 1 d 2)
          (not (empty? fnames))]}
   (ref {:fnames fnames :dimension d})))

(defn make
  "Set the value keyed by ks (nested as in update-in)
  in the data ref."
  [data ks v]
  {:pre [(coll? ks)
         (seq ks)
         (ref? data)]}
  (dosync
    (alter data update-in ks (constantly v))))

(defn change
  "Update the value keyed by the given (nested) keys
  in the data ref. args will be the arguments passed to
  update-in.

  Example: (change data [:step] {:first :second, :second :third})"
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
  "Returns the current step of the SR construction."
  [data]
  {:pre [(not (nil? data))]
   :post [(valid-states %)]}
  (:step @data))

(defn get-image
  "Get the PImage instance for the given filename."
  [data fname]
  {:pre [(not (ref? data))]}
  ((:imgs data) fname))

(defn write
  "Write the pretty printed version of data to out-file."
  [data out-file]
  {:pre [(not (ref? data))]}
  (with-open [w (jio/writer out-file)]
    (binding [*out* w]
      (clojure.pprint/write data))))
