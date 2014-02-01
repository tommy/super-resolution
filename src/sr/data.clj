(ns sr.data
  (:import [processing.core PImage])
  (:require [clojure.java.io :as jio]
            [clojure.walk :as walk]
            [quil.applet :refer [current-applet]]
            [clojure.tools.logging :refer [spy]]))

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

;; SERIALIZATION OF @DATA
(defn- image-ref
  [^PImage img]
  (let [fname (str "data/reference-images/" (. java.util.UUID randomUUID) ".png")
        app (when-not (.parent img) (quil.applet/applet))]
    (when app ;; horrible hack, since processing has a sucky design w/r/t images.
      (set! (.parent img) app)) ;; (yes, img.parent is a public field, used only in .save to
    (.save img fname)           ;;  get an absolute path for the output file. whatever.)
    (when app
      (quil.applet/applet-close app))
    (list 'LOAD-PIMAGE fname)))

(defn- future-ref
  [fut]
  (if (realized? fut)
    (list 'FUTURE @fut)
    (list 'FUTURE 'LOST)))
    ;(throw (RuntimeException. "Tried writing a data object with pending computation."))))

(defn replace-instances-with-references
  [data]
  (let [f (fn [x]
            (cond
              (= PImage (class x)) (image-ref x)
              (future? x) (future-ref x)
              :otherwise x))]
    (walk/prewalk f data)))

(defn write
  "Write the pretty printed version of data to out-file."
  [data out-file]
  {:pre [(not (ref? data))]}
  (with-open [w (jio/writer out-file)]
    (binding [*out* w]
      (clojure.pprint/write
        (replace-instances-with-references data)))))

;; DE-SERIALIZATION OF @DATA
(defn load-referenced-objects
  "Walk the given form and replace all instances of
  (LOAD-PIMAGE abc) with a PImage instance."
  ([form]
   (load-referenced-objects form (current-applet)))

  ([form applet]
   (let [f (fn [x]
             (if-not (list? x) x
               (condp = (first x)
                 'LOAD-PIMAGE (.loadImage applet (second x))
                 'FUTURE (future (second x))
                 x)))]
     (walk/postwalk f form))))

(defn read
  ([in-file]
   (read in-file (current-applet)))
  ([in-file applet]
   (let [form (read-string (slurp in-file))]
     (load-referenced-objects form applet))))
