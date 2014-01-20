(ns sr.gui
  (:require [sr.projective :as proj])
  (:use [clojure.tools.logging :only [spy]])
  (:require [sr.states :refer [advance-step click-handle draw]])
  (:require [quil.core :refer [defsketch load-image]])
  (:require [sr.data :refer [ref? make create]])
  (:require [sr.feature :refer [init-features]]))

;; SETUP

(defn load-imgs
  "Load PImages referenced by filenames."
  [data]
  {:pre [(ref? data)
         (not (empty? (:fnames @data)))]
   :post [(not (nil? (:imgs @data)))]}
  (let [fnames (:fnames @data)
        f (fn [m k] (assoc m k (load-image k)))
        m (reduce f {} fnames)]
    (make data [:imgs] m)))

(defn set-step
  [data]
  {:pre [(ref? data)]}
  (make data [:step] nil)
  (make data [:step-do] {})
  (advance-step data))


(defn setup
  "Set up function for the sketch. Initializes values in the data object."
  [data]
  {:pre [(ref? data)]}
  (doto data
    load-imgs
    init-features
    set-step)
  (prn @data))


(defn open

  ([fnames]
  (open fnames 2))

  ([fnames dimension]
  (let [data (create fnames dimension)]
    (defsketch sr
      :title "SR"
      :setup (partial setup data)
      :draw (partial draw data)
      :mouse-clicked (partial click-handle data)
      :size [300 300])))

  ([fnames dimension features]
  (let [data (create fnames dimension)]
    (defsketch sr
      :title "SR"
      :setup (fn []
               (do
                 (make data [:feature-match :features]
                   {(first fnames) features})
                 (setup data)
                 (advance-step data)))
      :draw (partial draw data)
      :mouse-clicked (partial click-handle data)
      :size [300 300]))))
