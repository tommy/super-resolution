(ns sr.gui
  (:require [sr.projective :as proj])
  (:require [clojure.tools.logging :refer [spy] :as log])
  (:require [sr.states :refer [advance-step click-handle draw]])
  (:require [quil.core :refer [sketch load-image]])
  (:require [sr.data :refer [ref? make create] :as d])
  (:require [sr.feature :refer [init-features]]
            [clojure.pprint :as pp]))

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
  (pp/pprint @data)
  (flush))


(defn open

  ([fnames-or-data]
  (if (not (ref? fnames-or-data))
    (open fnames-or-data 2)
    (sketch
      :title "SR"
      :draw (partial draw fnames-or-data)
      :mouse-clicked (partial click-handle fnames-or-data)
      :size [400 600])))

  ([fnames dimension]
  (let [data (create fnames dimension)]
    (sketch
      :title "SR"
      :setup (partial setup data)
      :draw (partial draw data)
      :mouse-clicked (partial click-handle data)
      :size [400 600]))))

(defn open-saved
  [file]
  (let [app (quil.applet/applet)
        data (d/read file app)]
    (quil.applet/applet-close app)
    (log/info "== Opening saved app state. Step is " (:step data))
    (open (ref data))))
