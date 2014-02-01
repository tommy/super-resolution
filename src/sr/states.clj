(ns sr.states
  (:require [quil.core :refer [background text-font create-font text
                               height width set-image mouse-x mouse-y
                               color rect-mode fill stroke rect save-frame]]
            [sr.data :refer [ref? make change the-step get-image] :as data]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]))
 

;; ORDER OF STATES
(def ^:private next-step
  {nil :feature-match
   :feature-match :transform
   :transform :show-transformation})

(def states
  (let [ks (keys next-step)
        vs (vals next-step)]
    (set (into ks vs))))

;; PREDICATE FOR DETERMINING WHEN TO TRANSITION TO NEXT STATE
(defmulti done? the-step)
(defmethod done? :default
  [data]
  false)

;; ASYNCHRONOUS STATE CHANGE ACTION
(defmulti step-do the-step)
(defmethod step-do :default
  [data]
  nil)

;; DRAWING
(defmulti draw the-step)
(defmethod draw :default
  [data]
  (let [x 0 y (/ (height) 3)]
    (background 10)
    (text-font (create-font "Georgia" 24 true))
    (text 
      (str "No draw method defined for state " (the-step data))
      x y)))

;; HANDLE CLICK EVENTS
(defmulti click-handle the-step)
(defmethod click-handle :default
  [data]
  (log/warn "== Default click handler.")
  (pp/pprint @data))

;; STEP TRANSITION
(defn advance-step
  [data]
  {:pre [(ref? data)]}
  (change data [:step] next-step)
  (let [future-result (future-call #(step-do data))]
    (make data [:step-do (the-step data)] future-result)))

(defn checked-step-transition
  [data]
  {:pre [(ref? data)]}
  (when (done? data)
    (log/info "Old state is: " (the-step data))
    (advance-step data)
    (log/info "New state is: " (the-step data))))

(load "states/feature_matching")
(load "states/transform")
(load "states/show_transformation")
