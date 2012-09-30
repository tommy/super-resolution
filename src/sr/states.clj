(ns sr.states
  (:use quil.core)
  (:use sr.data)
  (:use sr.logging))
 

;; ORDER OF STATES
(def next-step
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
  (prn @data))

(defn advance-step-synchronous
  [data]
  {:pre [(ref? data)]}
  (change data [:step] next-step)
  (let [result (step-do data)]
    (make data [:step-do (the-step data)] (future result))))

(defn advance-step
  [data]
  {:pre [(ref? data)]}
  (change data [:step] next-step)
  (let [result (future (step-do data))]
    (make data [:step-do (the-step data)] result)))

(defn checked-step-transition
  [data]
  {:pre [(ref? data)]}
  (when (done? data)
    (println "Old state is: " (the-step data))
    (advance-step data)
    (println "New state is: " (the-step data))))

(load "states/feature_matching")
(load "states/transform")
(load "states/show_transformation")
