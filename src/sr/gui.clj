(ns sr.gui
  (:require [sr.projective :as proj])
  (:use [clojure.tools.logging :only [spy]])
  (:use sr.logging)
  (:use quil.core)
  (:use sr.data)
  (:use sr.feature)
  (:use sr.image))

(defn transform-img
  [data [fname p]]
  (let [oldimg (get-image data fname)
        newimg (proj/transform oldimg p)]
    {fname newimg}))

(defn transform-imgs
  [data ps]
  (let [f (fn [m p] (note (into m (transform-img data p))))]
    (make data [:trans]
      (reduce f {} ps))))

(defn the-state
  "The current state. Used for multimethod dispatch."
  [& args]
  @(state :step))

(def nothing (constantly nil))

(def next-step
  {nil :feature-match
   :feature-match :transform
   :transform :show-transformation})

(defmulti step-do the-step)

(defmethod step-do :default
  [data]
  nil)

(defmethod step-do :transform
  [data]
  (prn "about to transform")
  (transform-imgs data
    (proj/calculate-transformations data)))

(defn advance-step
  [data]
  (dosync
    (alter (state :step) next-step))
  (let [result (future (step-do data))]
    (dosync
      (alter (state :step-do)
        assoc (the-state) result))
    ;(prn @result)))
  ))


(defmulti done? the-state)

(defmethod done? :feature-matching
  [data]
  (feature-matching-done? data))

(defmethod done? :transform
  [data]
  (realized? (:transform @(state :step-do))))

(defmethod done? :default
  [& args]
  false)

;; SETUP

(defn load-imgs
  "Load PImages referenced by filenames."
  [data]
  (let [fnames (:fnames @data)
        f (fn [m k] (assoc m k (load-image k)))
        m (reduce f {} fnames)]
    (make data [:imgs] m)))

(defn set-step
  [data]
  (let [step (ref nil)
        step-do (ref {})]
    (set-state! :step step
                :step-do step-do)
    (make data [:step] step)
    (make data [:step-do] step-do))
  (advance-step data))


(defn setup
  "Set up function for the sketch. Initializes values in the data object."
  [data]
  (doto data
    load-imgs
    init-features
    set-step)
  (prn @data))


;; DRAWING

(defn paint-img
  "Feature-matching step.
  
  Paint the current PImage to the screen with its origin at (0,0)."
  [data]
  (set-image 0 0 (current-image data)))

(defn progress-bar
  [id]
  (let [h 20
        y (- (/ (height) 2) (/ h 2))
        total (/ (width) 3)
        prog (* total (progress id))
        x (- (/ (width) 2) (/ total 2))
        color-total (color 100)
        color-done (color 200)]
    (rect-mode :corner)

    (fill color-total)
    (stroke color-total)
    (rect x y total h)

    (fill color-done)
    (stroke color-done)
    (rect x y prog h)))


(defmulti draw the-state)

(defmethod draw :feature-match
  [data]
  (do
    (background 10)
    (paint-img data)))

(defmethod draw :transform
  [data]
  (do
    (background 10)
    (text-font (create-font "Georgia" 10 true))
    (text "Transforming..." 0 (/ (height) 3))
    (progress-bar :trans)))

(defmethod draw :display-transformation
  [data]
  (let [prim (get-image data (primary data))
        trans (:trans @data)
        boths (map #(vector (get-image data (key %)) (val %)) trans)
        img-b (ffirst boths)
        img-b' (second (first boths))]
    (do
      (background 10)
      (text-font (create-font "Georgia" 10 true))
      (text "U" 0 10)
      (set-image 0 10 prim)

      (let [y (+ 30 (.height prim))]
        (text "B" 0 y)
        (set-image 0 y img-b)
        (let [y' (+ 30 y (.height img-b))]
          (text "B'" 0 y')
          (set-image 0 y' img-b')))

      )))


;; CLICK HANDLING

(defn checked-step-transition
  [data]
  (when (done? data)
    (advance-step data)
    (println "New state is: " (the-state))))

(defmulti click-handle the-state)

(defmethod click-handle :feature-match
  [data]
  (do
    (let [x (mouse-x)
          y (mouse-y)]
      (add-feature data [x y])
      (drop-curr data)
      (checked-step-transition data)
      (prn @data)
      )))

(defmethod click-handle :default
  [data]
  (prn @data))

(defn open
  ([fnames]
  (let [data (create fnames)]
    (defsketch sr
      :title "SR"
      :setup (partial setup data)
      :draw (partial draw data)
      :mouse-clicked (partial click-handle data)
      :size [300 300])))
  ([fnames features]
  (let [data (create fnames)]
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
