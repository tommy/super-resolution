(ns sr.gui
  (:require [sr.projective :as proj])
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
  (let [f (fn [m p] (into m (transform-img data p)))]
    (make data [:trans]
      (reduce f {} ps))))

(defn by-state
  "The current state. Used for multimethod dispatch."
  [& args]
  @(state :step))

(def next-step
  {nil :feature-match
   :feature-match :transform})

(def step-do
  {nil (fn [_] nil)
   :feature-match (fn [_] nil)
   :transform (fn [data]
                (transform-imgs data
                  (proj/calculate-transformations data)))})

(defn advance-step
  [data]
  (dosync
    (alter (state :step) next-step)
    (alter (state :step-do)
      assoc @(state :step)
      ((step-do @(state :step)) data))))

(defn set-step
  []
  (set-state! :step (ref (next-step nil))
              :step-do (ref {})))


;; SETUP

(defn load-imgs
  [data]
  (let [fnames (:fnames @data)
        f (fn [m k] (assoc m k (load-image k)))
        m (reduce f {} fnames)]
    (make data [:imgs] m)))

(defn setup
  [data]
  (do
    (load-imgs data)
    (init-features data)
    (set-step)
    (prn @data)))


;; DRAWING

(defn paint-img
  [data]
  (set-image 0 0 (current-image data)))

(defmulti draw by-state)

(defmethod draw :feature-match
  [data]
  (do
    (background 10)
    (paint-img data)))

(defmethod draw :transform
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

      (text "B" 0 30)
      (set-image 0 30 img-b)
      (text "B'" 0 50)
      (set-image 0 50 img-b')

      )))


;; CLICK HANDLING

(defn checked-step-transition
  [data]
  (when (feature-matching-done? data)
    (advance-step data)
    (println "New state is: " @(state :step))))

(defmulti click-handle by-state)

(defmethod click-handle :feature-match
  [data]
  (do
    (let [x (mouse-x)
          y (mouse-y)]
      (add-feature data x) ; only 1D feature for now
      (drop-curr data)
      (checked-step-transition data)
      (prn @data)
      )))

(defmethod click-handle :default
  [data]
  nil)

(defn open
  [fnames]
  (let [data (create fnames)]
    (defsketch feature-matching
      :title "SR"
      :setup (partial setup data)
      :draw (partial draw data)
      :mouse-clicked (partial click-handle data)
      :size [300 300])))
