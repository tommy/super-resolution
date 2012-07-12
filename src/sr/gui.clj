(ns sr.gui
  (:require [sr.projective :as proj])
  (:use quil.core)
  (:use sr.data)
  (:use sr.feature))

(defn transform-img
  [data [i p]]
  (let [oldimg (nth (:imgs @data) i)
        newimg (proj/transform oldimg p)]
    [i newimg]))

(defn transform-imgs
  [data ps]
  (dosync
    (alter data assoc :trans
      (map (partial transform-img data) ps))))

(defn by-state
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



(defn within?
  [x' y' [x y w h]]
  (and
    (<= x x' (+ x w))
    (<= y y' (+ y h))))

(defn img-within?
  [x' y' pos]
  (if (within? x' y' (val pos))
    pos
    false))

(defn clicked-img
  [ps x y]
  (let [is (filter (partial img-within? x y) ps)]
    (first is)))

;; SETUP

(defn load-imgs
  [data]
  (let [fnames (:fnames @data)
        imgs (map load-image fnames)]
      (dosync
        (alter data assoc :imgs imgs))))

(defn setup
  [data]
  (do
    (load-imgs data)
    (dosync (alter data assoc :curr 0))
    (dosync (alter data assoc :features {:n 1}))
    ;(dosync (alter data assoc :pos {}))
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
  (let [imgs (:imgs @data)
        trans (:trans @data)
        img-a (first imgs)
        img-b (second imgs)
        img-b' (-> trans first second)]
    (do
      (background 10)
      ;(text-align :center)
      (text-font (create-font "Georgia" 10 true))
      (text "A" 0 10)
      (set-image 0 10 img-a)
      (text "B" 0 30)
      (set-image 0 30 img-b)
      (text "B'" 0 50)
      (set-image 0 50 img-b')
      )))


;; CLICK HANDLING

(defn checked-step-transition
  [data]
  (when (< 3 (num-features data))
    (advance-step data)
    (println "New state is: " @(state :step))))

(defmulti click-handle by-state)

(defmethod click-handle :feature-match
  [data]
  (do
    (let [x (mouse-x)
          y (mouse-y)
          n (num-features data)]
      (add-feature data n x) ; only 1D feature for now
      (inc-curr data)
      (checked-inc-feature data)
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
