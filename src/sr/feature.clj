(ns sr.feature
  (:require [sr.data :refer [ref? make change]]))

(defn feature-matching-done?
  "True if sufficiently many features have been identified."
  [data]
  {:pre [(not (ref? data))
         (not (nil? (get-in data [:feature-match :rest])))]}
  (empty?
    (get-in data [:feature-match :rest])))

;; Functions on the data object

(def valid-dimensions #{1 2})

(defn features-needed
  "The minimum number of features that need to be identified across
  the n images in order to be able to solve the linear system of
  equations.

  This value depends on the dimension of the images."
  [data]
  {:pre [(contains? valid-dimensions (get-in data [:dimension]))]}
  (case (get-in data [:dimension])
    1 3
    2 4))

(defn ordered-fnames
  "Returns a seq of the fnames of the images in the order that they
  should appear on screen to perform the feature matching."
  [data]
  {:pre [(not (ref? data))]}
  (let [fnames (keys (:imgs data))
        cycles (fn [n xs] (take (* n (count xs)) (cycle xs)))]
    (cycles (features-needed data) fnames)))


(defn primary
  "The primary image of the set.

  That is, the image into whose projective space the other images
  will be transformed."
  [data]
  {:pre [(not (ref? data))]
   :post [(not (nil? %))]}
  (get-in data [:feature-match :primary :fname]))

(defn init-features
  "Initialize some of the properties that will be needed for the
  feature matching."
  [data]
  {:pre [(ref? data)]
   :post [(not (nil? (get-in @data [:feature-match :rest])))
          (not (nil? (get-in @data [:feature-match :primary :fname])))]}
  (let [os (ordered-fnames @data)
        primary (first os)]
    (prn os)
    (make data [:feature-match :rest] os)
    (make data [:feature-match :primary :fname] primary)))

(defn current-fname
  "The fname of the image whose features is currently being identified."
  [data]
  {:pre [(not (ref? data))]
   :post [(not (nil? %))]}
  (first
    (get-in data [:feature-match :rest])))

(defn current-image
  "The PImage whose feature is currently being identified."
  [data]
  {:pre [(not (ref? data))]
   :post [(not (nil? %))]}
  (let [c (current-fname data)]
    (get-in data [:imgs c])))

(defn drop-curr
  "Drop the image of which we just identified a feature
  and move on to the next one."
  [data]
  {:pre [(ref? data)]}
  (change data [:feature-match :rest] rest))

(defn curr-primary-feature
  "The location of the current primary feature (the u value)."
  [data]
  {:pre [(not (ref? data))]}
  (get-in data [:feature-match :primary :current-feature]))

(defn feature-pair
  "A matched feature between the primary image and one of the
  secondary images.

  Uses the currently known primary feature."
  [data x]
  {:pre [(not (ref? data))]}
  (let [u (curr-primary-feature data)]
    {:u u :x x}))

(defn set-current-primary-feature
  "Set the current primary feature, which will be paired with each
  feature clicked on the secondary images, until a new feature of
  the primary image is identified."
  [data u]
  {:pre [(ref? data)]}
  (make data [:feature-match :primary :current-feature] u))

(defn add-secondary-feature
  "Add an identified secondary feature, paired with the current
  primary feature."
  [data curr x]
  {:pre [(ref? data)]}
  (change data [:feature-match :features curr]
    conj (feature-pair @data x)))

(defn add-feature
  "Identify a new feature.

  If the current image is the primary image, save the feature
  as the current primary feature.

  If the current image is a secondary image, pair it with the
  current primary feature."
  [data p]
  {:pre [(ref? data)]}
  (let [curr (current-fname @data)]
    (if (= curr (primary @data))
      (set-current-primary-feature data p)
      (add-secondary-feature data curr p))))
