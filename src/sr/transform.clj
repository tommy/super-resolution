(ns sr.transform
  (:require [incanter.core :as i])
  (:import processing.core.PImage)
  (:require [sr.util :refer [safe-nth to-row-major from-row-major]])
  (:require [sr.logging :refer [note as-task-item task]])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :as pp]))

(set! *warn-on-reflection* true)

(defn- one-row
  "Returns a seq of the pixel values of the first row of a PImage."
  [^PImage img]
  (let [w (.width img)
        px (.pixels img)]
    (take w (vec px))))

(defn- one-d-img
  "Returns a seq of pixel values for an image h pixels high, where
  each row is identical."
  [h row]
  (let [s (* h (count row))]
    (take s (cycle row))))

(defn- transform-1d-vector
  "Apply a projective transformation p to a 1D vector."
  [old p]
  {:pre [(coll? old)
         (fn? p)]}
  (let [rs (range (count old))
        n (count old)
        newvec
         (map (comp #(safe-nth old n % 0) p) rs)]
    (vec newvec)))

(defn- transform-1d
  "Apply the (1-dimensional) projective transformation p to the
  PImage oldimg, and return the result as a PImage."
  [^PImage oldimg p]
  (let [w (.width oldimg)
        h (.height oldimg)
        newimg (PImage. w h (int 1))
        oldvec (one-row oldimg)
        newvec (one-d-img h (transform-1d-vector oldvec p))]
    (set! (.pixels newimg) (into-array Integer/TYPE newvec))
    (.updatePixels newimg)
    newimg))

(defn- safe-get
  [ary w h default [x y]]
  (if (and (<= 0 x (dec w))
           (<= 0 y (dec h)))
    (aget ary ((to-row-major w) [x y]))
    default))

(defn transform-2d
  [^PImage oldimg p]
  (let [w (.width oldimg)
        h (.height oldimg)
        oldpxs (.pixels oldimg)
        newimg (PImage. w h (int 1))
        pt-transform
          (as-task-item :transformation-progress
            (comp
              (partial safe-get oldpxs w h 0)
              (partial map #(Math/round %))
              p
              i/matrix
              (from-row-major w)))
        newpxs (.pixels newimg)
        _ (task :transformation-progress (* w h))]
    (note
      (doseq [idx (range (alength newpxs))]
        (aset newpxs idx (pt-transform idx))))
    (.updatePixels newimg)
    newimg))

(defn transform
  "Apply the projective transformation p to the PImage oldimg,
  and return the result as a PImage."
  [data ^PImage oldimg p]
  {:pre [(contains? #{1 2} (get-in data [:dimension]))
         (= PImage (class oldimg))]
   :post [(= PImage (class %))]}
  (case (get-in data [:dimension])
    1 (note (transform-1d oldimg p))
    2 (note (transform-2d oldimg p))))
