(ns sr.transform
  (:require [incanter.core :as i])
  (:import processing.core.PImage)
  (:use sr.util)
  (:use sr.logging)
  (:require [clojure.tools.logging :as log]))

(defn one-row
  "Returns a seq of the pixel values of the first row of a PImage."
  [img]
  (let [w (.width img)
        px (.pixels img)]
    (take w (vec px))))

(defn one-d-img
  "Returns a seq of pixel values for an image h pixels high, where
  each row is identical."
  [h row]
  (let [s (* h (count row))]
    (take s (cycle row))))

(defn transform-1d-vector
  "Apply a projective transformation p to a 1D vector."
  [old p]
  {:pre [(coll? old)
         (fn? p)]}
  (let [rs (range (count old))
        n (count old)
        newvec
         (map (comp #(safe-nth old n % 0) p) rs)]
    (vec newvec)))

(defn transform-2d-matrix
  "Result is returned as row-major seq."
  [mat p]
  (let [pt-transform
         (as-task-item :transformation-progress
           (comp #(safe-nth-2 mat % 0) p i/matrix))
        cols (i/ncol mat)
        rows (i/nrow mat)
        _ (task :transformation-progress (* cols rows))]
    (map
      pt-transform
      (for [x (range cols)
            y (range rows)]
        (vector x y)))))

(defn transform-1d
  "Apply the (1-dimensional) projective transformation p to the
  PImage oldimg, and return the result as a PImage."
  [oldimg p]
  (let [w (.width oldimg)
        h (.height oldimg)
        newimg (PImage. w h (int 1))
        oldvec (one-row oldimg)
        newvec (one-d-img h (transform-1d-vector oldvec p))]
    (do
      (set! (.pixels newimg) (into-array Integer/TYPE newvec))
      (.updatePixels newimg)
      newimg)
        ))

(defn transform-2d
  "Apply the (2-dimensional) projective transformation p to the
  PImage oldimg, and return the result as a PImage."
  [oldimg p]
  (let [w (.width oldimg)
        h (.height oldimg)
        newimg (note (PImage. w h (int 1)))
        oldmat (note (i/matrix (seq (.pixels oldimg)) w))
        newseq (note (transform-2d-matrix oldmat p))
        newpxs (into-array Integer/TYPE newseq)]
    (do
      (prn "Got the transformed img")
      (note (set! (.pixels newimg) newpxs))
      (note (.updatePixels newimg))
      ;(.save newimg "/home/tommy/transformed-image.png")
      (log/spy newimg))))

(defn transform
  "Apply the projective transformation p to the PImage oldimg,
  and return the result as a PImage."
  [data oldimg p]
  {:pre [(contains? #{1 2} (get-in data [:dimension]))
         (= PImage (class oldimg))]
   :post [(= PImage (class %))]}
  (case (get-in data [:dimension])
    1 (transform-1d oldimg p)
    2 (transform-2d oldimg p)))
