(ns sr.math
  (:require [incanter.core :as i]))

(defn column
  "A column vector."
  [xs]
  {:pre [(every? (comp not coll?) xs)]
   :post [(= [(count xs) 1] (i/dim %))]}
  (i/matrix xs))

(defn row
  "A row vector."
  [xs]
  {:pre [(every? (comp not coll?) xs)]
   :post [(= [1 (count xs)] (i/dim %))]}
  (i/matrix [xs]))


(defn matricies?
  [& args]
  (cond
    (every? i/matrix? args) :matrix
    (every? (comp not i/matrix?) args) :scalar
    :otherwise :matrix-and-scalar))

(defmulti mult
  "Returns the product of the arguments.

  For matrix arguments, use matrix multiplication.
  For non-matrix arguments, use scalar multiplication."
  matricies?)

(defmethod mult :matrix
  [& args]
  (apply i/mmult args))

(defmethod mult :scalar
  [& args]
  (apply i/mult args))

(defn product-sum
  "a * b + c"
  [a b c]
  (i/plus
    (mult a b)
    c))
