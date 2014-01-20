(ns sr.projective.one
  "The projective transformation that transforms the image X into
  the image U is defined by the parameters {a, b, c} found by
  solving the matrix equation:
  
  [u] = [x  1  -ux] * [a b c]'
  
  U = A * [a b c]'
  => [a b c]' = (A^-1) * U
  "
  (:require [incanter.core :as i])
  (:require [sr.math :refer [column]]))

(defn U1D
  "The k-by-1 column matrix [u]."
  [us]
  (column us))

(defn A1D
  "Generate A matrix in the system of linear equations
  for the 1 dimensional projective transformation."
  [[[u1 x1] [u2 x2] [u3 x3]]]
  (i/matrix
    [[x1 1 (- (* u1 x1))]
     [x2 1 (- (* u2 x2))]
     [x3 1 (- (* u3 x3))]]))

