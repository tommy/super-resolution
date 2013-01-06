(ns sr.projective.two
  "The projective transformation that transforms the image X
  into the image U as defined by the parameters {A, b, c} found by
  solving the system of equations
  
  [-x1']   [ x1x1' y1x1' -x1 -y1   0   0 -1  0 ]   [c1 ]
  [-y1']   [ x1y1' y1y1'   0   0 -x1 -y1  0 -1 ]   [c2 ]
  [-x2']   [ x2x2' y2x2' -x2 -y2   0   0 -2  0 ]   [a11]
  [-y2'] = [ x2y2' y2y2'   0   0 -x2 -y2  0 -2 ] . [a12]
  [-x3']   [ x3x3' y3x3' -x3 -y3   0   0 -3  0 ]   [a21]
  [-y3']   [ x3y3' y3y3'   0   0 -x3 -y3  0 -3 ]   [a22]
  [-x4']   [ x4x4' y4x4' -x4 -y4   0   0 -4  0 ]   [b1 ]
  [-y4']   [ x4y4' y4y4'   0   0 -x4 -y4  0 -4 ]   [b2 ]

  where 
  A = [a11 a12]
      [a21 a22]

  b = [b1 b2]^T

  c = [c1 c2]
  "
  (:require [incanter.core :as i])
  (:require [clojure.tools.logging :as log])
  (:use sr.math))

(defn rowA
  "Generate the 'Type A' row of the matrix M, which looks like
  [ xx' yx' -x -y 0 0 -1 0 ]
  
  for the correspondence point indexed by i.
  
  Matrix M is used in the equation
  [ -x' -y' ]^T = M * [ c1 c2 a11 a12 a21 a22 b1 b2 ]^T"
  [xs ys xs' ys' i]
  (let [
        [x y x' y'] (map (comp #(get % i) vec) [xs ys xs' ys'])
       ]
    [ (* x x')
      (* y x')
      (- x)
      (- y)
      0
      0
      -1
      0
    ])) 

(defn rowB
  "Generate the 'Type B' row of the matrix M, which looks like

  [ xy' yy' 0 0 -x -y 0 -1 ]
  
  for the correspondence point indexed by i.
  
  Matrix M is used in the equation
  [ -x' -y' ]^T = M * [ c1 c2 a11 a12 a21 a22 b1 b2 ]^T"
  [xs ys xs' ys' i]
  (let [
        [x y x' y'] (map (comp #(get % i) vec) [xs ys xs' ys'])
       ]
    [ (* x y')
      (* y y')
      0
      0
      (- x)
      (- y)
      0
      -1
    ])) 

(defn M
  "Generate the matrix M used in the equation

  [ -x' -y' ]^T = M * [ c1 c2 a11 a12 a21 a22 b1 b2 ]^T"
  [xs ys xs' ys']
  {:pre [(= 4 (count xs) (count ys) (count xs') (count ys'))]}
  (let [rowA (partial rowA xs ys xs' ys')
        rowB (partial rowB xs ys xs' ys')
        n (count xs)]
    (i/matrix
      (interleave
        (map rowA (range n))
        (map rowB (range n))))))

(defn b
  "Generate the matrix b used in the equation

  b = M * [ c1 c2 a11 a12 a21 a22 b1 b2 ]^T

  where b is the 8x1 column vector
  b = [-x1' -y1' -x2' -y2' ... ]^T"
  [xs' ys']
  {:pre  [(= (count xs') (count ys'))]
   :post [(= [(+ (count xs') (count ys')) 1] (i/dim %))]}
  (column
    (interleave
      (map - xs')
      (map - ys'))))


(defn solve-for-parameters
  "Solve for the parameters of the projective transformation by solving

  [ -x' -y' ]^T = M * [ c1 c2 a11 a12 a21 a22 b1 b2 ]^T
  
  Returns the matrix parameters [ A b c ]."
  [xs ys xs' ys']
  {:pre [(= 4 (count xs) (count ys) (count xs') (count ys'))]}
  (let [b (b xs' ys')
        _ (log/spy b)
        M (M xs ys xs' ys')
        _ (log/spy M)
        unknowns (i/mmult (i/solve M) b)
        _ (log/spy unknowns)
        [c1 c2 a11 a12 a21 a22 b1 b2] unknowns
        A (i/matrix [[a11 a12] [a21 a22]])
        b (column [b1 b2])
        c (row [c1 c2])]
    [A b c]))
