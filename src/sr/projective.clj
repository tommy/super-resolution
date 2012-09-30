(ns sr.projective
  (:require [incanter.core :as i])
  (:require [quil.core :as q])
  (:import processing.core.PImage)
  (:use sr.projective.one)
  (:use sr.projective.two)
  (:use clojure.tools.logging)
  (:use sr.logging)
  (:use sr.math)
  (:use sr.util))

(defmulti p
  "Return a function that is the projective transformation
  specified by parameters [a, b, c]."
  matricies?)

(defmethod p :matrix
  [a b c]
  (fn [x]
    (let [c (i/trans c)
          n (product-sum a x b)
          d (product-sum c x 1)]
      (i/div n d))))

(defmethod p :scalar ; should this include :matrix-and-scalar?
  [a b c]
  (fn [x]
    (let [n (product-sum a x b)
          d (product-sum c x 1)]
      (i/div n d))))


(defmulti make-transformation
  "Create the projective transformation function."
  (fn [dimension points]
    dimension))

(defmethod make-transformation 1 ;; make transformation for 1-D
  [_ points]
  {:pre [(seq? points)]}
  (let [points (map (partial map-vals first) points)
        u (U1D (map :x points))
        A (A1D (map #(vector (:x %) (:u %)) points))]
     (println)
     (println "**** u = " u ", A = " A)
     (println)
     (let [[a b c] (mult (i/solve A) u)]
       (p a b c))))

(defmethod make-transformation 2 ;; make-transformation for 2-D
  [_ points]
  (let [xs' (column (map (comp first :u) points))
        ys' (column (map (comp second :u) points))
        Ax (A2DX (map (juxt :x :u) points))
        Ay (A2DY (map (juxt :x :u) points))
        [a21 a22 b2 c1 c2] (mult (i/solve Ax) xs')
        bb (BY c1 c2 (map (juxt :x :u) points))
        X (i/matrix (map (comp (juxt first second (constantly 1)) :x) points))]
    (println
      "xs=" xs'
      "ys=" ys'
      "Ax=" Ax
      "Ay=" Ay
      "c1=" c1
      "c2=" c2
      "bb=" bb
      "X=" X)
    (p
      (i/identity-matrix 2)
      (i/matrix [0 0])
      (i/matrix [0 0]))))

(defn calculate-transformations
  [data]
  (let [dim (get-in data [:dimension])
        features (get-in data [:feature-match :features])
        f (fn [m [k v]]
            (assoc m k (spy (make-transformation dim v))))
        test (fn [m [k v]] (assoc m k (constantly k)))]
    (spy (reduce f {} features))))



