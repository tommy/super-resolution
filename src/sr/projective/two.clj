(ns sr.projective.two
  (:require [incanter.core :as i])
  (:use sr.math))

(defn ROW2D
  [[[ux uy] [xx xy]]]
  (vector xx xy 1 ux ux))

(defn A2D
  [pairs]
  (i/matrix
    (map ROW2D pairs)))

(defn ROW2DX
  [[[ux uy] [xx xy]]]
  (vector xx xy 1 (- (* xx ux)) (- (* xy ux))))

(defn ROW2DY
  [[[ux uy] [xx xy]]]
  (vector xx xy 1 (- (* xx uy)) (- (* xy uy))))

(def A2DX
  (comp i/matrix (partial map ROW2DX)))

(def A2DY
  (comp i/matrix (partial map ROW2DY)))

(defn BY
  [c1 c2 pairs]
  (let [f (fn [[[ux uy] [xx xy]]]
            (+
              uy
              (* c1 xx uy)
              (* c2 xy uy)))]
    (i/matrix
      (map f pairs))))
    

