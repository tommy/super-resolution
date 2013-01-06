(ns sr.projective.two
  (:require [incanter.core :as i])
  (:require [clojure.tools.logging :as log])
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

(defn to-homogenous-coordinates
  [[x y]]
  {:pre [(number? x) (number? y)]}
  [x y 1])

(defn from-homogenous-coordinates
  [[x y z]]
  {:pre [(< (Math/abs (- 1 z)) 0.1)]}
  [x y])

(defn homography
  "Solve for the projective transformation M where:

  [x']       [x]
  [y'] = M . [y]
  [z']       [z]

  where p' = (x',y',z') and p = (x,y,z)
  for p' in ps' and p in ps.

  Thus, the matrix M is the following product:

  [x1' x2' x3' x4' x5' x6' x7' x8' x9']   [x1 x2 x3 x4 x5 x6 x7 x8 x9]^-1
  [y1' y2' y3' y4' y5' y6' y7' y8' y9'] . [y1 y2 y3 y4 y5 y6 y7 y8 y9]      = M
  [z1' z2' z3' z4' z5' z6' z7' z8' z9']   [z1 z2 z3 z4 z5 z6 z7 z8 z9]
  "
  [ps' ps]
  {:post [(i/matrix? %) (do (prn %) true)]}
  (try
  (let [ps' (map to-homogenous-coordinates ps')
        ps (map to-homogenous-coordinates ps)
        P' (apply i/bind-columns ps')
        _ (log/spy P')
        P (apply i/bind-rows ps)
        _ (log/spy P)
        _ (log/spy (i/rank P))
        Pinv (i/solve P)
        _ (log/spy Pinv)
        M (i/mmult P' (i/trans Pinv))
        _ (log/spy M)
        ]
    ;[ps' ps P' P Pinv M]
    M)
  (catch Exception e (.printStackTrace e))))


(defn homography-matrix-as-fn
  [M]
  (fn
    [[x y z :as p]]
      (if (nil? z)
        ((comp
           from-homogenous-coordinates
           (partial i/mmult M)
           to-homogenous-coordinates)
          p)
        ; if the third coordinate is already there
        (i/mmult M (column p)))))
