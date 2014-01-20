(ns sr.projective
  (:require [incanter.core :as i])
  (:require [quil.core :as q])
  (:import processing.core.PImage)
  (:require [sr.projective.one :refer [U1D A1D]])
  (:require [sr.projective.two :refer [solve-for-parameters]])
  (:require [clojure.tools.logging :refer [spy]])
  (:require [sr.math :refer [column row product-sum mult]])
  (:require [sr.util :refer [map-vals]]))


(defn p
  "Return a function that is the projective transformation
  specified by parameters [a, b, c].
  
  The structure is independent of whether the parameters
  are scalars or matrices."
  [a b c]
  (fn [x]
    (let [n (product-sum a x b)
          d (product-sum c x 1)]
      (i/div n d))))

(defn projective-identity
  "The identity element of the projective group.
  
  dim should be 1 or 2 (the dimension of the images the transformation
  is defined on)."
  [dim]
  {:pre (#{1 2} dim)}
  (case dim
    1 (p 1 0 0)
    2 (p (i/identity-matrix 2) (column [0 0]) (row [0 0]))))


(defmulti make-transformation
  "Create the projective transformation function.
  
  That is, a function p that transforms coordinates according
  to a projective transformation.
  
  p([x y]) = [x' y']"
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

(defmethod make-transformation 2
  [_ points]
  (let [points (take 4 points)
        _ (spy points)
        ps  (map :u points)
        ps' (map :x points)
        xs  (map first ps)
        ys  (map second ps)
        xs' (map first ps')
        ys' (map second ps')
        _ (spy xs)
        _ (spy ys)
        _ (spy xs')
        _ (spy ys')
        [A b c] (solve-for-parameters xs ys xs' ys')
        _ (spy A)
        _ (spy b)
        _ (spy c)
       ]
    (p A b c)))

(defn calculate-transformations
  "Calculate the projective transformation that transforms each
  feature-matched image to the first image U, and store them
  in the data map."
  [data]
  (let [dim (get-in data [:dimension])
        ;; features is a map from image name to list of features
        features (get-in data [:feature-match :features])
        f (fn [m [k v]]
            (assoc m k (spy (make-transformation dim v))))]
    (spy (reduce f {} features))))
