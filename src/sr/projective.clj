(ns sr.projective
  (:require [incanter.core :as i]))

(def m i/matrix)

(defmulti mult
   (fn [& args]
   (every? i/matrix? args)))

(defmethod mult true
  [& args]
  (apply i/mmult args))

(defmethod mult false
  [& args]
  (apply i/mult args))

(defn- product-sum
  [a b c]
  (i/plus
    (mult a b)
    c))

(defn p
  "Return a function that is the projective transformation
  specified by parameters [a, b, c]."
  [a b c]
  (fn [x]
    (let [n (product-sum a x b)
          d (product-sum c x 1)]
      (i/div n d))))

(defn U1D
  [u1 u2 u3]
  (i/matrix [u1 u2 u3]))

(defn A1D
  "Generate X matrix in the system of linear equations
  for the 1 dimensional projective transformation."
  [[u1 x1] [u2 x2] [u3 x3]]
  (i/matrix
    [[x1 1 (- (* u1 x1))]
     [x2 1 (- (* u2 x2))]
     [x3 1 (- (* u3 x3))]]))

(defn make-transformation
  [points]
  (let [u (apply U1D (map :u points))
        A (apply A1D (map #(vector (:u %) (:x %)) points))
        [a b c] (mult (i/solve A) u)]
    (p a b c)))

(defn calculate-transformations
  [data]
  (let [features (filter #(some (set [(first %)]) (range 10)) (:features @data))
        f (fn [m [k v]]
            (assoc m k (make-transformation v)))]
    (reduce f {} features)))
