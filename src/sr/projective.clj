(ns sr.projective
  (:require [incanter.core :as i])
  (:require [quil.core :as q]))

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
  (let [u (apply U1D (map :x points))
        A (apply A1D (map #(vector (:x %) (:u %)) points))
        [a b c] (mult (i/solve A) u)]
    (p a b c)))

(defn spy
  [v]
  (do
    (println "***** " v)
    v))

(defn calculate-transformations
  [data]
  (let [features (get-in @data [:feature-match :features])
        f (fn [m [k v]]
            (assoc m k (make-transformation v)))]
    (spy (reduce f {} features))))


(defn safe-nth
  [coll n i default]
  (if
    (<= 0 i (dec n)) (nth coll i)
    default))

(defn one-row
  [img]
  (let [w (.width img)
        px (.pixels img)]
    (take w (vec px))))

(defn one-d-img
  [h row]
  (let [s (* h (count row))]
    (take s (cycle row))))


(defn transform-1d-vector
  [old p]
  (let [rs (range (count old))
        n (count old)
        newvec
         (map (comp #(safe-nth old n % 0) p) rs)]
    (vec newvec)))

(defn transform
  [oldimg p]
  (let [w (.width oldimg)
        h (.height oldimg)
        newimg (q/create-image w h (int 1))
        oldvec (one-row oldimg)
        newvec (one-d-img h (transform-1d-vector oldvec p))]
    (do
      (set! (.pixels newimg) (into-array Integer/TYPE newvec))
      (.updatePixels newimg)
      newimg)
        ))
