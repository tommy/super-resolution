(ns sr.projective
  (:require [incanter.core :as i])
  (:require [quil.core :as q])
  (:import processing.core.PImage)
  (:use clojure.tools.logging)
  (:use sr.logging))


(def matricies?
  (comp
    (partial every? i/matrix?)
    vector))

(def m i/matrix)

(defmulti mult matricies?)

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

(defmulti p
  "Return a function that is the projective transformation
  specified by parameters [a, b, c]."
  matricies?)

(defmethod p true
  [a b c]
  (fn [x]
    (let [c (i/trans c)
          n (product-sum a x b)
          d (product-sum c x 1)]
      (i/div n d))))

(defmethod p false
  [a b c]
  (fn [x]
    (let [n (product-sum a x b)
          d (product-sum c x 1)]
      (i/div n d))))

(defn U1D
  [us]
  (i/matrix us))

(defn A1D
  "Generate X matrix in the system of linear equations
  for the 1 dimensional projective transformation."
  [[[u1 x1] [u2 x2] [u3 x3]]]
  (i/matrix
    [[x1 1 (- (* u1 x1))]
     [x2 1 (- (* u2 x2))]
     [x3 1 (- (* u3 x3))]]))


(def U2D U1D)

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
    

(defn printall
  [& args]
  (apply println (interleave args (repeat "\n"))))


(defn make-transformation
  [points]
  (let [xs' (U1D (map (comp first :u) points))
        ys' (U1D (map (comp second :u) points))
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
    (p 1 0 0)))

(defn old-make-transformation
  [points]
  (let [u (U2D (map :x points))
        A (A2D (map #(vector (:x %) (:u %)) points))]
     (println)
     (println "**** u = " u ", A = " A)
     (println)
     (let [
          [a b c] (mult (i/solve A) u)]
       (p a b c))))

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

(defn safe-nth-2
  [mat [ix iy] default]
  (let [m' (safe-nth mat (i/nrow mat) iy default)]
    (if (coll? m')
      (safe-nth m' (i/ncol mat) ix default)
      m')))


(defn one-row
  [img]
  (let [w (.width img)
        px (.pixels img)]
    (take w (vec px))))

(defn one-d-img
  [h row]
  (let [s (* h (count row))]
    (take s (cycle row))))

(defn twice-map-indexed
  [f coll]
  (map-indexed
    (fn [r k]
      (map-indexed
        (partial f r)
        k))
    coll))


(defn transform-1d-vector
  [old p]
  (let [rs (range (count old))
        n (count old)
        newvec
         (map (comp #(safe-nth old n % 0) p) rs)]
    (vec newvec)))

(defn transform-2d-matrix
  "Result is returned as row-major seq."
  [mat p]
  (let [pt-transform
         (as-task-item :trans
           (comp #(safe-nth-2 mat % 0) p i/matrix))
        cols (i/ncol mat)
        rows (i/nrow mat)
        _ (task :trans (* cols rows))]
    (map
      pt-transform
      (for [x (range cols)
            y (range rows)]
        (vector x y)))))

(defn transform-1d
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

(def to-list (comp i/vectorize i/trans))

(defn transform-2d
  [oldimg p]
  (let [w (.width oldimg)
        h (.height oldimg)
        newimg (note (PImage. w h (int 1)))
        oldmat (note (i/matrix (seq (.pixels oldimg)) w))
        newseq (note (transform-2d-matrix oldmat p))
        newpxs (into-array Integer/TYPE newseq)]
    (do
      (prn "Got the transformed img")
      (set! (.pixels newimg) newpxs)
      (.updatePixels newimg)
      ;(.save newimg "transformed-image.png")
      newimg)))

(def transform transform-2d)
