(ns sr.test
  (:use sr.gui)
  (:use sr.projective)
  (:require [incanter.core :as i]))

(def img1a "data/1d-1.png")
(def img1b "data/1d-2.png")

(def img2a "data/small0.jpg")
(def img2b "data/small1.jpg")

(def fnames [img1a img1b])

(def twod [img2a img2b])

(def features
 '({:u [23 37], :x [8 35]} {:u [96 55], :x [86 57]} {:u [108 14], :x [97 17]} {:u [33 26], :x [21 26]} {:u [43 7], :x [32 6]} {:u [78 74], :x [65 75]} {:u [70 10], :x [87 8]} {:u [72 9], :x [84 7]}))

(def features-large-size
 '({:u [449 176], :x [390 178]}
   {:u [138 98], :x [84 99]}
   {:u [85 218], :x [31 223]}
   {:u [152 261], :x [99 265]}
   {:u [323 292], :x [271 295]}
   {:u [394 176], :x [342 176]}
   {:u [282 33], :x [233 36]}
   {:u [209 66], :x [159 66]}))

(def pairs
  (map (juxt :u :x) features))

(def uxs
  (map (comp first :u) features))
(def uys
  (map (comp second :u) features))
(def xxs
  (map (comp first :x) features))
(def xys
  (map (comp second :x) features))

(defn xrow8
  [[[ux uy] [xx xy]]]
  [xx xy 0 0 1 0 (- (* xx ux)) (- (* xy ux))])

(defn yrow8
  [[[ux uy] [xx xy]]]
  [0 0 xx xy 0 1 (- (* xx uy)) (- (* xy uy))])

(defn xrow5
  [pair]
  (i/sel (i/trans (i/matrix (xrow8 pair))) :cols [0 1 4 6 7]))

(defn yrow5
  [pair]
  (i/sel (i/trans (i/matrix (xrow8 pair))) :cols [2 3 5 6 7]))

(def AX8
  (i/matrix (map xrow8 pairs)))

(def AY8
  (i/matrix (map yrow8 pairs)))

(def AX5
  (i/matrix (map xrow5 (take 5 pairs))))

(def AY5
  (i/matrix (map yrow5 (take 5 pairs))))

(defn uy3fn
  [c1 c2 [[ux uy] [xx xy]]]
  (+
    uy
    (* c1 xx uy)
    (* c2 xy uy)))

(defn UY3
  [c1 c2]
  (map (partial uy3fn c1 c2) (drop 5 pairs)))

(defn ay3row
  [[[ux uy] [xx xy]]]
  [xx xy 1])

(def AY3
  (map ay3row (drop 5 pairs)))


(def solved-x5
   (i/mmult (i/solve AX5) (take 5 uxs)))
(def solved-y3
  (i/mmult (i/solve (i/matrix AY3)) (let [[a11 a12 b1 c1 c2] solved-x5] (i/matrix (UY3 c1 c2)))))

(def the-p
  (let [[a11 a12 b1 c1 c2] solved-x5
        [a21 a22 b2] solved-y3
        A (i/matrix [[a11 a12] [a21 a22]])
        b (i/matrix [b1 b2])
        c (i/matrix [c1 c2])]
    (p A b c)))

