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

(defn -main [& args]
  (open twod 2))
