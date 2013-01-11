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

(def drowzees ["data/drowzee.png" "data/drowzeeB.png"])

(comment
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
)

(defn -main [& args]
  (open drowzees 2))
