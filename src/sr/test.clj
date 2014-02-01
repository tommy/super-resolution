(ns sr.test
  (:require [sr.gui :refer [open open-saved]]))

(def oned ["data/1d-1.png" "data/1d-2.png"])
(def twod ["data/small0.jpg" "data/small1.jpg"])
(def hike ["data/hikeU.jpg" "data/hikeB.jpg" "data/hikeC.jpg"])

(def drowzees ["data/drowzee.png" "data/drowzeeB.png"])

(defn ex-2d []
  (open drowzees 2))

(defn ex-1d []
  (open oned 1))

(defn -main [& args]
  (ex-2d))
