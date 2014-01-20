(ns sr.test
  (:require [sr.gui :refer [open]]))

(def oned ["data/1d-1.png" "data/1d-2.png"])
(def twod ["data/small0.jpg" "data/small1.jpg"])

(def drowzees ["data/drowzee.png" "data/drowzeeB.png"])

(defn -main [& args]
  (open drowzees 2))
