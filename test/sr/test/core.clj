(ns sr.test.core
  (:use [clojure.test])
  (:require [sr.projective]
            [sr.transform]
            [quil.applet]))

(deftest make-transformation-2d
  (let [p (sr.projective/make-transformation 2 '({:u [43 50], :x [54 81]}
                                                 {:u [53 13], :x [92 35]}
                                                 {:u [48 18], :x [80 40]}
                                                 {:u [21 12], :x [52 11]}))]

    (is [43 50] (p (incanter.core/matrix [54 81])))
    (is [53 13] (p (incanter.core/matrix [92 35])))
    (is [48 18] (p (incanter.core/matrix [80 40])))
    (is [21 12] (p (incanter.core/matrix [52 11])))

    (let [oldimg (.loadImage (quil.applet/applet) "data/drowzeeB.png")
          newimg (sr.transform/transform-2d oldimg p)]
      (is (read-string (slurp "test/data/drowzeeB.png.transformed"))
          (seq (.pixels newimg))))))
