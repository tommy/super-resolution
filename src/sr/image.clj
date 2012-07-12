(ns sr.image
  (:use quil.core))

(defn get-image
  [data fname]
  ((:imgs @data) fname))

(defn within?
  [x' y' [x y w h]]
  (and
    (<= x x' (+ x w))
    (<= y y' (+ y h))))

(defn img-within?
  [x' y' pos]
  (if (within? x' y' (val pos))
    pos
    false))

