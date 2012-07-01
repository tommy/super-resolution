(ns sr.gui
  (:require [sr.projective :as proj])
  (:use quil.core))

(defn transform-img
  [data i p]
  (let [oldimg (nth (:imgs @data) i)]
    ()))

(defn transform-imgs
  [data]
  (let [ps (:transform @(state :step-do))]
    (dosync
      (alter data assoc :trans
        (map (partial transform-img data) ps)))))

(defn by-state
  [& args]
  @(state :step))

(def next-step
  {nil :feature-match
   :feature-match :transform})

(def step-do
  {nil (fn [_] nil)
   :feature-match (fn [_] nil)
   :transform [proj/calculate-transformations
               transform-imgs]})

(defn advance-step
  [data]
  (dosync
    (alter (state :step) next-step)
    (alter (state :step-do)
      assoc @(state :step)
      ((step-do @(state :step)) data))))

(defn set-step
  []
  (set-state! :step (ref (next-step nil))
              :step-do (ref {})))

(defn make-feature-pair
  [data curr i p]
  (let [ps (get-in @data [:features :primary])
        curr-feature (first ps)]
    {:u curr-feature :x p}))

(defn add-primary-feature
  [data i p]
  (dosync (alter data
    update-in [:features :primary] conj p)))

(defn add-secondary-feature
  [data curr i p]
  (dosync (alter data
    update-in [:features curr]
    conj (make-feature-pair data curr i p))))

(defn add-feature
  [data i p]
  (let [curr (:curr @data)]
    (if (zero? curr)
      (add-primary-feature data i p)
      (add-secondary-feature data curr i p))))
    

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

(defn clicked-img
  [ps x y]
  (let [is (filter (partial img-within? x y) ps)]
    (first is)))

(defn num-features
  [data]
  (get-in @data [:features :n]))
  ;(reduce + (map count (vals (:features @data)))))

(defn count-imgs
  [data]
  (count (:imgs @data)))

(defn inc-curr
  [data]
  (let [num-imgs (count-imgs data)
        inc-mod #(mod (inc %) num-imgs)]
    (dosync
      (alter data
        update-in [:curr] inc-mod))))

(defn checked-inc-feature
  [data]
  (when (= 0 (:curr @data))
    (dosync (alter data update-in [:features :n] inc))))

(defn current-image
  [data]
  (let [data @data]
    (nth (:imgs data) (:curr data))))

;; SETUP

(defn load-imgs
  [data]
  (let [fnames (:fnames @data)
        imgs (map load-image fnames)]
      (dosync
        (alter data assoc :imgs imgs))))

;(defn set-maxh
;  [data]
;  (let [imgs (:imgs @data)
;        maxh (apply max (map #(.height %) imgs))]
;    (dosync
;      (alter data assoc :maxh maxh))))

(defn setup
  [data]
  (do
    (load-imgs data)
    (dosync (alter data assoc :curr 0))
    (dosync (alter data assoc :features {:n 1}))
    ;(dosync (alter data assoc :pos {}))
    (set-step)
    (prn @data)))


;; DRAWING

(defn paint-img
  [data]
  (set-image 0 0 (current-image data)))

;(defn paint-img
;  [data maxh current i img]
;  (do 
;    (let [y (+ 10 (* (+ 20 maxh) i))
;          w (.width img)
;          h (.height img)]
;      (fill (color 0 255 0))
;      (when (= current i)
;        (rect 0 (- y 4) (+ w 8) (+ 8 h)))
;      (set-image 0 y img)
;      (dosync
;        (alter data update-in [:pos] assoc img [0 y w h]))
;  )))

(defmulti draw by-state)

(defmethod draw :feature-match
  [data]
  (do
    (background 10)
    (paint-img data)))

(defmethod draw :transform
  [data]
  (do
    (background 10)))


;; CLICK HANDLING

(defn checked-step-transition
  [data]
  (when (< 3 (num-features data))
    (advance-step data)
    (println "New state is: " @(state :step))))

(defmulti click-handle by-state)

(defmethod click-handle :feature-match
  [data]
  (do
    (let [x (mouse-x)
          y (mouse-y)
          n (num-features data)]
      (add-feature data n x) ; only 1D feature for now
      (inc-curr data)
      (checked-inc-feature data)
      (checked-step-transition data)
      (prn @data)
      )))

(defmethod click-handle :default
  [data]
  nil)

(comment
 {:fnames ["a.png"]
  :imgs [<PImage>]
  :pos {<PImage> [x y w h]}
  :features {1 [i11 i12], 2 [i21 i22]}
  :curr 1
  }
)

(defn open
  [imgs]
  (let [data (ref {:fnames imgs})]
    (defsketch feature-matching
      :title "SR"
      :setup (partial setup data)
      :draw (partial draw data)
      :mouse-clicked (partial click-handle data)
      :size [300 300])))
