(ns sr.feature
  (:use sr.projective)
  (:use sr.data))

;; Functions on the data object

(defn num-features
  [data]
  (get-in @data [:features :n]))

(defn current-image
  [data]
  (let [data @data]
    (nth (:imgs data) (:curr data))))


(defn inc-curr
  [data]
  (let [num-imgs (count-imgs data)
        inc-mod #(mod (inc %) num-imgs)]
    (change data [:curr] inc-mod)))

(defn checked-inc-feature
  [data]
  (when (= 0 (:curr @data))
    (change data [:features :n] inc)))

(defn make-feature-pair
  [data curr i p]
  (let [ps (get-in @data [:features :primary])
        curr-feature (first ps)]
    {:u curr-feature :x p}))

(defn add-primary-feature
  [data i p]
  (change data [:features :primary] conj p))

(defn add-secondary-feature
  [data curr i p]
  (change data
    [:features curr]
    conj (make-feature-pair data curr i p)))

(defn add-feature
  [data i p]
  (let [curr (:curr @data)]
    (if (zero? curr)
      (add-primary-feature data i p)
      (add-secondary-feature data curr i p))))
