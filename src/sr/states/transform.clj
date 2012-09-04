(in-ns 'sr.states)

(use 'sr.image)
(require '[sr.projective :as proj])

(defn transform-img
  [data [fname p]]
  (let [oldimg (get-image data fname)
        newimg (proj/transform oldimg p)]
    {fname newimg}))

(defn transform-imgs
  [data ps]
  {:pre [(every? fn? ps)]}
  (let [f (fn [m p] (note (into m (transform-img data p))))]
    (make data [:trans]
      (reduce f {} ps))))

(defn progress-bar
  [id]
  (let [h 20
        y (- (/ (height) 2) (/ h 2))
        total (/ (width) 3)
        prog (do (prn "id " id) (* total (progress id)))
        x (- (/ (width) 2) (/ total 2))
        color-total (color 100)
        color-done (color 200)]
    (rect-mode :corner)

    (fill color-total)
    (stroke color-total)
    (rect x y total h)

    (fill color-done)
    (stroke color-done)
    (rect x y prog h)))

(defmethod step-do :transform
  [data]
  (prn "about to transform")
  (transform-imgs data
    (proj/calculate-transformations @data)))

(defmethod done? :transform
  [data]
  (realized? (:transform (state :step-do))))


(defmethod draw :transform
  [data]
  (do
    (background 10)
    (text-font (create-font "Georgia" 10 true))
    (text "Transforming..." 0 (/ (height) 3))
    (progress-bar :trans)))

