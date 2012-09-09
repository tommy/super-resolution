(in-ns 'sr.states)

(require '[sr.projective :as proj])

(defn transform-img
  [data [fname p]]
  (let [oldimg (get-image @data fname)
        newimg (proj/transform @data oldimg p)]
    {fname newimg}))

(defn transform-imgs
  [data ps]
  {:pre [;(= (set (:fnames data))
           ; (conj (set (keys ps))
           ;   (get-in data [:feature-match :primary :fname])))
         (every? fn? (vals ps))]}
  (let [f (fn [m p] (note (into m (transform-img data p))))]
    (make data [:trans]
      (reduce f {} ps))))

(defn progress-bar
  [id]
  (let [h 20
        total (/ (width) 3)
        x (- (/ (width) 2) (/ total 2))
        y (- (/ (height) 2 (/ h 2)))]
    (if-let [percent (progress id)]
      ;; if the progress was non-nil,
      ;; draw the progress bar
      (do
        (let [prog (* total percent)
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
      ;; if the progress is nil (missing)
      (let [red (color 100 0 0)]
        (fill red)
        (stroke red)
        (rect x y total h)))))
      

(defmethod step-do :transform
  [data]
  (prn "about to transform")
  (transform-imgs data
    (proj/calculate-transformations @data)))

(defmethod done? :transform
  [data]
  (realized? (:transform (get-in @data [:step-do]))))


(defmethod draw :transform
  [data]
  (do
    (background 10)
    (text-font (create-font "Georgia" 10 true))
    (text "Transforming..." 0 (/ (height) 3))
    (progress-bar :trans)
    (checked-step-transition data)))

(def ex (atom nil))

(defmethod click-handle :transform
  [data]
  (let [f (get-in @data [:step-do :transform])]
    (reset! ex {:f f})))
