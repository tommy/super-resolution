(in-ns 'sr.states)

(require '[sr.projective :as p])
(require '[sr.transform :as t])
(require '[sr.logging :as l])

(defn- transform-img
  [data [fname p]]
  {:pre [(ref? data)]
   :post [(not (nil? %))]}
  (log/info (format "== Transforming image %s" fname))
  (let [oldimg (get-image @data fname)
        newimg (t/transform @data oldimg p)]
    {fname newimg}))

(defn- transform-imgs
  "ps is a map from image filename to the projective transformation
  that takes that image to the U image."
  [data ps]
  {:pre [;(= (set (:fnames data))
           ; (conj (set (keys ps))
           ;   (get-in data [:feature-match :primary :fname])))
         (seq ps)
         (every? fn? (vals ps))]}
  (let [f (fn [m p] (l/note (into m (transform-img data p))))]
    (reduce f {} ps)))

(defn- progress-bar
  "Draw the progress bar."
  [id]
  (let [h 20
        total (/ (width) 3)
        x (- (/ (width) 2) (/ total 2))
        y (- (/ (height) 2) (/ h 2))]
    (if-let [percent (l/progress id)]
      ;; if the progress was non-nil,
      ;; draw the progress bar
      (let [prog (* total percent)
            color-total (color 100)
            color-done (color 200)]
        (rect-mode :corner)

        (fill color-total)
        (stroke color-total)
        (rect x y total h)

        (fill color-done)
        (stroke color-done)
        (rect x y prog h))
      ;; if the progress is missing
      ;; draw a red bar
      (let [red (color 100 0 0)]
        (fill red)
        (stroke red)
        (rect x y total h)))))
      

(defmethod step-do :transform
  [data]
  (log/info "== About to transform")
  (data/write @data "data/data-transform-step-do.form")
  (let [ps (p/calculate-transformations @data)]
    (transform-imgs data ps)))

(defmethod done? :transform
  [data]
  (realized? (get-in @data [:step-do :transform])))

(defmethod draw :transform
  [data]
  (let [text-x (- (/ (width) 2) (/ (width) 6))
        text-y (- (/ (height) 2) 15)]
    (do
      (background 10)
      (text-font (create-font "Georgia" 12 true))
      (text "Transforming..." text-x text-y)
      (progress-bar :transformation-progress)
      (checked-step-transition data))))

(def ex (atom nil))

(defmethod click-handle :transform
  [data]
  (let [f (get-in @data [:step-do :transform])]
    (reset! ex {:f f})))
