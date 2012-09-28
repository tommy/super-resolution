(in-ns 'sr.states)

(defmethod draw :show-transformation
  [data]
  (let [prim (get-image @data (primary @data))
        trans (:trans @data)
        boths (map #(vector (get-image @data (key %)) (val %)) trans)
        img-b (ffirst boths)
        img-b' (second (first boths))]
    (assert (not (nil? prim)))
    (assert (not (nil? trans)))
    (assert (not (nil? img-b)))
    (assert (not (nil? img-b')))
    (do
      (background 10)
      (fill 255)
      (text-font (create-font "Georgia" 10 true))
      (text "U" 0 10)
      (set-image 0 10 prim)

      (let [y (+ 30 (.height prim))]
        (text "B" 0 y)
        (set-image 0 y img-b)
        (let [y' (+ 30 y (.height img-b))]
          (text "B'" 0 y')
          (set-image 0 y' img-b')))

      )))

(defmethod click-handle :show-transformation
  [data]
  (save-frame)
  (prn @data))
