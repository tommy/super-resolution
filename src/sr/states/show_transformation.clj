(in-ns 'sr.states)

(defn draw-image
  [label x y img]
  (text label x y)
  (set-image x y img))

(defn draw-image-pair
  [label margin x y [img img']]
  (draw-image label x y img)
  (draw-image (str label "'")
              x
              (+ y margin (.height img))
              img'))

(defmethod draw :show-transformation
  [data]
  (let [prim (get-image @data (primary @data))
        trans (:trans @data)
        boths (map #(vector (get-image @data (key %)) (val %)) trans)]
    (assert (not (nil? prim)))
    (assert (not (nil? trans)))
    (assert (seq boths))

    (do
      (background 10)
      (fill 255)
      (text-font (create-font "Georgia" 10 true))
      (text "U" 0 10)
      (set-image 0 10 prim)

      (let [margin 30
            draw-fn (fn [label x y imgs] (draw-image-pair label margin x y imgs))
            labels ["B" "C" "D" "E" "F"]
            xs (range 0 (width) (.width prim))
            ys (repeat (count boths) (+ margin (.height prim)))]
        (dorun (map draw-image-pair labels (repeat margin) xs ys boths))))))

(defmethod click-handle :show-transformation
  [data]
  (save-frame "transformed-###.png")
  (pp/pprint @data))
