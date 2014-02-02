(in-ns 'sr.states)

(defn- draw-image
  [label x y img]
  (text label x y)
  (set-image x y img))

(defn- draw-image-pair
  [label margin x y [img img']]
  (draw-image label x y img)
  (draw-image (str label "'")
              x
              (+ y margin (.height img))
              img'))

(defmethod draw :show-transformation
  [data]
  (let [prim (get-image @data (primary @data))
        trans @(get-in @data [:step-do :transform])
        boths (map #(vector (get-image @data (key %)) (val %)) trans)]
    (assert (not (nil? prim)))
    (assert (not (nil? trans)))
    (assert (seq boths))

    (do
      (background 10)
      (fill 255)
      (text-font (create-font "Georgia" 10 true))

      (draw-image "U" 0 10 prim)

      (let [margin 30
            draw-fn (fn [label x y imgs] (draw-image-pair label margin x y imgs))
            labels ["B" "C" "D" "E" "F"]
            xs (range 0 (width) (.width prim))
            ys (repeat (count boths) (+ margin (.height prim)))]
        (dorun (map draw-image-pair labels (repeat margin) xs ys boths))))))

(defmethod click-handle :show-transformation
  [data]
  (checked-step-transition data))

(defmethod done? :show-transformation
  [data]
  true)

(defmethod key-typed :show-transformation
  [data]
  (let [k (raw-key)]
    (case k
      \s (do (log/info "Saving frame.") (save-frame "transformed-###.png"))
      \w (do (log/info "Writing state.") (data/write @data "data/data-show-on-click.form"))
      nil)))
