(in-ns 'sr.states)

(use 'sr.feature)

(defn paint-img
  "Feature-matching step.
  
  Paint the current PImage to the screen with its origin at (0,0)."
  [data]
  (set-image 0 0 (current-image data)))


(defmethod done? :feature-match
  [data]
  (feature-matching-done? @data))

(defmethod draw :feature-match
  [data]
  (dosync
    (background 10)
    (when-not (done? data)
      (paint-img @data))))

(defmethod click-handle :feature-match
  [data]
  (let [x (mouse-x)
        y (mouse-y)]
    (add-feature data [x y])
    (drop-curr data)
    (checked-step-transition data)
    (prn @data)
    ))
