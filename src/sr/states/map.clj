(in-ns 'sr.states)

(defmethod draw :map
  [data]
  (background 10)
  (text-font (create-font "Georgia" 24 true))
  (text "Maximum A Posteriori optimization goes here." 20 20))
