(ns sr.logging
  (:use clojure.tools.logging))

(defmacro note
  "Logs the form, evaluates it, and returns the result without logging it.
  
  Modeled after the clojure.tools.logging/spy macro."
  ([expr]
    `(note :debug ~expr))
  ([level expr]
    `(do
       (log ~level '~expr)
       (let [a# ~expr]
         (log ~level (str "Done with " '~expr))
         a#))))
