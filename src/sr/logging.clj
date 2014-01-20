(ns sr.logging
  (:require [clojure.tools.logging :refer [log]]))

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

(defn printall
  [& args]
  (apply println (interleave args (repeat "\n"))))


(defonce tasks (ref {}))

(defn- tget
  [id]
  (get @tasks id (ref nil)))

(defn task
  "Create a new task. If a max value is specified, the task is expected
  to be incremental."
  ([id]
  (dosync
    (alter tasks assoc id (atom {:percent 0.0}))))
  ([id max]
  (dosync
    (alter tasks assoc id (atom {:max max :now 0})))))

(defn task-set
  "Sets the current value of the task keyed by id.

  If v is a float between 0 and 1, it is taken as the percentage completion.
  Otherwise, v is number of completed steps towards the max. (i.e., v/max.)"
  [id v]
  (let [a (tget id)]
    (if (float? v)
      (reset! a {:percent v})
      (swap! a assoc :now v))))

(defn task-inc
  "Increments the current number of completed steps towards the max."
  [id]
  (let [a (tget id)]
    (swap! a update-in [:now] inc)))
  

(defn- percentage
  [v]
  (or
    (:percent v)
    (/ (:now v) (:max v))))

(defn progress
  "Returns the percentage completion of the progress indicator keyed by k.
  If the key is not found, returns nil."
  [id]
  (when-let [v @(tget id)]
    (float (percentage v))))

(defmacro task-item-macro
  "Executes the body and then increments the item count in the task keyed
  by id.

  Example:

  (task :something 50)
  (doseq [i (range 50)]
    (task-item :something
      (some-operation i)))"

  [id expr]
  `(do
     (let [a# ~expr]
       (task-inc ~id)
       a#)))

(defn as-task-item
  [id f]
  (fn [& args]
    (task-inc id)
    (apply f args)))
