(ns sr.logging
  (:require [clojure.tools.logging :refer [log debug]]
            [clojure.pprint :as pp]))

;; MISCELLANEOUS

(defn printall
  [& args]
  (apply println (interleave args (repeat "\n"))))


;; EXECUTION TIMING

(defonce timer-stats (atom {}))

(defn update-avg
  "Update the running average for a data set.

  Input should be a map in the format
  {:avg 4.5, :n 6}"
  [{:keys [avg n] :as old} datapoint]
  (if old
    {:n (inc n)
     :avg (/ (+ (* n avg) datapoint) (inc n))}
    {:n 1 :avg datapoint}))

(defn record-time
  "Given the execution time of a given form,
  update the average in the timer-stats map
  keyed by the form."
  [form msecs]
  (swap! timer-stats update-in [form] update-avg msecs))

(defmacro note
  "Logs the form, evaluates it, and returns the result without logging it.
  
  Modeled after the clojure.tools.logging/spy macro."
  ([expr]
    `(note :debug ~expr))
  ([level expr]
    `(do
       (log ~level '~expr)
       (let [start# (. System (nanoTime))
             ret# ~expr
             msecs# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
         (log ~level (str "Done with " '~expr " in " msecs# " msecs"))
         (record-time '~expr msecs#)
         ret#))))


;; PROGRESS TRACKING

(defonce ^:private tasks (ref {}))
(add-watch tasks :printer (fn [_ _ old new]
                            (debug "== Creating task.")
                            (pp/pprint new)))

(defn- tget
  [id]
  (get @tasks id (ref nil)))

(defn task
  "Create a new task. If a max value is specified, the task is expected
  to be incremental."
  ([id]
   (let [a (atom {:percent 0.0})]
     ;(add-watch a :printer (fn [_ _ _ n] (prn "== TASK " id) (pp/pprint n)))
     (dosync
       (alter tasks assoc id a))))
  ([id max]
   (let [a (atom {:max max :now 0})]
     ;(add-watch a :printer (fn [_ _ _ n] (prn "== TASK " id) (pp/pprint n)))
     (dosync
       (alter tasks assoc id a)))))

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

(defn as-task-item
  "Given a function f and the id of a task being tracked,
  return a function that behaves the same as f but increments
  the :now value of the task when it is executed."
  [id f]
  (fn [& args]
    (task-inc id)
    (apply f args)))
