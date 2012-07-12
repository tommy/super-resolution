(defproject sr "1.0.0-SNAPSHOT"
  :repl-init sr.test
  :description "Super-resolution image construction"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [incanter "1.3.0"
                    :exclusions
                        [incanter/incanter-processing]]
                 [quil "1.4.0"]]
  :dev-dependencies [[midje "1.3-alpha4"]
                     [clj-stacktrace "0.2.4"]])
