(defproject sr "1.0.0-SNAPSHOT"
  :main sr.test
  :repl-init sr.test
  :description "Super-resolution image construction"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [incanter "1.3.0-SNAPSHOT"
                    :exclusions
                        [incanter/incanter-processing]]
                 [quil "1.4.0"]]
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"])
