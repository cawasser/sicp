(defproject sicp "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :aot [sicp.core]
  :main sicp.core
  :source-paths ["src"]
  :plugins [[lein-gorilla "0.4.0"]])
