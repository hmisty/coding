(defproject price_corridor "0.1.0-SNAPSHOT"
  :description "charting price corridor of bitcoin"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [clj-time "0.15.2"]
                 [com.hypirion/clj-xchart "0.2.0"]]
  :repl-options {:init-ns price-corridor.core})
