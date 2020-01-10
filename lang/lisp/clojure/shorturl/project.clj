(defproject shorturl "1.0.0"
  :description "Short URL demo app"
  :url "http://github.com/hmisty/coding/lisp/clojure/shorturl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [ring "1.0.1"]
                 [compojure "1.0.1"]
                 [enlive "1.0.0"]]
;;  :aot [shorturl.server]
  :main shorturl.server
  :plugins [[lein-ring "0.7.5"]]
  :ring {:handler shorturl.core/app})
