;; package to a standalone jar
;; run as a standalone server

(ns shorturl.server
  (:gen-class)
  (:use [shorturl.core :only (app)] 
        [ring.middleware file-info file])
  (:require [ring.adapter.jetty :as jetty]))

#_(defn handler []
  (-> (var app)                    
    (wrap-file "resources")
    (wrap-file-info)))

(defn -main [& [port]]
  (let [port    (if port (Integer/parseInt port) 8081)]    
    (jetty/run-jetty #'app {:join? false :port port})))
