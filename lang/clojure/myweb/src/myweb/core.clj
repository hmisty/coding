(ns myweb.core
  (:require [org.httpkit.server :as s]))

(defn fib [n]
  (case n 
    0 1
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn handler [req]
  {:status 200
   :header ["Content-Type" "text/plain"]
   :body "Hi!"})

(defn create-server []
  (s/run-server handler {:port 8080}))

(defn stop-server [server]
  (server :timeout 100))
