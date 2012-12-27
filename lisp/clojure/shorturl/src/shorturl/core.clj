(ns shorturl.core
  (:use [clojure.pprint]
        [compojure.core :only (GET POST defroutes)])
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.response :as resp]
            [net.cgrand.enlive-html :as en]
             [compojure.handler]
             [compojure.route]))

;; the counter algorithm
(defonce counter (atom 10000))

(defonce urls (atom {}))

(defn shorten [url]
  (let [id (swap! counter inc)
        id (Long/toString id 36)]
    (swap! urls assoc id url)
    id))

;; the handlers
#_(defn homepage [request]
  (str @urls))

(en/deftemplate homepage
  (en/xml-resource "homepage.html")
  [request]
  [:#listing :li] (en/clone-for [[id url] @urls]
                                [:a] (comp
                                        (en/content (format "%s : %s" id url))
                                        (en/set-attr :href (str \/ id)))))

(defn redirect [id]
  (resp/redirect (@urls id)))

;; the web app
#_(defn app [request]
  {:status 200
   :body (with-out-str
           (pprint request))})

(defroutes app*
  (compojure.route/resources "/")
  (GET "/" request (homepage request))
  (GET "/:id" [id] (redirect id))
  (POST "/shorten" request
        (let [id (shorten (-> request :params :url))]
          (resp/redirect "/"))))

;; the middleware
(def app (compojure.handler/site app* ))

;; the jetty server
(defonce server (jetty/run-jetty #'app { :port 8081 :join? false}))
