(ns shorturl.core
  (:use [clojure.pprint]
        [compojure.core :only (GET POST defroutes)])
  (:require [ring.util.response :as resp]
            [net.cgrand.enlive-html :as en]
            [compojure.handler]
            [compojure.route]
            [shorturl.context :as ctx]))

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
  [:#listing :li]
  (en/clone-for [[id url] @urls]
                [:a] (comp
                       (en/content (format "%s : %s" id url))
                       (en/set-attr :href (ctx/link (str \/ id))))))

(defn perft []
  "ok")

(defn redirect [id]
  (resp/redirect (@urls id)))

;; the handlers
#_(defn handler [request]
  {:status 200
   :body (with-out-str
           (pprint request))})

(defroutes handler
  (compojure.route/resources "/")
  (GET "/" request (homepage request))
  (GET "/t" request (perft))
  (GET "/:id" [id] (redirect id))
  (POST "/shorten" request
        (let [id (shorten (-> request :params :url))]
          (resp/redirect (ctx/link "/"))))
  ;; default route for avoiding: handler returns nil
  (GET "*" request 
       {:status 200 :body (with-out-str (pprint request))})
)

;; the web app by adding middleware
(def app
  (-> handler
    (compojure.handler/site)
    (ctx/wrap-context)))
