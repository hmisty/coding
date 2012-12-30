;; borrowed from https://github.com/alienscience/compojure-war-example
;; nevertheless, it's still not an elegant way for handling the context path

(ns shorturl.context
  "Functions to handle servlet context")

;; This caches the context path of the servlet
(def context-path (atom nil))

(defn- get-context-path
  "Returns the context path when running as a servlet"
  ([] @context-path)
  ([servlet-req]
     (if (nil? @context-path)
       (reset! context-path (.getContextPath servlet-req)))
     @context-path))

(defn wrap-context
  "Removes the deployed servlet context from a URI when running as a
   deployed web application"
  [handler]
  (fn [request]
    (if-let [servlet-req (:servlet-request request)]
      (let [context (get-context-path servlet-req)
            uri (:uri request)]
        (if (.startsWith uri context)
          (handler (assoc request :uri
                          (.substring uri (.length context))))
          (handler request)))
      (handler request))))

(defn link
  "Builds a link, including context path, out of the given url"
  [url]
  (str (get-context-path) url))
