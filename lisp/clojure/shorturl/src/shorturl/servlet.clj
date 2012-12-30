;; package to a war
;; deploy as a servlet

(ns shorturl.servlet
  (:import [javax.servlet.http HttpServletRequest HttpServletResponse])
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use [ring.util.servlet :only (defservice servlet)]
        #_[ring.middleware.params :only (wrap-params)])
  (:require [shorturl.core]))

;; the servlet
(defservice shorturl.core/app)
