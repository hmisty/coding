;; package to a war
;; deploy as a servlet
;; for maven only. lein ring uberwar will generate a servlet.clj automatically.

(ns shorturl.servletx
  #_(:import [javax.servlet.http HttpServletRequest HttpServletResponse])
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use [ring.util.servlet :only (defservice)])
  (:require [shorturl.core]))

;; the servlet
(defservice shorturl.core/app)
