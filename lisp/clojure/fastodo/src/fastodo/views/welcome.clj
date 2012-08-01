(ns fastodo.views.welcome
  (:require [fastodo.views.common :as common]
            [noir.response :as res])
  (:use noir.core
		hiccup.core
		hiccup.page
		hiccup.form))

(defpage "/" []
		 (common/layout
		   [:h1 "hi, welcome"]))
