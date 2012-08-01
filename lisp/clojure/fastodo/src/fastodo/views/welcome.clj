(ns fastodo.views.welcome
  (:require [fastodo.views.common :as common]
            [noir.content.getting-started])
  (:use [noir.core :only [defpage]]
		[hiccup.page :only [include-css html5]]))

(defpage "/welcome" []
         (common/layout
           [:p "Welcome to fastodo"]))

(defpage "/my-page" []
		 (common/site-layout
		   [:h1 "This is my first page!"]))

