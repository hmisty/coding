; better not to use - in the ns, e.g. fastodo.views.upload-example 
(ns fastodo.views.uploadexample
  (:require [fastodo.views.common :as common]
            [noir.response :as res])
  (:use noir.core
		hiccup.core
		hiccup.page
		hiccup.form))

(defpage "/upload-example" []
		 (common/layout
		   (form-to {:enctype "multipart/form-data"}
					[:post "/upload"]
					(label :file "File to upload")
					(file-upload :file)
					[:br]
					(submit-button "Upload"))))

(defpage [:post "/upload"] {:keys [file]}
		 (println file)
		 (if-not (= "0" (:size file))
				 (res/redirect "/success")
				 (res/redirect "/fail")))

(defpage "/success" []
		 (common/layout
		   [:p "File upload successful!"]))

(defpage "/fail" []
		 (common/layout
		   [:p "File upload FAIL"]))

