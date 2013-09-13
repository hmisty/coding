(ns echo
  (:use socketio))

(defn -main
  []
  (let [server (create 8089)
        clients (atom #{})]
    (on-connect server (fn [client] (swap! clients conj client)))
    (on-disconnect server (fn [client] (swap! clients disj client)))
    (on-message server (fn [c])) 
    (start server)))
