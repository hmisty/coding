(ns udpecho.core
  (:gen-class)
  (:import (java.net DatagramSocket DatagramPacket InetAddress
            SocketTimeoutException)))

(def ADDRESS (InetAddress/getByName "127.0.0.1"))
(def PORT 8000)
(def PACKET_SIZE 1024)

(def active (atom true))

(defn start-server [] 
  (println "server started.")
  (let [server-socket (DatagramSocket. PORT)
        packet (DatagramPacket. (byte-array PACKET_SIZE) PACKET_SIZE)]
    (loop []
      (when @active
        (.receive server-socket packet)
        (println (String. (.getData packet) 0 (.getLength packet)))
        ;; send it right back
        (.send server-socket packet)
        (recur)))))

(defn start-receiver [socket]
  (println "receiver started.")
  (let [packet (DatagramPacket. (byte-array PACKET_SIZE) PACKET_SIZE)]
    (loop []
      (when @active
        (.receive socket packet)
        (println (String. (.getData packet) 0 (.getLength packet)))
        (recur)))))

(defn start-client [] 
  (println "client started.")
  (let [client-socket (DatagramSocket.)
        packet (DatagramPacket. (byte-array PACKET_SIZE) PACKET_SIZE ADDRESS PORT)]
    (future (start-receiver client-socket))
    (loop []
      (when @active
        (let [input (read-line)]
          (.setData packet (.getBytes input) 0 (.length input))
          (.send client-socket packet))
        (recur)))))

(defn -main [& args]
  (if (= (first args) "-s") (start-server) (start-client)))
