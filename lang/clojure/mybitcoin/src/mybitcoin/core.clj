(ns mybitcoin.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.net Socket]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol helper functions
;; reference: https://en.bitcoin.it/wiki/Protocol_documentation
(defn print-hex
  "Displays the byte-list in lowercase HEX string format."
  [byte-list] 
  (apply str (map (fn [byte] (format "%02x" byte)) byte-list)))

(defn byte2<-int16_t 
  "Converts an int16_t number [n] to 2 bytes."
  [n]
  (map (fn [bits] (bit-and 0xff (bit-shift-right n bits))) [0 8]))

(defn byte4<-int32_t 
  "Converts an int32_t number [n] to 4 bytes."
  [n]
  (map (fn [bits] (bit-and 0xff (bit-shift-right n bits))) [0 8 16 24]))

(defn byte8<-uint64_t
  "Converts an uint64_t number [high low] to 8 bytes."
  [high low]
  (concat (byte4<-int32_t low) (byte4<-int32_t high)))

(defn byte8<-int64_t
  "Converts an int64_t number [n] to 8 bytes."
  [n]
  (concat (byte4<-int32_t n) (byte4<-int32_t (bit-shift-right n 32))))

(defn bytes<-var_str
  "Converts a string [s] to n bytes (n is the first byte)."
  [s]
  (let [b (map byte s)
        l (.length s)]
    (cons (byte l) b)))

(defn bytes<-n_str
  "Converts a string [s] to [n] bytes (from left to right)."
  [n s]
  (let [b (map byte s)
        l (.length s)
        d (- n l)]
    (if (> d 0)
      (concat b (repeat d 0)) 
      (take n b))))

(defn byte8<-nonce
  "Returns nonce."
  []
  (byte8<-uint64_t (rand-int 0x7fffffff) (rand-int 0x7fffffff)))

(defn byte8<-current_timestamp
  "Converts the current timestamp, millisec from 1970/1/1 00:00:00 GMT to 8 bytes."
  []
  (byte8<-int64_t (.getTime (java.util.Date.))))

(defn byte18<-ip_port
  "Converts the [ip port] to 18 bytes (12 - IPv6 prefix, 4 - IPv4, 2 - port) in network byte order. ip format is like 127.0.0.1."
  [ip port]
  (concat (byte8<-int64_t 0x0)
          (reverse (byte4<-int32_t 0xffff))
          (map (fn [s] (Integer. s)) (string/split ip #"\."))
          (reverse (byte2<-int16_t port))))

(defn dhash
  "Returns SHA256(SHA256(bytes)) of the byte-array [bytes]."
  [bytes]
  (let [m (java.security.MessageDigest/getInstance "SHA-256")]
    (.digest m (.digest m bytes))))

(defn checksum
  "Returns the checksum for the [payload] bytes."
  [payload]
  (take 4 (dhash (byte-array payload))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message data

;; message payload of "version"
(def version-payload
  (let [protocol-version (byte4<-int32_t 60002)
        services (byte8<-int64_t 1)
        timestamp (byte8<-current_timestamp)
        receiver-address (concat services
                                 (byte18<-ip_port "0.0.0.0" 8333))
        sender-address (concat services
                                 (byte18<-ip_port "0.0.0.0" 8333))
        node-id (byte8<-nonce)
        sub-version (bytes<-var_str "/Satoshi:0.7.2/")
        block-height (byte4<-int32_t 212672)]
    (concat protocol-version services timestamp receiver-address sender-address node-id sub-version block-height)))

;; message header
(def version-header
  (let [magic (byte4<-int32_t 0xd9b4bef9)
        command (bytes<-n_str 12 "version")
        payload-length (byte4<-int32_t (count version-payload))
        payload-checksum (checksum version-payload)]
    (concat magic command payload-length payload-checksum)))

;; construct the version message bytes
(def version-message
  (concat version-header version-payload))

;; verack message bytes
(def verack-message
  (let [magic (byte4<-int32_t 0xd9b4bef9)
        command (bytes<-n_str 12 "verack")
        payload-length (byte4<-int32_t 0)
        payload-checksum (checksum (byte 0))]
    (concat magic command payload-length payload-checksum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; networking

;; client node over tcp/ip
(defn start-client "Starts a client node." []
  (let [host "95.217.9.184"
        port 8333
        header (byte-array 24)
        payload (byte-array 0x66)] 
    (with-open [socket (Socket. host port)
                out (io/output-stream socket)
                in (io/input-stream socket)]
      (.write out (byte-array version-message))
      (.flush out)
      (.read in header)
      (.read in payload)
      (print-hex (concat header payload)))))

