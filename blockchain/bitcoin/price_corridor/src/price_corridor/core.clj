(ns price-corridor.core
  (:require [clojure.data.csv :as csv]
            [com.hypirion.clj-xchart :as c]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.periodic :as p]))

;; load the csv data of btc historical prices
(def data 
  (let [csv-filename "data/btc_history.csv"]
    (reverse 
      (map (fn [tick]
             (let [[date close] tick]
               [(f/parse (f/formatter "yyyy/M/d") date)
                (Float/parseFloat close)]))
           (nthrest (with-open [in-file (clojure.java.io/reader csv-filename)]
                      (doall
                        (csv/read-csv in-file))) 1)))))

;;(take 50 (p/periodic-seq (f/parse (f/formatter "yyyy/M/d") "2009/1/3") (t/days 1))) 

(defn ndays
  "how many days since the begin date (yyyy MM dd)"
  [y m d]
  (ndays- (t/date-time y m d)))

(defn ndays-
  "how many days since the begin date (date-time)"
  [end-date]
  (let [begin-date (t/date-time 2009 1 1)]
    (t/in-days (t/interval begin-date end-date))))

(defn predict*
  "calculates the predicted price for a day"
  [a b days]
  (Math/pow 10 (+ a (* b (Math/log10 days)))))

(defn predict1
  [days]
  (let [a -17.01593313
        b 5.84509376]
    (predict* a b days)))

;; charting
;; ref: https://github.com/hypirion/clj-xchart/blob/master/docs/render-options.md
(c/view
  (let [data_x (map #(ndays- (first %)) data)
        data_y (map #(second %) data)]
    (c/xy-chart
      {"price" {:x data_x :y data_y :style {:marker-type :none}}
       "pred1" {:x data_x :y (map predict1 data_x) :style {:marker-type :none}}}
      {:title "bitcoin growth"
       :y-axis {:logarithmic? true}
       :x-axis {:logarithmic? true
                :label {:rotation 90}}})))
