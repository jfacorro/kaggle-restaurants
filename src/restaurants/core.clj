(ns restaurants.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as ppr]
            [restaurants.protocols :refer [train predict error]]
            [restaurants.model.average :as avg]))

(def *output-path* "output.csv")

(defn keywordize [s]
  (-> s
    str/lower-case
    (str/replace " " "-")
    keyword))

(defn load-csv [path & [n]]
  (with-open [file (io/reader (io/resource path))]
    (let [[header & records] (line-seq file)
          header  (map keywordize (str/split header #","))
          records (if n (take n records) records)]
      (mapv #(zipmap header (str/split % #",")) records))))

(defn histogram [records h k]
  (assoc h k (into [] (map k records))))

(defn solution [input model]
  (spit *output-path* "Id,Prediction\n")
  (doseq [record (load-csv input 10)]
    (spit *output-path*
      (str
        (:id record) ","
        (format "%.1f" (predict model record)) "\n")
      :append true)))

(let [records (load-csv "train.csv")
      hist    (reduce (partial histogram records) {} (keys (first records)))
      model   (-> (avg/->Average 0)
                (train records))]
  (solution "test.csv" model)
  #_(ppr/pprint (dissoc hist :id) #_(take 1 hist)))

