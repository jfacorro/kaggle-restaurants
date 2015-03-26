(ns restaurants.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as ppr]))

(def *output-path* "output.csv")

(defn keywordize [s]
  (-> s
    str/lower-case
    (str/replace " " "-")
    keyword))

(defn load-csv [path]
  (with-open [file (io/reader (io/resource path))]
    (let [[header & records] (line-seq file)
          header (map keywordize (str/split header #","))]
      (mapv #(zipmap header (str/split % #",")) records))))

(defn histogram [records h k]
  (assoc h k (into [] (map k records))))

(defn solution-avg [avg]
  (spit *output-path* "Id,Prediction\n")
  (doseq [n (range 100000)]
    (spit *output-path* (str n "," (format "%.1f" avg) "\n") :append true)))

(let [records (load-csv "train.csv")
      hist    (reduce (partial histogram records) {} (keys (first records)))
      revenues (->> hist
                 :revenue
                 (map #(Float/parseFloat %))
                 (reduce +))]
  (solution-avg (/ revenues (count records)))
  #_(ppr/pprint (dissoc hist :id) #_(take 1 hist)))

