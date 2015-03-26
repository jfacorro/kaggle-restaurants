(ns restaurants.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as ppr]
            [restaurants.protocols :refer [train predict error]]
            [restaurants.model.average :as avg]
            [restaurants.utils :refer [rmse]]))

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

(defn solution [input output model]
  (spit output "Id,Prediction\n")
  (doseq [record (load-csv input)]
    (spit output
      (str
        (:id record) ","
        (format "%.1f" (predict model record)) "\n")
      :append true)))

(let [records (load-csv "train.csv")
      ;;hist    (reduce (partial histogram records) {} (keys (first records)))
      model   (-> (avg/->AverageBest nil)
                (train records))
      predicts (map (partial predict model) records)]
  (prn (rmse records predicts))
  ;;(solution "test.csv" "output.csv" model)
  #_(ppr/pprint (dissoc hist :id) #_(take 1 hist)))

