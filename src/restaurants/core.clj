(ns restaurants.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as ppr]
            [restaurants.protocols :refer [train predict]]
            [restaurants.model.average :as avg]
            [restaurants.model.bagging :as bag]
            [restaurants.utils :refer [rmse]]))

(defn parse-date [date]
  (let [fmt (java.text.SimpleDateFormat. "MM/dd/yyyy")]
  (.parse fmt date)))

(defn convert-value [k v]
  (cond
    (re-find #"^\d+$" v) (Integer/parseInt v)
    (re-find #"^\d+.\d+$" v) (Float/parseFloat v)
    (= k :open-date) (parse-date v)
    :else (keywordize v)))

(defn process-values [rec]
  (reduce-kv #(assoc %1 %2 (convert-value %2 %3)) rec rec))

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
      (->> records
        (mapv #(zipmap header (str/split % #",")))
        (mapv process-values)))))

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

(defn order-avg-model-by-rmse [records]
  (->> (keys (first records))
    (map #(let [model (train (avg/->AverageBy % nil) records)]
           [% (map (partial predict model) records)]))
    (map (fn [[k ps]] [k (rmse records ps)]))
    (sort-by second)))

(try
  (let [records (load-csv "train.csv")
        ;;hist    (reduce (partial histogram records) {} (keys (first records)))
        models  {:average (avg/->Average nil)
                 :average-by-city-group (avg/->AverageBy :city-group nil)
                 :average-by-city (avg/->AverageBy :city nil)
                 :average-best (avg/->AverageBest nil)
                 :bagging-avg-best (bag/->Bagging (avg/->AverageBest nil) 50 1000)}
        trained (reduce-kv
                  (fn [trained k model]
                    (assoc trained k (train model records)))
                  {}
                  models)]
    (doseq [[k model] trained]
      (->> records
        (map (partial predict model))
        (rmse records)
        (prn k :----->)))
    ;;(ppr/pprint (first records))
    ;;(order-avg-model-by-rmse records)
    ;;(solution "test.csv" "output.csv" model)

    #_(ppr/pprint (dissoc hist :id) #_(take 1 hist)))
  (catch Exception ex
    (clojure.repl/pst ex)))

