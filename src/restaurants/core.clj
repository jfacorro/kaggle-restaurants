(ns restaurants.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as ppr]
            [clojure.repl :as repl]

            [incanter.core :refer [view] :as i]
            [incanter.charts :as charts]
            incanter.io

            [restaurants.protocols :refer [train predict]]
            [restaurants.model.average :as avg]
            [restaurants.model.bagging :as bag]
            [restaurants.model.decision-trees :as dt :reload true]
            [restaurants.utils :refer [rmse keywordize] :as utils]))

(defn parse-date [date]
  (let [fmt (java.text.SimpleDateFormat. "MM/dd/yyyy")]
    (.getMonth (.parse fmt date))))

(defn convert-value [k v]
  (cond
    (re-find #"^\d+$" v) (Integer/parseInt v)
    (re-find #"^\d+.\d+$" v) (Float/parseFloat v)
    (= k :open-date) (parse-date v)
    :else (keywordize v)))

(defn process-values [rec]
  (reduce-kv #(assoc %1 %2 (convert-value %2 %3)) {} rec))

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

(defn cross-validation [k dataset model]
  (let [n           (int (/ (count dataset) k))
        validation  (partition n dataset)
        training    (->> validation
                      (map set)
                      (map #(remove % dataset)))
        cv          (fn [t v]
                      (let [trained (train model t)
                            predicted (map (partial predict trained) v)]
                        (rmse v predicted)))
        result      (map cv training validation)]
    (utils/avg result)))

(try
  (let [records (load-csv "train.csv")
        ;;hist    (reduce (partial histogram records) {} (keys (first records)))
        models  {;;:average (avg/->Average nil)
                 ;;:average-by-city-group (avg/->AverageBy :city-group nil)
                 ;;:average-by-city (avg/->AverageBy :city nil)
                 :average-best (avg/->AverageBest nil)
                 ;;:bagging-avg-best (bag/->Bagging (avg/->AverageBest nil) 10 137)
                 ;;:regression-tree (dt/->RegressionTree nil)
                 :bagging-regression-tree (bag/->Bagging (dt/->RegressionTree nil) 5 137)
                 }
        ;;dataset (incanter.core/to-dataset records)
        ]
    #_(doseq [k (keys (first records))]
      (when (number? (k (first records)))
        (doto
          (charts/scatter-plot k :revenue :data dataset :group-by :city-group)
          (i/save (str (name k) ".png")))))
    (doseq [[k model] models]
      (prn k
        (cross-validation 10 records model)))
    ;;(solution "test.csv" "output.csv" (:regression-tree trained))
    )
  (catch Exception ex
    (repl/pst ex 20)))
