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
            [restaurants.model.decision-trees :as dt]
            [restaurants.utils :refer [rmse keywordize]]))

(defn parse-date [date]
  (let [fmt (java.text.SimpleDateFormat. "MM/dd/yyyy")]
  (->
    (.parse fmt date)
    .getMonth)))

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

(defn order-avg-model-by-rmse [records]
  (->> (keys (first records))
    (map #(let [model (train (avg/->AverageBy % nil) records)]
           [% (map (partial predict model) records)]))
    (map (fn [[k ps]] [k (rmse records ps)]))
    (sort-by second)))

(defn predict-and-rmse [k model data-set]
  (->> data-set
    (map (partial predict model))
    (rmse data-set)
    (prn k :----->)))

(try
  (let [records   (load-csv "train.csv")
        split-pos (* (count records) 0.9)
        train-set (vec (take split-pos records))
        test-set  (vec (drop split-pos records))
        ;;hist    (reduce (partial histogram records) {} (keys (first records)))
        models  {;;:average (avg/->Average nil)
                 :average-by-city-group (avg/->AverageBy :city-group nil)
                 ;;:average-by-city (avg/->AverageBy :city nil)
                 :average-best (avg/->AverageBest nil)
                 :bagging-avg-best (bag/->Bagging (avg/->AverageBest nil) 50 137)
                 :regression-tree (dt/->RegressionTree nil)}
        trained (reduce-kv
                  (fn [trained k model]
                    (assoc trained k (train model train-set)))
                  {}
                  models)
        dataset (incanter.core/to-dataset records)]
    #_(doseq [k (keys (first records))]
      (when (number? (k (first records)))
        (doto
          (charts/scatter-plot k :revenue :data dataset :group-by :city-group)
          (i/save (str (name k) ".png")))))
    (doseq [[k model] trained]
      (predict-and-rmse k model test-set)
      (predict-and-rmse k model train-set))
    ;;(order-avg-model-by-rmse records)
    (solution "test.csv" "output.csv" (:regression-tree trained))
    ;;(view (charts/bar-chart :city-group :revenue :data dataset :group-by :open-date :legend true))
    ;;(ppr/pprint (first records))
    #_(dt/split-by-attr records :p1)
    )
  (catch Exception ex
    (repl/pst ex 50)))
