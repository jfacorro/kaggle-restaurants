(ns restaurants.model.average
  (:require [restaurants.protocols :as p]
            [restaurants.utils :refer [rmse]]))

(defn average [records]
  (let [sum (->> records
              (map :revenue)
              (reduce +))]
    (/ sum (count records))))

(defrecord Average [avg]
  p/Model
  (train [this records] 
    (assoc this :avg (average records)))
  (predict [this item] avg))

(defn compute-avg
  [avgs k records]
  (assoc avgs k (average records)))

(defrecord AverageBy [field avgs]
  p/Model
  (train [this records]
    (let [avgs (->> records 
                 (group-by field)
                 (reduce-kv compute-avg {}))]
      (assoc this :avgs avgs)))
  (predict [this item]
    (get avgs (field item))))

(defrecord AverageBest [avgs]
  p/Model
  (train [this [rec & _ :as records]]
    (let [f    #(p/train (->AverageBy % nil) records)
          ks   (filter (partial not= :revenue) (keys rec))
          avgs (map f ks)]
      (assoc this :avgs avgs)))
  (predict [this item]
    (let [f     (fn [[s c] avg-by]
                  (if-let [v (p/predict avg-by item)]
                    [(+ s v) (inc c)]
                    [s c]))
          [s c] (reduce f [0.0 0] avgs)]
      (/ s c))))
