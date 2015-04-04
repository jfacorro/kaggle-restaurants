(ns restaurants.model.average
  (:require [restaurants.protocols :as p]
            [restaurants.utils :refer [rmse] :as utils]))

(defrecord Average [avg]
  p/Model
  (description [this] "Average")
  (train [this records] 
    (assoc this :avg (utils/avg (map p/target records))))
  (predict [this item] avg))

(defn compute-avg
  [avgs k records]
  (assoc avgs k (utils/avg (map p/target records))))

(defrecord AverageBy [field avgs]
  p/Model
  (description [this] (str "Average By " field))
  (train [this records]
    (let [avgs (->> records 
                 (group-by field)
                 (reduce-kv compute-avg {}))]
      (assoc this :avgs avgs)))
  (predict [this item]
    (get avgs (field item))))


(defrecord AverageByEach [models]
  p/Model
  (description [this] (str "Average By Avg. of Each Field"))
  (train [this [rec & _ :as records]]
    (let [f      #(p/train (->AverageBy % nil) records)
          models (map f (p/attributes rec))]
      (assoc this :models models)))
  (predict [this item]
    (let [f     (fn [[s c] avg-by]
                  (if-let [v (p/predict avg-by item)]
                    [(+ s v) (inc c)]
                    [s c]))
          [s c] (reduce f [0.0 0] models)]
      (/ s c))))

(defrecord AverageBest [models n]
  p/Model
  (description [this] (str "Average the Best " n " Fields"))
  (train [this [rec & _ :as records]]
    (let [f      #(p/train (->AverageBy % nil) records)
          models (map f (p/attributes rec))
          rmse   (fn [model]
                   [(rmse records (map #(p/predict model %) records))
                    model])
          models (->> models
                   (map rmse)
                   (sort-by first)
                   (take n)
                   (map second))]
      (assoc this :models models)))
  (predict [this item]
    (let [f     (fn [[s c] avg-by]
                  (if-let [v (p/predict avg-by item)]
                    [(+ s v) (inc c)]
                    [s c]))
          [s c] (reduce f [0.0 0] models)]
      (/ s c))))
