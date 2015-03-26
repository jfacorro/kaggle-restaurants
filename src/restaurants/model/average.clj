(ns restaurants.model.average
  (:require [restaurants.protocols :as p]))

(defrecord Average [avg]
  p/Model
  (train [this records]
    (let [sum (->> records
                (map :revenue)
                (reduce +))]
      (assoc this :avg (/ sum (count records))))))