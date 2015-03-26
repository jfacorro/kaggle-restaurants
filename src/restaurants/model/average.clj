(ns restaurants.model.average
  (:require [restaurants.protocols :as p]))

(defrecord Average [avg]
  p/Model
  (train [this records]
    (let [sum (->> records
                (map (comp #(Float/parseFloat %) :revenue))
                (reduce +))]
      (assoc this :avg (/ sum (count records)))))
  (error [this records]
    0)
  (predict [this item]
    avg))