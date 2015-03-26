(ns restaurants.model.bagging
  (:require [restaurants.protocols :as p]
            [restaurants.utils :refer [rmse avg]]))

(defn rand-sample [records n]
  (reduce (fn [s _]
            (conj s (get records (rand-int (count records))))) 
    []
    (range n)))

(defrecord Bagging [model m n]
  p/Model
  (train [this records]
    (let [samples (repeatedly m #(rand-sample records n))
          models  (map (partial p/train model) samples)]
      (assoc this :models models)))
  (predict [this item]
    (let [predictions (map #(p/predict % item) (:models this))]
      (avg predictions))))