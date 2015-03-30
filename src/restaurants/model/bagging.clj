(ns restaurants.model.bagging
  (:require [restaurants.protocols :as p]
            [restaurants.utils :refer [rmse avg]]))

(defn rand-sample [records n]
  (let [c (count records)]
    (loop [s [], i n]
      (if (pos? i)
        (recur
          (conj s (nth records (rand-int c)))
          (dec i))
        s))))

(defrecord Bagging [model m n]
  p/Model
  (train [this records]
    (let [samples (repeatedly m #(rand-sample records n))
          models  (map (partial p/train model) samples)]
      (assoc this :models models)))
  (predict [this item]
    (let [predictions (mapv #(p/predict % item) (:models this))]
      (avg predictions))))