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
  (description [this]
    (str "Bagging for " (p/description model)))
  (train [this train-set]
    (p/train this train-set nil))
  (train [this train-set validation-set]
    (let [samples (repeatedly m #(rand-sample train-set n))
          models  (pmap #(p/train model % validation-set) samples)]
      (assoc this :models models)))
  (predict [this item]
    (let [predictions (mapv #(p/predict % item) (:models this))]
      (avg predictions))))
