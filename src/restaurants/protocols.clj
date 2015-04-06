(ns restaurants.protocols)

(defprotocol Model
  (description [this])
  (train [this train-set] [this train-set validation-set])
  (predict [this item]))

(defprotocol DataPoint
  (target [this])
  (attributes [this]))