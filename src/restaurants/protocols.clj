(ns restaurants.protocols)

(defprotocol Model
  (description [this])
  (train [this train-set])
  (predict [this item]))

(defprotocol DataPoint
  (target [this])
  (attributes [this]))