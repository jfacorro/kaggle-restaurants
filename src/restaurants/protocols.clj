(ns restaurants.protocols)

(defprotocol Model
  (train [this train-set])
  (predict [this item]))
