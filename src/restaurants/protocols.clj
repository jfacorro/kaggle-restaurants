(ns restaurants.protocols)

(defprotocol Model
  (train [this train-set])
  (error [this test-set])
  (predict [this item]))
