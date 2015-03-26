(ns restaurants.protocols)

(defprotocol Model
  (train [train-set])
  (error [test-set])
  (predict [item]))
