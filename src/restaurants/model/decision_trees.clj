(ns restaurants.model.decision-trees
  (:require [restaurants.protocols :as p]))

(deftype BranchNode [pred right left])
(deftype LeafNode [value])

(defn build-tree [records]
  (BranchNode. 1 1 1))

(defrecord DecisionTree [tree]
  p/Model
  (train [this records]
    (assoc this :avg 0))
  (predict [this item]
    tree))