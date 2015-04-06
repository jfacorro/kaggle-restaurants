(ns restaurants.model.decision-trees.pruning
  (:require [restaurants.protocols :as p]
            [restaurants.utils :as utils]
            [restaurants.model.decision-trees.protocols :refer [leaf? decide]])
  (:import  [restaurants.model.decision_trees.protocols BranchNode LeafNode]))

(defn- prune? [node validation]
  (and
    (not (leaf? node))
    (let [samples    (.samples node)
          prediction (utils/avg (map p/target samples))
          rmse-node  (utils/rmse validation (repeat prediction))
          rmse-st    (utils/rmse validation (map (partial decide node) validation))]
      (< rmse-node rmse-st))))

(defn prune [node validation-set]
  (if (leaf? node)
    node
    (let [pred     (.pred node)
          left     (prune (.left node) validation-set #_(filter pred validation-set))
          right    (prune (.right node) validation-set #_(filter (comp not pred) validation-set))
          samples  (.samples node)
          new-node (BranchNode. pred left right samples)]
      (if (prune? new-node validation-set)
        (LeafNode. (utils/avg (map p/target samples)))
        new-node))))
