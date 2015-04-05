(ns restaurants.model.decision-trees.pruning
  (:require [restaurants.protocols :as p]
            [restaurants.utils :as utils]
            [restaurants.model.decision-trees.protocols :refer [leaf? decide]])
  (:import  [restaurants.model.decision_trees.protocols BranchNode LeafNode]))

(defn- alpha [mv lv sv m delta]
  (->> (+ lv sv)
    (* (Math/log (* 2 m)))
    (+ (Math/log (/ m delta)))
    (/ mv)
    Math/sqrt
    (* 1.2)))

(defn- count-nodes 
  [node]
  (if (leaf? node)
    1
    (+ 1 (count-nodes (.left node)) 
         (count-nodes (.right node)))))

(defn- prune? [node depth cnt-all]
  (and
    (not (leaf? node))
    (let [samples   (.samples node)
          mv        (count samples)
          lv        depth
          sv        (count-nodes node)
          m         cnt-all
          alpha     (alpha mv lv sv m 0.5)
          rmse-node (utils/rmse samples)
          rmse-st   (utils/rmse samples (map (partial decide node) samples))]
      (< (* rmse-node alpha) rmse-st))))

(defn prune [node depth cnt-all]
  (if (leaf? node)
    node
    (let [left     (prune (.left node) (inc depth) cnt-all)
          right    (prune (.right node) (inc depth) cnt-all)
          samples  (.samples node)
          new-node (BranchNode. (.pred node) left right samples)]
      (if (prune? new-node depth cnt-all)
        (do
          (prn :pruned!)
          (LeafNode. (utils/avg (map p/target samples))))
        new-node))))
