(ns restaurants.model.decision-trees.protocols)

(defprotocol TreeNode
  (decide [this item])
  (leaf?  [this]))

(deftype BranchNode [pred left right samples]
  TreeNode
  (decide [this item]
    (decide (if (pred item) left right) item))
  (leaf? [_] false))

(deftype LeafNode [value]
  TreeNode
  (decide [this _] value)
  (leaf? [_] true))
