(ns restaurants.model.decision-trees
  (:require [restaurants.protocols :as p]
            [restaurants.utils :as utils]
            [restaurants.model.average :as avg]
            [restaurants.model.decision-trees.pruning :as prune]
            [restaurants.model.decision-trees.protocols :refer [leaf? decide]])
  (:import  [restaurants.model.decision_trees.protocols BranchNode LeafNode]))

(defn attr-class [x]
  (if (number? x)
    :number
    :category))

(defmulti sort-values (fn [_ _ s] (-> s first attr-class)))
(defmulti make-predicate (fn [_ s] (-> s ffirst attr-class)))

(defmethod sort-values :number [_ _ s]
  (sort s))

(defmethod sort-values :category [records k s]
    (->> records
      (group-by k)
      (map (fn [[k items]]
             [k (utils/avg (map p/target items))]))
      (sort-by second)
      (map first)))

(defmethod make-predicate :number [k [_ [r & _]]]
  (with-meta #(< (k %) r) {:v r}))

(defmethod make-predicate :category [k [l _]]
  (let [s (set l)]
    (with-meta
      #(-> % k s boolean)
      {:v l})))

(defn stop-split? [records]
  (<= (count records) 7))

(defn partitions [s]
  (map #(split-at % s) (range 1 (count s))))

(defn calculate-rmse
  [records info]
  (let [halves    (group-by (:pred info) records)
        left      (get halves true)
        right     (get halves false)
        nl        (count left)
        nr        (count right)
        n         (count records)
        rmse      (+ (* (/ nl n) (utils/rmse left))
                     (* (/ nr n) (utils/rmse right)))]
    (assoc info :error rmse :left left :right right)))

(defn calculate-variance
  [records info]
  (let [halves    (group-by (:pred info) records)
        left      (get halves true)
        right     (get halves false)
        nl        (count left)
        nr        (count right)
        n         (count records)
        variance  (+ (- (* n (utils/variance records)))
                     (* nl (utils/variance left))
                     (* nr (utils/variance right)))]
    (assoc info :error variance :left left :right right)))

(defn split-by-attr
  [records k]
  (when-let [parts (->> records (map k) set (sort-values records k) partitions seq)]
    (->> parts
      (map #(->> %
             (make-predicate k)
             (hash-map :attr k :pred)
             (calculate-variance records)))
      (sort-by :error)
      first)))

(defn best-split [[rec :as records] attr-sel]
  (->> (attr-sel rec)
    (map (partial split-by-attr records))
    (filter identity)
    (sort-by :error)
    first))

(defn- prn-split-info [{:keys [attr left right pred error]}]
  (prn :split-at attr
    :left  (count left)
    :right (count right)
    :error error
    :pred  (-> pred meta :v )))

(defn build-node [records attr-sel]
  (if (stop-split? records)
    (LeafNode. (utils/avg (map p/target records)))
    (let [{:keys [pred left right] :as split} (best-split records attr-sel)]
      ;;(prn-split-info split)
      (if pred
        (let [[left right] (map #(build-node % attr-sel) [left right])]
          (BranchNode. pred left right records))
        (LeafNode. (utils/avg (map p/target records)))))))

(defn best-attrs [ds n]
  (->> ds
    (p/train (avg/->AverageBest nil n))
    :models
    (map :field)))

(defrecord RegressionTree [root]
  p/Model
  (description [this] "Regression Tree")
  (train [this train-set]
    (p/train this train-set nil))
  (train [this train-set validation-set]
    (let [attrs    (best-attrs train-set 13)
          attr-sel #(do % attrs)
          root (build-node train-set attr-sel)
          root (if (seq validation-set) (prune/prune root validation-set) root)]
      (assoc this :root root)))
  (predict [this item]
    (decide root item)))

(defrecord RandomForest [roots n m]
  p/Model
  (description [this] "Random Forest")
  (train [this train-set]
    (p/train this train-set nil))
  (train [this train-set validation-set]
    (let [attrs      (best-attrs train-set 15)
          rand-attrs #(do % (->> attrs shuffle (take m)))
          f          #(let [root (build-node train-set rand-attrs)]
                       (if (seq validation-set)
                         (prune/prune root validation-set)
                         root))
          roots      (apply pcalls (repeat n f))]
      (prn :roots (frequencies (map leaf? roots)))
      (assoc this :roots roots)))
  (predict [this item]
    (->> roots
      (map #(decide % item))
      utils/avg)))
