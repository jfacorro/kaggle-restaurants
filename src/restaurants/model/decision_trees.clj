;;(remove-ns 'restaurants.model.decision-trees)
(ns restaurants.model.decision-trees
  (:require [restaurants.protocols :as p]
            [restaurants.utils :as utils]))

(defprotocol TreeNode
  (decide [this item]))

(deftype BranchNode [pred right left]
  TreeNode
  (decide [this item]
    (decide (if (pred item) right left) item)))

(deftype LeafNode [value]
  TreeNode
  (decide [this item] value))

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
             [k (utils/avg (map :revenue items))]))
      (sort-by second)
      (map first)))

(defmethod make-predicate :number [k [_ [r & _]]]
  #(< (k %) r))

(defmethod make-predicate :category [k [l _]]
  (let [s (set l)]
    #(-> % k s boolean)))

(defn stop-split? [records]
  (<= (count records) 10))

(defn partitions [s]
  (map #(split-at % s) (range 1 (count s))))

(defn rmse [records]
  (let [avg (utils/avg (map :revenue records))]
    (utils/rmse records (repeat avg))))

(defn calculate-error
  [records info]
  (let [halves    (group-by (:pred info) records)
        left      (get halves true)
        right     (get halves false)
        nl        (count left)
        nr        (count right)
        n         (count records)
        rmse      (+ (* (/ nl n) (rmse left))
                     (* (/ nr n) (rmse right)))]
    (assoc info :rmse rmse :left left :right right)))

(defn split-by-attr
  [records k]
  (if-let [parts (->> records (map k) set (sort-values records k) partitions seq)]
    (->> parts
      (map #(->> %
             (make-predicate k)
             (hash-map :attr k :pred)
             (calculate-error records)))
      (sort-by :rmse)
      first)
    (let [avg (utils/avg (map :revenue records))]
      {:rmse (utils/rmse records (repeat avg))})))

(defn best-split [records]
  (let [rmse-all (rmse records)]
    (->> (keys (first records))
      (filter #(-> % #{:revenue :id :city} not))
      (map (partial split-by-attr records))
      (sort-by :rmse)
      first)))

(defn split [records]
  (let [{:keys [pred left right attr rmse]} (best-split records)]
    ;;(prn :split-at attr :left (count left) :right (count right) :rmse rmse)
    [pred left right]))

(defn build-node [records]
  (if (stop-split? records)
    (LeafNode. (utils/avg (map :revenue records)))
    (let [[p l r] (split records)]
      (if p
        (BranchNode. p (build-node l) (build-node r))
        (LeafNode. (utils/avg (map :revenue records)))))))

(defrecord RegressionTree [root]
  p/Model
  (train [this records]
    (assoc this :root (build-node records)))
  (predict [this item]
    (decide root item)))
