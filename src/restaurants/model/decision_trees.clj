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
  (let [avg  (utils/avg (map p/target records))
        rmse (rmse records)]
    (< (/ rmse avg) 0.55))
  #_(<= (count records) 1))

(defn partitions [s]
  (map #(split-at % s) (range 1 (count s))))

(defn rmse [records]
  (let [avg (utils/avg (map p/target records))]
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
    {:rmse (rmse records)}))

(defn best-split [records]
  (->> (p/attributes (first records))
    (map (partial split-by-attr records))
    (sort-by :rmse)
    first))

(defn split [records]
  (let [{:keys [pred left right attr rmse]} (best-split records)]
    ;;(prn :split-at attr :left (count left) :right (count right) :rmse rmse :pred (-> pred meta :v ))
    [pred left right]))

(defn build-node [records]
  (if (stop-split? records)
    (LeafNode. (utils/avg (map p/target records)))
    (let [[p l r] (split records)]
      (if p
        (BranchNode. p (build-node l) (build-node r))
        (LeafNode. (utils/avg (map p/target records)))))))

(defrecord RegressionTree [root]
  p/Model
  (description [this] "Regression Tree")
  (train [this records]
    (assoc this :root (build-node records)))
  (predict [this item]
    (decide root item)))
