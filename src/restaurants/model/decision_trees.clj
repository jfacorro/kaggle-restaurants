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
        variance  (- (* n (utils/variance records))
                     (* nl (utils/variance left))
                     (* nr (utils/variance right)))]
    (assoc info :error (/ variance) :left left :right right)))

(defn split-by-attr
  [records k]
  (if-let [parts (->> records (map k) set (sort-values records k) partitions seq)]
    (->> parts
      (map #(->> %
             (make-predicate k)
             (hash-map :attr k :pred)
             (calculate-variance records)))
      (sort-by :error)
      first)
    {:error 0 #_(utils/variance records)}))

(defn best-split [records attr-selection]
  (->> (attr-selection (first records))
    (map (partial split-by-attr records))
    (sort-by :error)
    first))

(defn build-node [records attr-selection]
  (if (stop-split? records)
    (LeafNode. (utils/avg (map p/target records)))
    (let [{:keys [pred left right] :as split} (best-split records attr-selection)]
      #_(prn :split-at (:attr split)
          :left  (count left)
          :right (count right)
          :error (:error split)
          :pred  (-> pred meta :v ))
      (if pred
        (BranchNode. pred 
          (build-node left attr-selection) 
          (build-node right attr-selection))
        (LeafNode. (utils/avg (map p/target records)))))))

(defrecord RegressionTree [root]
  p/Model
  (description [this] "Regression Tree")
  (train [this records]
    (assoc this :root (build-node records p/attributes)))
  (predict [this item]
    (decide root item)))


(defrecord RandomForest [roots n m]
  p/Model
  (description [this] "Random Forest")
  (train [this records]
    (let [random-attrs
                (fn [r] (->> (p/attributes r) shuffle (take m)))
          roots (repeatedly n #(build-node records random-attrs))]
      (assoc this :roots roots)))
  (predict [this item]
    (->> roots
      (map #(decide % item))
      utils/avg)))
