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

(defmulti sort-values (comp attr-class first))
(defmulti make-predicate (fn [_ s] (-> s ffirst attr-class)))

(defmethod sort-values :number [s]
  (sort s))

(defmethod sort-values :category [s]
  (shuffle s))

(defmethod make-predicate :number [attr [_ [r & _]]]
  #(< (attr %) r))

(defmethod make-predicate :category [attr [l _]]
  (let [s (set l)]
    #(-> % attr s boolean)))

(defn stop-split? [records]
  (<= (count records) 5))

(defn partitions [s]
  (map #(split-at % s) (range 1 (count s))))

(defn calculate-error
  [records info]
  (let [halves    (group-by (:pred info) records)
        left      (get halves true)
        nl        (count left)
        right     (get halves false)
        nr        (count right)
        n         (count records)
        left-avg  (utils/avg (map :revenue left))
        right-avg (utils/avg (map :revenue right))
        rmse      (+ (* (/ nl n) (utils/rmse left (repeat left-avg)))
                     (* (/ nr n) (utils/rmse right (repeat right-avg))))]
    (assoc info :rmse rmse :left left :right right)))

(defn split-by-attr
  [records k]
  (if-let [parts (->> records (map k) set sort-values partitions seq)]
    (->> parts
      (map (partial make-predicate k))
      (map (partial hash-map :attr k :pred))
      (map (partial calculate-error records))
      (sort-by :rmse)
      first)
    (let [avg (utils/avg (map :revenue records))]
      {:rmse (utils/rmse records (repeat avg))})))

(defn ->>prn [f x]
  (prn (f x))
  x)

(defn rmse [records]
  (let [avg (utils/avg (map :revenue records))]
    (utils/rmse records (repeat avg))))

(defn best-split [records]
  (let [rmse-all (rmse records)]
    (->> (keys (first records))
      (filter #(-> % #{:revenue :id :city} not))
      (map (partial split-by-attr records))
      (sort-by :rmse)
      (filter #(< (:rmse %) rmse-all))
      first
      #_(->>prn (fn [{:keys [rmse]}]
                  (let [avg-all   (utils/avg (map :revenue records))
                        rmse-all  (utils/rmse records (repeat avg-all))]
                    (if (< rmse-all rmse)
                      (prn :WTF!!! rmse-all rmse)
                      (prn :YEAHH!!!))))))))

(defn split [records]
  (let [{:keys [pred left right attr rmse]} (best-split records)]
    (prn :split-at attr :left (count left) :right (count right) :rmse rmse)
    [pred left right]))

(defn build-node [records]
  (if (stop-split? records)
    (do
      ;(prn (map (juxt :id :p8 :revenue) records ))
      (LeafNode. (utils/avg (map :revenue records))))
    (let [[p l r] (split records)]
      (if p
        ;;(prn :->branch (count l) (count r))
        (BranchNode. p (build-node l) (build-node r))
        (LeafNode. (utils/avg (map :revenue records)))))))

(defrecord RegressionTree [root]
  p/Model
  (train [this records]
    (assoc this :root (build-node records)))
  (predict [this item]
    (decide root item)))