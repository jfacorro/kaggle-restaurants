(ns restaurants.utils)

(defn rmse [records predictions]
  (->> records
    (map :revenue)
    (map - predictions)
    (map #(* % %))
    (reduce +)
    (* (/ (count records)))
    Math/sqrt))

(defn avg [s]
  (/ (reduce + s) (count s)))