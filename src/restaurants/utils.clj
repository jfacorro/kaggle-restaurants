(ns restaurants.utils)

(defn rmse [records predictions]
  (->> records
    (map :revenue)
    (map #(Float/parseFloat %))
    (map - predictions)
    (map #(* % %))
    (reduce +)
    (* (/ (count records)))
    Math/sqrt))