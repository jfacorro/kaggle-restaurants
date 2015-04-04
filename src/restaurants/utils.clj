(ns restaurants.utils
  (:require [clojure.string :as str]
            [restaurants.protocols :as p]))

(defn avg [s]
  (/ (reduce + s) (count s)))

(defn rmse
  ([records]
   (rmse records 
     (->> records (map p/target) avg repeat)))
  ([records predictions]
    (->> records
      (map p/target)
      (map - predictions)
      (map #(* % %))
      (reduce +)
      (* (/ (count records)))
      Math/sqrt)))

(defn variance
  [records]
  (->> records
    (map p/target)
    (map - (->> records (map p/target) avg repeat))
    (map #(* % %))
    (reduce +)
    (* (/ (count records)))))

(defn keywordize [s]
  (-> s
    str/lower-case
    (str/replace " " "-")
    keyword))
