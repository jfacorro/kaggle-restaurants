(ns restaurants.utils
  (:require [clojure.string :as str]
            [restaurants.protocols :as p]))

(defn rmse [test-set predictions]
  (->> test-set
    (map p/target)
    (map - predictions)
    (map #(* % %))
    (reduce +)
    (* (/ (count test-set)))
    Math/sqrt))

(defn avg [s]
  (/ (reduce + s) (count s)))

(defn keywordize [s]
  (-> s
    str/lower-case
    (str/replace " " "-")
    keyword))
