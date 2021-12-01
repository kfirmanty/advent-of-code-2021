(ns day1
  (:require [clojure.string :as string]))

(def input (->> (string/split (slurp "input/day1") #"\n")
               (map #(Integer/parseInt %))))

(defn solve-1 []
  (->>
   input
   (partition 2 1)
   (map (fn [[p n]]
          (> n p)))
   (filter true?)
   count))

(defn solve-2 []
  (->>
   input
   (partition 3 1)
   (partition 2 1)
   (map (fn [[p n]]
          (> (reduce + n) (reduce + p))))
   (filter true?)
   count))
