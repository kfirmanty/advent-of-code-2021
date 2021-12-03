(ns day3
  (:require [clojure.string :as string]))

(def input (-> "input/day3"
               slurp
               (string/split #"\n")))

(defn- extract-bit [vals most-common?]
  (let [extract (if most-common?
                  last
                  first)
        [group-a group-by :as groups] (->> vals
                                           sort
                                           (partition-by identity)
                                           (sort-by count))]
    (if (= (count group-a) (count group-by))
      (if most-common?
        \1
        \0)
      (->> groups
           extract
           first))))

(defn solve-1 []
  (let [raw-nums (for [i (-> input first count range)]
                   (let [col (map #(nth % i) input)
                         gamma (extract-bit col true)
                         epsilon (extract-bit col false)]
                     [gamma epsilon]))]
    (->> raw-nums
         (apply map vector)
         (map #(apply str %))
         (map #(Integer/parseInt % 2))
         (apply *))))

(defn- find-rating [input rating-type]
  (loop [possibilities input i 0]
    (if (= (count possibilities) 1)
      (Integer/parseInt (first possibilities) 2)
      (let [col (map #(nth % i) possibilities)
            bit (extract-bit col (= rating-type :oxygen))
            possibilities (->> possibilities
                               (filter #(.startsWith % (str bit) i)))]
        (recur possibilities (inc i))))))

(defn solve-2 []
  (let [oxygen (find-rating input :oxygen)
        co2 (find-rating input :co2)]
    (* oxygen co2)))
