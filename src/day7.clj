(ns day7
  (:require [clojure.string :as string]))

(defn- parse [f]
  (map #(Integer/parseInt %) (-> f
                                 slurp
                                 (string/split #","))))

(def input (parse "input/day7"))

(def test-input (parse "input/day7test"))

(defn- solve [cost-fn]
  (let [positions (sort input)
        scored (for [pos (range (first positions) (inc (last positions)))]
                 [pos (reduce #(+ %1 (cost-fn (Math/abs (- %2 pos)))) 0 positions)])]
    (->> scored
         (sort-by second)
         first)))

(defn solve-1 []
  (solve identity))

(def inc-step-cost (memoize (fn [steps]
                              (reduce + (range (inc steps))))))

(defn solve-2 []
  (solve inc-step-cost))
