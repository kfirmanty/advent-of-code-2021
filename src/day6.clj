(ns day6
  (:require [clojure.string :as string]))

(defn- parse [f]
  (map #(Integer/parseInt %) (-> f
                                 slurp
                                 (string/split #","))))
(def input (parse "input/day6"))
(def test-input (parse "input/day6test"))

(defn- tick-map [hmap]
  (into {}
        (for [i (range (count (keys hmap)))]
          (cond
            (= i 6) [i (+ (hmap 0) (hmap 7))]
            (= i 8) [i (hmap 0)]
            :else [i (hmap (inc i))]))))

(defn- solve [days-limit]
  (let [mem (merge (into {} (for [i (range 9)]
                              [i 0]))
                   (->> input
                        (group-by identity)
                        (map (fn [[k fs]]
                               [k (count fs)]))
                        (into {})))]
    (loop [days days-limit fishes mem]
      (if (<= days 0)
        (reduce + (vals fishes))
        (recur (dec days) (tick-map fishes))))))

(defn- solve-1 []
  (solve 80))

(defn- solve-2 []
  (solve 256))
