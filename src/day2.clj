(ns day2
  (:require [clojure.string :as string]))

(def input (->>
            (string/split (slurp "input/day2") #"\n")
            (map #(let[[dir val] (string/split % #"\s+")]
                    [(keyword dir) (Integer/parseInt val)]))))

(defn solve-1 []
  (let [s (reduce (fn [s [dir val]]
                   (case dir
                     :up (update s :depth - val)
                     :down (update s :depth + val)
                     :forward (update s :horizontal + val))) {:depth 0 :horizontal 0} input)]
    (* (:depth s) (:horizontal s))))

(defn solve-2 []
  (let [s (reduce (fn [s [dir val]]
                   (case dir
                     :up (update s :aim - val)
                     :down (update s :aim + val)
                     :forward (-> s
                                  (update :horizontal + val)
                                  (update :depth + (* (:aim s) val))))) {:depth 0 :horizontal 0 :aim 0} input)]
    (* (:depth s) (:horizontal s))))
