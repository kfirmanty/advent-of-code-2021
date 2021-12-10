(ns day10
  (:require [clojure.string :as string]))

(defn parse [f]
  (-> f slurp (string/split #"\n")))

(def input (parse "input/day10"))

(def test-input (parse "input/day10test"))

(defn- closes? [c history]
  (let [matches {\) \(
                 \} \{
                 \> \<
                 \] \[}
        last-opened (peek history)]
    (= (matches c) last-opened)))

(defn- syntax-check-line [line]
  (let [openings #{\{ \[ \( \<}
        points {\) 3
                \] 57
                \} 1197
                \> 25137}]
    (loop [[c & line] line opened-history '()]
      (cond
        (nil? c) opened-history
        (openings c) (recur line (conj opened-history c))
        (and (not (openings c)) (closes? c opened-history)) (recur line (pop opened-history))
        :else (points c)))))

(defn solve-1 []
  (->> input
       (map syntax-check-line)
       (filter int?)
       (reduce +)))

(defn- opened->points [opened]
  (let [points {\( 1
                \[ 2
                \{ 3
                \< 4}]
    (reduce (fn [total opened]
              (let [p (points opened)]
                (+ (* 5 total) p))) 0 opened)))

(defn solve-2 []
  (let [answers (->> input
                     (map syntax-check-line)
                     (filter #(-> % int? not))
                     (map opened->points)
                     sort)]
    (nth answers (Math/floor (/ (count answers) 2)))))
