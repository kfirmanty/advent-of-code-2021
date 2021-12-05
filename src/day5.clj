(ns day5
  (:require [clojure.string :as string]))

(defn parse-input [f]
  (let [lines (string/split (slurp f) #"\n")]
    (map (fn [l]
           (let [nums (-> l
                          (string/replace " -> " ",")
                          (string/split #","))]
             (map #(Integer/parseInt %) nums)))
         lines)))

(def input (parse-input "input/day5"))
(def test-input (parse-input "input/day5test"))

(defn- straight? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn- diagonal? [[x1 y1 x2 y2]]
  (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn- straight-line->points [[x1 y1 x2 y2]]
  (let [[x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))]
      [x y])))

(defn- diagonal-line->points [[x1 y1 x2 y2]]
  (let [xdir (if (< x1 x2)
               1
               -1)
        ydir (if (< y1 y2)
               1
               -1)]
    (for [i (range (inc (Math/abs (- x1 x2))))]
      [(+ x1 (* xdir i))
       (+ y1 (* ydir i))])))

(defn- line->points [line]
  (if (straight? line)
    (straight-line->points line)
    (diagonal-line->points line)))

(defn- solve [viable?]
  (let [points (->>
                input
                (filter viable?)
                (mapcat line->points))]
    (->> points
         (filter some?)
         sort
         (partition-by identity)
         (filter #(> (count %) 1))
         count)))

(defn solve-1 []
  (solve straight?))

(defn solve-2 []
  (solve #(or (straight? %) (diagonal? %))))
