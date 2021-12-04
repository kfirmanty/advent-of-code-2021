(ns day4
  (:require [clojure.string :as string]))

(defn- find-num [board num]
  (filter some?
          (for [x (range 5)
                y (range 5)
                :when (= num (-> board (nth y) (nth x) first))]
            [x y])))

(defn- mark [board num]
  (let [occurences (find-num board num)]
    (reduce (fn [board [x y]]
              (assoc-in board [y x] [num true])) board occurences)))

(defn- rows [board]
  board)

(defn- cols [board]
  (for [i (range 5)]
    (map #(nth % i) (rows board))))

(defn- winning? [board]
  (let [rows-cols (concat (rows board) (cols board))]
    (->> rows-cols
         (filter #(->> %
                       (map second)
                       (reduce (fn [p n]
                                 (and p n)))))
         not-empty)))

(defn- ->board [nums]
  (->> nums
       (filter not-empty)
       (map #(vector (Integer/parseInt %) false))
       (partition 5)
       (map (partial into []))
       (into [])))

(defn- unmarked-sum [board]
  (->> board
       (apply concat)
       (filter #(-> % second not))
       (map first)
       (reduce +)))

(defn- parse-input [file]
  (let [[numbers & boards] (-> file slurp (string/split #"\n\n"))
        numbers (map #(Integer/parseInt %) (string/split numbers #","))
        boards (->> boards
                    (map #(-> %
                              (string/replace "\n" " ")
                              (string/split #"\s+")))
                    (map ->board))]
    [numbers boards]))

(def input
  (parse-input "input/day4"))

(def test-input
  (parse-input "input/day4test"))

(defn solve-1 []
  (let [[numbers boards] input]
    (loop [[number & numbers] numbers boards boards]
      (let [boards (map #(mark % number) boards)
            winning (filter winning? boards)]
        (if-not (empty? winning)
          (* number (unmarked-sum (first winning)))
          (recur numbers boards))))))

(defn solve-2 []
  (let [[numbers boards] input]
    (loop [[number & numbers] numbers boards boards latest-winner nil]
      (let [boards (map #(mark % number) boards)
            winning (filter winning? boards)
            boards (remove #((into #{} winning) %) boards)
            latest-winner (if (not-empty winning)
                            [(first winning) number]
                            latest-winner)]
        (if (nil? numbers)
          (let [[board number] latest-winner]
            (* number (unmarked-sum board)))
          (recur numbers boards latest-winner))))))
