(ns day11
  (:require [clojure.string :as string]))

(defn- parse [f]
  (loop [[line & lines] (map (fn [line] (map #(Integer/parseInt %) (string/split line #"")))
                             (-> f slurp (string/split #"\n"))) y 0 board {}]
    (if (nil? line)
      board
      (let [board (->> line (map-indexed (fn [i el] [i el]))
                       (reduce (fn [board [x el]]
                                 (assoc-in board [x y] el))
                               board))]
        (recur lines (inc y) board)))))

(def input (parse "input/day11"))
(def test-input (parse "input/day11test"))
(def test-short-input (parse "input/day11testshort"))

(defn- inc-xy-on-flash [board [x y]]
  (let [val (get-in board [x y])
        will-flash? (= val 9)
        val (if (#{0 9} val)
              0
              (inc val))]
    [(assoc-in board [x y] val) will-flash?]))

(defn- near [board [x y]]
  (filter #(some? (nth % 2))
          (for [xo [-1 0 1]
                yo [-1 0 1]
                :when (not= [xo yo] [0 0])]
            (let [nx (+ x xo)
                  ny (+ y yo)]
              [nx ny (get-in board [nx ny])]))))

(defn- flash [board [x y]]
  (let [to-flash (near board [x y])]
    (loop [[tf & tfs] to-flash board board new-flashing []]
      (if (nil? tf)
        [board new-flashing]
        (let [[nx ny] tf
              [board will-flash?] (inc-xy-on-flash board [nx ny])]
          (recur tfs board (if will-flash? (conj new-flashing [nx ny]) new-flashing)))))))

(defn- inc-all [board]
  (->> board
       (map (fn [[x vs]]

              [x (into {} (map (fn [[y v]]
                                 [y (mod (inc v) 10)]) vs))]))
       (into {})))

(defn- flashing-ones [board]
  (->> board
       (mapcat (fn [[x vs]]
                 (filter some?
                         (map (fn [[y v]]
                                (when (= v 0)
                                  [x y]))
                              vs))))
       (filter some?)))

(defn- step [board flash-counter]
  (let [board (inc-all board)
        flashing (flashing-ones board)]
    (loop [[f & fs] flashing board board flash-counter (+ flash-counter (count flashing))]
      (if (nil? f)
        [board flash-counter]
        (let [[board new-flashing] (flash board f)]
          (recur (concat fs new-flashing) board (+ flash-counter (count new-flashing))))))))

(defn solve-1 []
  (let [flash-counter 0]
    (second (reduce (fn [[s flash-counter] _]
                      (step s flash-counter)) [input flash-counter] (range 100)))))

(defn all-flash? [board]
  (= (->>
      board
      (mapcat (fn [[x vs]]
                (vals vs)))
      (filter #(not= % 0))
      count) 0))

(defn solve-2 []
  (let [flash-counter 0]
    (loop [[board _] [input flash-counter] i 0]
      (if (all-flash? board)
        i
        (recur (step board flash-counter) (inc i))))))
