(ns day20
  (:require [clojure.string :as string]))

(defn- parse [f]
  (let [[lookup & r] (-> f slurp (string/split #"\n+"))]
    {:lookup lookup
     :board (apply merge-with merge (for [x (range (-> r first count))
                                          y (range (count r))]
                                      {x {y (-> r (nth y) (nth x))}}))}))

(def test-input (parse "input/day20test"))
(def input (parse "input/day20"))

(defn- pixel->number [lookup board outside-this-turn x y]
  (Integer/parseInt (apply str (for [y (range (dec y) (+ y 2))]
                                 (apply str (for [x (range (dec x) (+ x 2))]
                                              ({\. 0
                                                \# 1}
                                               (get-in board [x y] outside-this-turn))))))
                    2))

(defn- print-board [board]
  (doseq [y (range (-> board vals first count))]
    (doseq [x (range (-> board keys count))]
      (print (get-in board [x y] \.)))
    (println)))

(def outside-this-turn (atom \#))
(defn- decode [lookup board]
  (swap! outside-this-turn {\# \.
                            \. \#})
  (let [off 3]
    (into {} (for [x (range (- off) (+ (-> board keys count) off))]
               {(+ x off) (into {}
                                (for [y (range (- off) (+ (-> board vals first count) off))]
                                  (let [num (pixel->number lookup board @outside-this-turn x y)]
                                    [(+ y off) (nth lookup num)])))}))))

(defn- solve [turns]
  (let [{:keys [lookup board]} input
        board (->> (iterate (partial decode lookup) board)
                   (take turns)
                   last)]
    (->> board
         (mapcat (fn [[x vs]]
                   (map (fn [[y v]]
                          v) vs)))
         (filter #(= % \#))
         count)))

(defn solve-1 []
  (solve 3))

(defn solve-2 []
  (solve 51))
