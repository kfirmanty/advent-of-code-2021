(ns day13
  (:require [clojure.string :as string]))

(defn- draw [points]
  (let [max-x (->> points
                   (map first)
                   (apply max))
        max-y (->> points
                   (map second)
                   (apply max))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (if (not-empty (filter #{[x y]} points))
          (print "#")
          (print ".")))
      (print "\n"))))

(defn- fold-instruction? [line]
  (string/includes? line "fold"))

(defn- parse-fold [line]
  (let [[dim pos] (-> line
                      (string/replace "fold along " "")
                      (string/split #"="))]
    {:dim (keyword dim)
     :pos (Integer/parseInt pos)}))

(defn- parse-point [line]
  (map #(Integer/parseInt %) (string/split line #",")))

(defn- parse [f]
  (let [lines (-> f
                  slurp
                  (string/split #"\n+"))
        folds (filter fold-instruction? lines)
        points (remove fold-instruction? lines)]
    {:folds (map parse-fold folds)
     :points (->> points
                  (map parse-point)
                  #_(reduce (fn [s el]
                              (assoc-in s el true)) {}))}))

(def test-input (parse "input/day13test"))
(def input (parse "input/day13"))

(defn- fold [{:keys [dim pos]} points]
  (let [op-fn (if (= dim :x)
                (fn [[x y]]
                  (if (> x pos)
                    [(- pos (Math/abs (- x pos))) y]
                    [x y]))
                (fn [[x y]]
                  (if (> y pos)
                    [x (- pos (Math/abs (- y pos)))]
                    [x y])))]
    (->> points
         (map op-fn)
         (into #{}))))

(defn solve-1 []
  (let [{:keys [folds points]} input
        npoints (fold (first folds) points)]
    (count npoints)))

(defn solve-2 []
  (let [{:keys [folds points]} input]
    (loop [[f & fs] folds points points]
      (if (nil? f)
        (draw points)
        (recur fs (fold f points))))))
