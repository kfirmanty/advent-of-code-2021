(ns day15
  (:require [clojure.string :as string]))

(defn- parse [f]
  (let [lines (-> f slurp (string/split #"\n"))]
    (->> lines
         (map-indexed (fn [y els]
                        (map-indexed (fn [x v]
                                       {x {y (Integer/parseInt (str v))}}) els)))
         (apply concat)
         (apply merge-with conj))))

(def test-input (parse "input/day15test"))
(def input (parse "input/day15"))

(defn- last-step [path]
  (last path))

(def all-visited (atom #{}))

(defn- winning? [board path]
  (let [last-step (last-step path)
        board-winning [(->>
                        board
                        keys
                        (apply max))
                       (->> board
                            vals
                            first
                            keys
                            (apply max))]]
    (= last-step board-winning)))

(defn path->cost [board path]
  (->> path
       (map #(get-in board %))
       (reduce +)))

(defn- next-steps [board path]
  (let [[x y] (last-step path)
        path-set (into #{} path)]
    (->> (for [[xoff yoff] [[-1 0] [0 -1] [1 0] [0 1]]]
           (let [nx (+ x xoff)
                 ny (+ y yoff)]
             (when (and (get-in board [nx ny])
                        (not (path-set [nx ny])))
               [nx ny])))
         (filter some?)
         (map (partial conj path)))))

(defn- prune [board paths path]
  (let [path-last-step (last-step path)
        path-total-cost (path->cost board path)]
    (loop [[p & ps] paths]
      (if (nil? p) path
          (let [p-last-step (last-step p)
                ]
            (if (and (= p-last-step path-last-step)
                     (> path-total-cost (path->cost board p)))
              nil
              (recur ps)))))))

(defn- breadth-first [board paths]
  (->> paths
       (mapcat (partial next-steps board))
       (remove #(@all-visited (last-step %)))
       (filter not-empty)
       (map (partial prune board paths)) ;;filter those that has the same last-step as one from the paths but higher cost
       (filter some?)))

(defn- draw-paths [board paths]
  (let [visited-steps (->> paths
                           (apply concat)
                           (into #{}))]
    (doseq [y (-> board keys count range)]
      (doseq [x (-> board vals first keys count range)]
        (let [pval (get-in board [x y])]
          (if (visited-steps [x y])
            (print (str pval \u0333))
            (print pval))))
      (println))
    (Thread/sleep 500)))

(defn solve-1 []
  (reset! all-visited #{})
  (let [board input]
    (loop [paths (breadth-first board #{[[0 0]]}) winning #{}]
      #_(draw-paths board paths)
      (if (empty? paths)
        (- (->> winning
               (map (partial path->cost board))
               (sort)
               first) (get-in board [0 0]))
        (let [all-paths (breadth-first board paths)
              _ (swap! all-visited #(into #{} (concat % (apply concat all-paths))))
              all-paths (->> all-paths
                             (map (partial prune board all-paths))
                             (filter some?))
              new-winning (filter (partial winning? board) all-paths)
              new-paths (remove (partial winning? board) all-paths)]
          (recur (into #{} new-paths) (into #{} (concat winning new-winning))))))))
