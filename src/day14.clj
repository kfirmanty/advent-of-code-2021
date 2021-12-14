(ns day14
  (:require [clojure.string :as string]))

(defn parse [f]
  (let [[polymer & rules] (-> f slurp (string/split #"\n+"))]
    [polymer (->> rules
                  (map (fn [sub]
                         (string/split sub #" -> ")))
                  (into {}))]))

(def test-input (parse "input/day14test"))

(def input (parse "input/day14"))

(defn- substitute [polymer rules]
  (let [l (- (count polymer) 2)]
    (->> polymer
         (partition 2 1)
         (map-indexed (fn [i [a b :as part]]
                        (str a (get rules (apply str part)) (when (= i l) b))))
         (apply str))))

(defn solve-1 []
  (let [[polymer rules] test-input
        seen (atom #{})
        final (reduce (fn [s _]
                        (substitute s rules)) polymer (range 20))
        freqs (->> final frequencies vals sort)]
    (- (last freqs) (first freqs))))

(defn- iterate-freqs [freqs rules]
  (->> freqs
       (mapcat (fn [[[a b :as k] v]]
                 [[(str a (get rules (apply str k))) v]
                  [(str (get rules (apply str k)) b) v]]))
       (group-by first)
       (map (fn [[k vs]]
              [k (reduce + (map second vs))]))
       (into {})))

(defn solve-2 []
  (let [[polymer rules] input
        [fchar lchar] ((juxt first last) polymer)
        inc-fn (if (= fchar lchar) (partial + 2) inc)
        polymer-freqs (->> polymer
                           (partition 2 1)
                           (map (partial apply str))
                           frequencies)
        final (reduce (fn [s _] (iterate-freqs s rules)) polymer-freqs (range 40))
        freqs (->> final
                   (mapcat (fn [[[a b] v]]
                             [[a v]
                              [b v]]))
                   (group-by first)
                   (map (fn [[k vs]]
                          (let [c (reduce + (map second vs))]
                            [k (if ((into #{} [fchar lchar]) k)
                                 (inc-fn c)
                                 c)])))
                   (map (fn [[c v]] [c (/ v 2)]))
                   (sort-by second))]
    (- (-> freqs last second) (-> freqs first second))))
