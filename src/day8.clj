(ns day8
  (:require [clojure.string :as string]
            [clojure.set :as cset]))

(def mappings {0 "abcefg"
               1 "cf"
               2 "acdeg"
               3 "acdfg"
               4 "bcdf"
               5 "abdfg"
               6 "abdefg"
               7 "acf"
               8 "abcdefg"
               9 "abcdfg"})

(defn- possible [wires]
  (->> mappings
       (filter (fn [[d segments]]
                 (= (count segments) (count wires))))))

(defn- parse [line]
  (let [digits (-> line
                   (string/replace "|" "")
                   (string/split #"\s+"))]
    [(drop-last 4 digits) (take-last 4 digits)]))

(def test-short-input
  (parse "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf"))

(def input (map parse (-> "input/day8" slurp (string/split #"\n"))))

(defn solve-1 []
  (->> input
       (map second)
       (mapcat #(map possible %))
       (filter #(= (count %) 1))
       count))

(defn wires->segments [wires]
  (let [possible (possible wires)]
    (into {} (map (fn [c]
                    [c (->> possible
                            (mapcat second)
                            (into #{}))]) wires))))

(defn set-wire->segment [possibilities wire segment]
  (assoc (->> possibilities
              (map (fn [[w segs]]
                     [w (if (set? segs)
                          (cset/difference segs #{segment})
                          segs)]))
              (into {}))
         wire segment))

(defn substitute-clear-ones [possibilities]
  (loop [possibilities possibilities]
    (let [todo (->> possibilities
                    (filter (fn [[wire segments]]
                              (and (set? segments) (= (count segments) 1)))))]
      (if (not-empty todo)
        (let [[[wire segments] & todo] todo]
          (recur (set-wire->segment possibilities wire (first segments))))
        possibilities))))

(defn solved? [possibilities]
  (->> possibilities
       (filter (fn [[wire segments]]
                 (set? segments)))
       empty?))

(defn failed-state? [possible]
  (or (->> possible
           (filter (fn [[wire segments]]
                     (and (set? segments) (= (count segments) 0))))
           not-empty)
      (->> possible
           (filter (fn [[wire segments]]
                     (and (set? segments) (#{1 2} (count segments)))))
           empty?)))

(defn with-lowest-guess-needed [possible]
  (let [lowest-guess (->> possible
                          (filter (fn [[wire segments]]
                                    (set? segments)))
                          (map (comp count second))
                          sort
                          first)]
    (->> possible
         (filter (fn [[wire segments]]
                   (and (set? segments) (= (count segments) lowest-guess)))))))

(defn guess [possible]
  (let [possible (substitute-clear-ones possible)]
    (cond
      (solved? possible) possible
      (failed-state? possible) nil
      :else (let [guesses (with-lowest-guess-needed possible)]
              (for [g guesses]
                (let [[wire segments] g]
                  (map
                   #(guess (set-wire->segment possible wire %))
                   segments)))))))

(defn str->number [input subs]
  (let [mapped (apply str (map subs input))]
    (->> mappings
         (filter (fn [[num segments]]
                   (= (sort segments) (sort mapped))))
         first
         first)))

(defn proper-substitutions? [input substitutions]
  (let [substituted (->> (map #(str->number % substitutions) input)
                         (filter some?))]
    (= (count substituted) (count input))))

(defn solve-part [input]
  (let [possible (->> input
                      first
                      (map wires->segments)
                      (apply merge-with cset/intersection))
        substitutions (if (solved? possible)
                        [possible]
                        (->> possible guess flatten (into #{})))
        proper-substitution (first (filter (partial proper-substitutions? (first input)) substitutions))]
    (map #(str->number % proper-substitution) (second input))))

(defn solve-2 []
  (->> input
       (map solve-part)
       (map #(apply str %))
       (map #(Integer/parseInt %))
       (reduce +)))
