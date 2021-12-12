(ns day12
  (:require [clojure.string :as string]
            [clojure.set :as cset]))

(defn- parse [f]
  (->> (reduce (fn [s el]
                 (let [[f t] (string/split el #"-")
                       has-start? (or (= f "start") (= t "start"))
                       has-end? (or (= f "end") (= t "end"))]
                   (cond has-start? (update s (keyword (if (= f "start") f t)) conj (keyword (if (= f "start") t f)))
                         has-end? (update s (keyword (if (= f "end") t f)) conj (keyword (if (= f "end") f t)))
                         :else (-> s
                                   (update (keyword f) conj (keyword t))
                                   (update (keyword t) conj (keyword f))))))
               {} (-> f
                      slurp
                      (string/split #"\n")))
       (map (fn [[k v]]
              [k (into #{} v)]))
       (into {})))

(def test-input (parse "input/day12test"))
(def input (parse "input/day12"))

(def ^:dynamic can-visit-twice? false)
(defn- already-visited-lower [path]
  (let [visited (filter #(let [p (name %)]
                           (and
                            (= (.toLowerCase p) p)
                            (not (#{:start :end} %))))
                        path)
        already-visited-twice? (> (count visited) (count (into #{} visited)))]
    (if (and can-visit-twice? (not already-visited-twice?))
      (into #{})
      (into #{} visited))))

(defn- next-steps [graph path]
  (let [from (peek path)
        possible-steps (graph from)
        already-visited-lower (already-visited-lower path)]
    (cset/difference possible-steps already-visited-lower)))

(defn- step [graph path]
  (let [next-steps (next-steps graph path)]
    (when (not-empty next-steps)
      (for [s next-steps]
        (conj path s)))))

(defn- finished? [path]
  (= (peek path) :end))

(defn solve-1 []
  (loop [paths #{'(:start)} finished #{}]
    (let [possible (mapcat (partial step input) paths)
          finished (->> possible
                        (filter finished?)
                        (concat finished)
                        (into #{}))
          possible-to-take (into #{} (remove finished? possible))]
      (if (empty? possible-to-take)
        (count finished)
        (recur possible-to-take finished)))))

(defn solve-2 []
  (with-bindings {#'can-visit-twice? true}
    (solve-1)))
