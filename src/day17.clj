(ns day17
  (:require [clojure.string :as string]
            [clojure.set :as cset]))

(defn parse [f]
  (let [descs (-> f
                  slurp
                  (string/replace "target area: x=" "")
                  (string/split #", y="))]
    (map (fn [d]
           (map #(Integer/parseInt %) (string/split d #"\.\."))) descs)))

(def test-input (parse "input/day17test"))
(def input (parse "input/day17"))

(defn x-vel [xvel]
  (cond (> xvel 0) (dec xvel)
        (< xvel 0) (inc xvel)
        :else 0))

(defn y-vel [yvel]
  (dec yvel))

(defn overshoot? [[px py] [x1 x2] [y1 y2]]
  (or (> px x2)
      (< py y1)))

(defn in-target? [[px py] [x1 x2] [y1 y2]]
  (and (<= x1 px x2)
       (>= y2 py y1)))

(defn shoot [xvel yvel [x1 x2 :as xs] [y1 y2 :as ys]]
  (loop [xvel xvel yvel yvel [px py :as pos] [0 0] highest-y yvel]
    (cond (in-target? pos xs ys) highest-y
          (overshoot? pos xs ys) nil
          :else (recur (x-vel xvel) (y-vel yvel) [(+ px xvel) (+ py yvel)] (max highest-y (+ py yvel))))))

(defn xy-shots [possible-x-vels xs ys]
  (->> (for [x-vel possible-x-vels]
         (for [y-vel (range (first ys) 1000)]
           (let [high-y (shoot x-vel y-vel xs ys)]
             (when high-y
               [x-vel y-vel high-y]))))
       (apply concat)
       (filter some?)))

(def ->x-range
  (memoize (fn [init-vel]
             (loop [vel init-vel xs [0] prev-val 0]
               (if (= vel 0)
                 (into #{} xs)
                 (recur (x-vel vel) (conj xs (+ prev-val vel)) (+ prev-val vel)))))))

(defn simulate-towards [x init-vel fx]
  (loop [pos x vel init-vel]
    (cond (= pos fx) true
          (or (<= pos 0 fx) (>= fx 0 pos) (= vel 0)) false
          :else (recur (+ pos vel) (x-vel vel)))))

(defn find-possible-x-vels [x1 x2]
  (let [possible-positions (into #{} (range x1 (inc x2)))]
    (->> (range (inc x2))
         (map (fn [vel]
                [vel (->x-range vel)]))
         (filter (fn [[vel pos]]
                   (not-empty (cset/intersection possible-positions pos))))
         (map first))))

(defn solve-1 []
  (let [[sx sy] [0 0]
        [[x1 x2] ys] input
        possible-x-vels (find-possible-x-vels (Math/abs x1) (Math/abs x2))]
    (->> (xy-shots possible-x-vels [x1 x2] ys)
         (map #(nth % 2))
         (apply max))))

(defn solve-2 []
  (let [[sx sy] [0 0]
        [[x1 x2] ys] input
        possible-x-vels (find-possible-x-vels (Math/abs x1) (Math/abs x2))]
    (->> (xy-shots possible-x-vels [x1 x2] ys)
         (map (partial take 2))
         (into #{})
         count)))
