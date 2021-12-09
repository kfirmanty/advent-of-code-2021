(ns day9
  (:require [clojure.string :as string]))

(defn- parse [f]
  (->> (-> f slurp (string/split #"\n"))
       (map-indexed (fn [y xs]
                      [y (->> xs
                              (map-indexed (fn [x el]
                                             [x (Integer/parseInt (str el))]))
                              (into {}))]))
       (into {})))

(def input (parse "input/day9"))
(def test-input (parse "input/day9test"))

(defn- neighbours [points [x y]]
  (->>
   (for [[off-x off-y] [[-1 0] [1 0] [0 -1] [0 1]]]
     (when-let [pval (get-in points [(+ off-y y) (+ off-x x)])]
       [pval (+ off-x x)(+ off-y y)]))
   (filter some?)))

(defn low-point? [points [x y]]
  (let [point-val (get-in points [y x])
        nvals (neighbours points [x y])]
    (every? #(< point-val (first %)) nvals)))

(defn- find-low-points [points]
  (filter some?
          (for [y (-> points keys count range)
                x (-> points vals first keys count range)
                :when (low-point? points [x y])]
            [(get-in points [y x]) x y])))

(defn- print-low-points [points]
  (doseq [y (-> points keys count range)]
    (doseq [x (-> points vals first keys count range)]
      (if (low-point? points [x y])
        (print (str  (get-in points [y x]) \u0333))
        (print (get-in points [y x]))))
    (println)))

(defn solve-1 [input]
  (->> input
       find-low-points
       (map first)
       (map inc)
       (reduce +)))

(defn- bigger-neighbours [points [x y] visited valid]
  (let [pval (get-in points [y x])
        ns (neighbours points [x y])
        nvalid (->> ns
                    (filter #(not (visited %)))
                    (filter #(and (not= (first %) 9)
                                  (> (first %) pval))))
        visited (into #{} (concat visited ns))]
    (loop [[n & ns] nvalid visited visited valid (concat valid nvalid) temp-visited []]
      (if (nil? n)
        [(into #{} valid) (into #{} (concat visited temp-visited))]
        (let [[_ x y] n
              [valid-from nvisited] (bigger-neighbours points [x y] visited valid)]
          (recur ns visited (concat valid valid-from) (conj temp-visited nvisited)))))))

(defn ->basin [points [point-val x y]]
  (first (bigger-neighbours points [x y] #{[point-val x y]} [[point-val x y]])))

(defn- print-basin [points [px py]]
  (let [basin-points (->basin points [(get-in points [py px]) px py])]
    (doseq [y (-> points keys count range)]
      (doseq [x (-> points vals first keys count range)]
        (let [pval (get-in points [y x])]
          (if (basin-points [pval x y])
            (print (str pval \u0333))
            (print pval))))
      (println))))

(defn solve-2 [input]
  (->> input
       find-low-points
       (map (partial ->basin input))
       (map count)
       sort
       (take-last 3)
       (reduce *)))
