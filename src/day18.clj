(ns day18
  (:require [clojure.string :as string]
            [clojure.zip :as zip]))

(defn- split [n]
  (let [n2 (/ n 2)]
    [(int (Math/floor n2)) (int (Math/ceil n2))]))

(defn- parse [f]
  (mapv read-string
        (-> f
            slurp
            (string/split #"\n"))))

(def short-test-input (parse "input/day18shorttest"))
(def test-input (parse "input/day18test"))
(def input (parse "input/day18"))

(defn- regular-number? [z]
  (-> z zip/node number?))

(defn- first-regular [z dir-fn]
  (loop [z (dir-fn z)]
    (when-not (or (nil? z) (zip/end? z))
      (if (regular-number? z)
        z
        (recur (dir-fn z))))))

(defn- first-regular-left [z]
  (first-regular z zip/prev))

(defn- first-regular-right [z]
  (first-regular z zip/next))

(defn- explode [z]
  (let [[lv rv] (zip/node z)
        update-left (fn [z]
                      (if-let [rz (first-regular-left z)]
                        (first-regular-right (zip/edit rz + lv))
                        z))
        update-right (fn [z]
                       (if-let [rz (first-regular-right z)]
                         (zip/edit rz + rv)
                         z))]
    (-> z
        (zip/replace 0)
        update-left
        update-right)))

(defn- changed? [z]
  (-> z second :changed?))

(defn- pnodes [z]
  (-> z second :pnodes))

(defn- one-operation [z predicate op-fn]
  (loop [z z]
    (cond (predicate z) (let [z (op-fn z)]
                          (if (changed? z)
                            (zip/vector-zip (zip/root z))
                            (recur (zip/next z))))
          (zip/end? z) :noop
          :else (recur (zip/next z)))))

(defn- one-explode-operation [z]
  (one-operation z
                 #(= (count (pnodes %)) 4)
                 #(if (vector? (zip/node %))
                    (explode %)
                    %)))

(defn- one-split-operation [z]
  (one-operation z
                 #(and (regular-number? %) (>= (zip/node %) 10))
                 #(zip/edit % split)))

(defn- print-debug [z nz nz2]
  (println (cond (not= nz :noop) (str "after explode: " nz)
                 (not= nz2 :noop) (str "after split: " nz2)
                 :else (str "after noop: " z))))

(defn- reduce-sailfish [z]
  (loop [z z]
    (let [nz (one-explode-operation z)
          nz2 (if (= nz :noop)
                (one-split-operation z)
                :noop)]
      #_(print-debug z nz nz2)
      (cond (and (= :noop nz) (= :noop nz2)) (first z)
            (not= :noop nz) (recur nz)
            (not= :noop nz2) (recur nz2)))))

(defn magnitude [s]
  (if (vector? s)
    (let [[n1 n2] s]
      (+ (* (magnitude n1) 3)
         (* (magnitude n2) 2)))
    s))

(defn solve-1 []
  (magnitude (reduce (fn [n1 n2]
                       (reduce-sailfish (zip/vector-zip [n1 n2]))) input)))

(defn solve-2 []
  (->> (for [n1 input
             n2 input
             :when (not= n1 n2)]
         (reduce-sailfish (zip/vector-zip [n1 n2])))
       (map magnitude)
       (apply max)))
