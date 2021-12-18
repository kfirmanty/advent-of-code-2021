(ns day16
  (:require [clojure.string :as string]))

(defn- pad [bin-str]
  (string/replace (format "%4s" bin-str) " " "0"))

(defn- ->binary [hex]
  (-> hex
      str
      (Integer/parseInt 16)
      (Integer/toBinaryString)
      pad))

(defn- parse-input [s]
  (mapcat ->binary s))

(def input (parse-input (slurp "input/day16")))

(defn- only-zeroes-left? [binary]
  (every? #(= % \0) binary))

(defn- binary->int [binary]
  (Long/parseLong (apply str binary) 2))

(defn- eat-n [n binary & [opt-fn]]
  [(drop n binary) (cond->
                       (take n binary)
                     opt-fn opt-fn)])

(defn- eat-version [binary]
  (eat-n 3 binary binary->int))

(defn- eat-type-id [binary]
  (eat-n 3 binary binary->int))

(defn- eat-literal-value [binary]
  (loop [[binary [btype & value-part]] (eat-n 5 binary) value []]
    (let [value (concat value value-part)]
      (if (not= btype \1)
        [binary {:type :literal-value
                 :value (binary->int value)}]
        (recur (eat-n 5 binary) value)))))

(defn- literal-value? [type-id]
  (= type-id 4))

(declare eat-packet)

(defn- eat-operator [binary]
  (let [[binary length-type-id] (eat-n 1 binary binary->int)
        to-eat (if (= length-type-id 0) 15 11)
        [binary total-length] (eat-n to-eat binary binary->int)
        [binary sub-packets] (loop [binary binary left-to-read total-length sub-packets []]
                               (let [pbinary-len (count binary)
                                     [binary packet] (eat-packet binary)
                                     left-to-read (if (= length-type-id 0)
                                                    (- left-to-read (- pbinary-len (count binary)))
                                                    (dec left-to-read))]
                                 (if (= left-to-read 0)
                                   [binary (conj sub-packets packet)]
                                   (recur binary left-to-read (conj sub-packets packet)))))]
    [binary {:type :operator
             :sub-packets sub-packets}]))

(defn- eat-packet [binary]
  (let [[binary version] (eat-version binary)
        [binary type-id] (eat-type-id binary)]
    (let [[binary packet] (if (literal-value? type-id)
                            (eat-literal-value binary)
                            (eat-operator binary))]
      [binary (assoc packet
                     :version version
                     :type-id type-id)])))

(defn ->versions-sum [packet]
  (if (:sub-packets packet)
    (+ (:version packet) (reduce + (map ->versions (:sub-packets packet))))
    (:version packet)))

(defn solve-1 []
  (let [[_ outer-packet] (eat-packet input)]
    (->versions-sum outer-packet)))

(defn- calculate [{:keys [sub-packets type-id] :as packet}]
  (if sub-packets
    (let [subpackets-vals (map calculate sub-packets)
          comp-fn (fn [comp-fn]
                    (fn [[s1 s2]]
                      (if (comp-fn s1 s2)
                        1
                        0)))
          op (case type-id
               0 (partial reduce +)
               1 (partial reduce *)
               2 (partial apply min)
               3 (partial apply max)
               5 (comp-fn >)
               6 (comp-fn <)
               7 (comp-fn =))]
      (op subpackets-vals))
    (:value packet)))

(defn solve-2 []
  (let [[_ outer-packet] (eat-packet input)]
    (calculate outer-packet)))
