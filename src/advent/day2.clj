(ns advent.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int
  [s]
  (Integer/parseInt s))

(def input (->> (io/resource "input2.txt")
                (slurp)
                (str/split-lines)
                (map #(str/split % #"\s+"))
                (map #(map parse-int %))))

(defn checksum-1
  [row]
  (->> [max min]
       (map #(reduce % row))
       (apply -)))

(defn compute
  [f input]
  (->> input
       (map f)
       (reduce +)))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn checksum-2
  [c]
  (->> (all-pairs c)
       (map sort)
       (filter (fn [[a b]] (zero? (rem b a))))
       (first)
       (reverse)
       (apply /)))

(compute checksum-1 [[5 1 9 5]
                     [7 5 3]
                     [2 4 6 8]])

(compute checksum-1 input)


(compute checksum-2 [[5 9 2 8]
                     [9 4 7 3]
                     [3 8 6 5]])

(compute checksum-2 input)
