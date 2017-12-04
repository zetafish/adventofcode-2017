(ns advent.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/trim (slurp (io/resource "input1.txt"))))

(defn ch->int
  [ch]
  (- (int ch) (int \0)))

(defn compute
  [n s]
  (let [c (map ch->int s)
        pairs (map vector c (concat (drop n c)
                                    (take n c)))]
    (->> pairs
         (filter (fn [[a b]] (= a b)))
         (map first)
         (reduce +))))

(defn compute-1
  [s]
  (compute 1 s))

(defn compute-n
  [s]
  (compute (/ (count s) 2) s))

(compute-1 "1122")
(compute-1 "1111")
(compute-1 "1234")
(compute-1 "91212129")
(compute-1 input)

(compute-n "1212")
(compute-n "1221")
(compute-n "123425")
(compute-n "123123")
(compute-n "12131415")
(compute-n input)
