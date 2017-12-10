(ns advent.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/trim (slurp (io/resource "input1.txt"))))

(defn ->digits
  [s]
  (map #(- (int %) (int \0)) s))

(defn rotate
  [n coll]
  (concat (drop n coll) (take n coll)))

(defn compute
  [n s]
  (let [d (->digits s)]
    (->>
      (map vector d (rotate n d))
      (filter (fn [[a b]] (= a b)))
      (map first)
      (reduce +))))

(defn part-1
  [s]
  (compute 1 s))

(defn part-2
  [s]
  (compute (/ (count s) 2) s))

#_ (part-1 input)
#_ (part-2 input)
