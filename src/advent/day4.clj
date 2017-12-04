(ns advent.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn duplicate-free?
  [coll]
  (->> coll
       (frequencies)
       (vals)
       (filter #(not= 1 %))
       (empty?)))

(defn anagram-free?
  [coll]
  (->> coll
       (map sort)
       (unique?)))

(defn compute
  [check-fn coll]
  (->> coll
       (map #(str/split % #" "))
       (filter check-fn)
       (count)))


(compute duplicate-free? ["aa bb cc dd ee"])
(compute duplicate-free? ["aa bb cc dd aa"])
(compute duplicate-free? ["aa bb cc dd aaa"])

(def input (-> (io/resource "advent/input5.txt")
               (slurp)
               (str/split-lines)))

(compute duplicate-free? input)
(compute anagram-free? input)
