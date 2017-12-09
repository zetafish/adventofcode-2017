(ns advent.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(defn parse-line
  [s]
  (let [[name weight _ & above] (filter (complement empty?) (str/split s #" |,"))
        weight (Integer/parseInt (re-find #"\d+" weight))]
    {:name name
     :weight weight
     :above above}))

(defn find-bottom
  [programs]
  (let [names (map :name programs)
        non-bottom (mapcat :above programs)]
    (first (set/difference (set names) (set non-bottom)))))

(def input (->> (io/resource "input7.txt")
                (slurp)
                (str/split-lines)
                (map parse-line)))

(def nodes (->> input
                (map (juxt :name identity))
                (into {})))

(defn above
  [n]
  (-> n nodes :above))

(defn weight
  [n]
  (-> n nodes :weight))


(defn total
  [n]
  (reduce +
          (weight n)
          (map total (above n))))

(defn find-bad
  [n]
  (let [a (above n)
        t (map total a)
        v (->> (frequencies t)
               (filter #(= 1 (second %)))
               (first)
               (first))]
    (->> (interleave a t)
         (partition 2)
         (filter #(= v (second %)))
         (first))))

(defn balanced?
  [n]
  (= 1 (->> (above n)
          (map total)
          (frequencies)
          (count))))

(defn find-fix
  [n]
  (let [a (above n)
        t (map total a)
        x (->> (interleave t a)
               (partition 2)
               (map vec)
               (into {}))
        f (frequencies t)
        good-value (first (first (filter #(not= 1 (second %)) f)))
        bad-value (first (first (filter #(= 1 (second %)) f)))
        to-add (- good-value bad-value)]
    [(x bad-value) to-add]))


(defn fix-tree
  [node k]
  (if (balanced? node)
    (+ k (weight node))
    (let [[n2 k2] (find-fix node)]
      (recur n2 k2))))

(fix-tree bottom 0)
