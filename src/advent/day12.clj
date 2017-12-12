(ns advent.day12
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(def parser
  (insta/parser
    "<S> = number <spaces arrow spaces> number (<comma spaces> number)*
     arrow = '<->'
     <number> = #'[0-9]+'
     spaces = ' '+
     comma = ','"))

(def small ["0 <-> 2"
            "1 <-> 1"
            "2 <-> 0, 3, 4"
            "3 <-> 2, 4"
            "4 <-> 2, 3, 6"
            "5 <-> 6"
            "6 <-> 4, 5"])

(def input (-> (io/resource "input12.txt")
               (slurp)
               (str/split-lines)))

(defn parse-rule
  [line]
  (map #(Integer/parseInt %)
       (parser line)))

(defn edges
  [[x & xs :as rule]]
  (map vector (repeat x) xs))

(defn report
  [groups]
  {:group-count (count groups)
   :zero-count (->> groups
                    (filter #(some zero? %))
                    (first)
                    (count))})

(defn build-group
  [graph n]
  (letfn [(f [seen todo]
            (if (empty? todo)
              seen
              (let [around (apply set/union (map graph todo))]
                (recur (set/union seen around)
                       (set/difference around todo seen)))))]
    (sort (f #{} [n]))))

(defn all-groups
  [graph]
  (letfn [(f [groups unseen]
            (if (empty? unseen)
              groups
              (let [group (build-group graph (first unseen))]
                (recur (cons group groups)
                       (set/difference unseen group)))))]
    (f [] (set (keys graph)))))

(defn add-edge
  [g [a b]]
  (-> g
      (update a #(set (cons b %)))
      (update b #(set (cons a %)))))

(defn analyze
  [lines]
  (let [rules (map parse-rule lines)
        edges (mapcat edges rules)
        graph (reduce add-edge {} edges)]
    {:zero-count (count (build-group graph 0))
     :group-count (count (all-groups graph))}))

#_(analyze small)
#_(analyze input)
