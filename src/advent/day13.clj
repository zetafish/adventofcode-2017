(ns advent.day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Packet Scanner

(defn parse-int
  [s]
  (Integer/parseInt s))

(def input
  (->>
    (io/resource "input13.txt")
    (slurp)
    (str/split-lines)
    (map #(mapv parse-int (str/split % #": ")))
    (into {})))

(def example
  {0 3
   1 2
   4 4
   6 4})

(defn layer
  [n]
  (let [a (range n)
        b (drop 1 (reverse (drop 1 a)))]
    (cycle (concat a b))))

(defn make-firewall
  [spec]
  (->> spec
       (mapv (fn [[k v]] [k (layer v)]))
       (into {})))

(defn map-vals
  [f m]
  (->> m
       (map (juxt first (comp f second)))
       (into {})))

(defn inspect
  [firewall]
  (map-vals first firewall))

(defn advance
  [firewall]
  (map-vals rest firewall))

(defn simulate
  [firewall]
  (cons (inspect firewall)
        (lazy-seq (simulate (advance firewall)))))

(defn compute
  [spec]
  (let [firewall (make-firewall spec)
        n (apply max (keys firewall))]
    (->> (simulate firewall)
         (take (inc n))
         (map-indexed (fn [i m] [i m]))
         (filter (fn [[i m]] (= 0 (m i))))
         (map (fn [[i m]] (* i (spec i))))
         (reduce +))))

(compute input)
