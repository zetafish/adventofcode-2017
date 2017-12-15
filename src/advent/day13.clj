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

(defn position
  [tick layer-size]
  (let [half-cycle (dec layer-size)
        full-cycle (* 2 half-cycle)
        tick (mod tick full-cycle)]
    (if (< tick layer-size)
      tick
      (- full-cycle tick))))

(defn compute
  [firewall delay]
  (let [hits (->> firewall
                  (filter (fn [[k v]]
                            (zero? (position (+ delay k) v))))
                  (map (fn [[k v]] (* k v))))]
    {:caught (pos? (count hits))
     :severity (reduce + hits)}))

(defn map-vals
  [f m]
  (into {} (map (juxt first (comp f second)) m)))

(defn find-sneaky-delay
  [firewall]
  (->> (iterate inc 0)
       (map #(compute firewall %))
       (take-while :caught)
       (count)))

(compute example 0)
(compute input 0)

(find-sneaky-delay example)
(find-sneaky-delay input)
