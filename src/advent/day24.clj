(ns advent.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 24: Electromagnetic Moat ---

(def input
  (->> (-> (io/resource "input24.txt")
           (slurp)
           (str/split-lines))
       (map #(str/split % #"/"))
       (map #(map read-string %))
       (into #{})))

(def sample
  (->> [[0 2] [2 2] [2 3] [3 4] [3 5] [0 1] [10 1] [9 10]]
       (into #{})))

(defn fit?
  [port component]
  (some #(= port %) component))

(defn connect
  [port [a b]]
  (if (= a port)
    b
    a))

(defn strength
  [bridge]
  (->> bridge
       (map #(apply + %))
       (reduce +)))

(defn expand
  [available {:keys [port chain]}]
  (->> (set/difference available chain)
       (filter #(fit? port %))
       (map #(hash-map :port (connect port %)
                       :chain (cons % chain)))))

(defn generate
  [available from]
  (let [expansion (mapcat #(expand available %) from)]
    (if (seq expansion)
      (concat from (generate available expansion))
      from)))

(->> (generate sample [{:port 0}])
     (map (comp strength :chain))
     (reduce max))

(def bridges
  (->> (generate input [{:port 0}])
       (map :chain)))


(->> bridges
     (map (juxt count strength))
     (sort)
     (reverse)
     (take 10))
