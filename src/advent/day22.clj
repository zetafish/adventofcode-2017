(ns advent.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 22: Sporifica Virus ---

(def lifecycle
  {:clean :weakened
   :weakened :infected
   :infected :flagged
   :flagged :clean})

(defn turn-right
  [[x y]]
  [y (- x)])

(defn turn-left
  [[x y]]
  [(- y) x])

(defn reverse-dir
  [[x y]]
  [(- x) (- y)])

(defn turn
  [{:keys [grid pos dir] :as state}]
  (condp = (grid pos :clean)
    :infected (update state :dir turn-right)
    :clean (update state :dir turn-left)
    :weakened state
    :flagged (update state :dir reverse-dir)))

(defn evolve
  [m {:keys [grid pos] :as state}]
  (let [v (grid pos :clean)]
    (-> state
        (update :grid assoc pos (m v))
        (update v inc))))

(defn move
  [{:keys [grid pos dir] :as state}]
  (update state :pos #(mapv + % dir)))

(defn step
  [m {:keys [grid pos dir] :as state}]
  (->> state
       (turn)
       (evolve m)
       (move)))

(defn make-grid
  [g]
  (let [n (count g)
        g (vec (reverse g))]
    (into {}
          (for [x (range n)
                y (range n)
                :when (= \# (get-in g [y x]))]
            [[(- x (quot n 2)) (- y (quot n 2))]
             :infected]))))

(defn draw
  [n {:keys [grid pos dir] :as state}]
  (letfn [(cell [p] (let [c (condp = (grid p :clean)
                              :infected \#
                              :clean \.
                              :weakened \W
                              :flagged \F)]
                      (cond
                          (= p pos) (str "[" c "]")
                          (= p (mapv + pos dir)) (str "(" c ")")
                          :else (str " " c " "))))]
    (println (dissoc state :grid))
    (println
      (str/join "\n"
                (for [y (range n (dec (- n)) -1)]
                  (str/join
                    (for [x (range (- n) (inc n))]
                      (cell [x y]))))))))

(def toggle {:clean :infected
             :infected :clean})

(def evolution {:clean :weakened
                :weakened :infected
                :infected :flagged
                :flagged :clean})

(defn simulate-1
  [n seed]
  (->> (iterate (partial step toggle) seed)
       (drop n)
       (first)))

(defn simulate-2
  [n seed]
  (->> (iterate (partial step evolution) seed)
       (drop n)
       (first)))

(def simulate simulate-2)

(def seed
  {:grid (make-grid (mapv vec ["..#" "#.." "..."]))
   :pos [0 0]
   :dir [0 1]
   :infected 0 :clean 0 :flagged 0 :weakened 0})


(draw 4 (simulate 0 seed))
(draw 4 (simulate 1 seed))
(draw 4 (simulate 2 seed))
(draw 4 (simulate 3 seed))
(draw 4 (simulate 4 seed))
(draw 4 (simulate 5 seed))
(draw 4 (simulate 6 seed))
(draw 4 (simulate 7 seed))
(draw 4 (simulate 100 seed))
(draw 4 (simulate 10000 seed))
(draw 4 (simulate 10000000 seed))

(def input (->> (io/resource "input22.txt")
                (slurp)
                (str/split-lines)
                (mapv vec)))


(def seed
  {:grid (make-grid input)
   :pos [0 0]
   :dir [0 1]
   :infected 0 :clean 0 :flagged 0 :weakened 0})

(println (dissoc (simulate 10000000 seed) :grid))
