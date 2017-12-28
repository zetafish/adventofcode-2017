(ns advent.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- Day 22: Sporifica Virus ---

(def turn-right
  {[1 0] [0 -1]
   [0 -1] [-1 0]
   [-1 0] [0 1]
   [0 1] [1 0]})

(def turn-left
  {[1 0] [0 1]
   [0 1] [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]})

(defn make-turn
  [{:keys [grid pos dir] :as state}]
  (if (grid pos)
    (update state :dir turn-right)
    (update state :dir turn-left)))

(defn toggle-infection
  [{:keys [grid pos] :as state}]
  (if (grid pos)
    (update state :grid disj pos)
    (-> state
        (update :grid conj pos)
        (update :infections inc))))

(defn move-forward
  [{:keys [grid pos dir] :as state}]
  (update state :pos #(mapv + % dir)))

(defn step
  [{:keys [grid pos dir] :as state}]
  (-> state
      make-turn
      toggle-infection
      move-forward))

(defn draw
  [n {:keys [grid pos dir] :as state}]
  (letfn [(cell [p] (let [c (if (grid p) "#" ".")]
                      (cond
                          (= p pos) (str "[" c "]")
                          (= p (mapv + pos dir)) (str "(" c ")")
                          :else (str " " c " "))))]
    (println state)
    (println
      (str/join "\n"
                (for [y (range n (dec (- n)) -1)]
                  (str/join
                    (for [x (range (- n) (inc n))]
                      (cell [x y]))))))))

(defn simulate
  [n seed]
  (->> (iterate step seed)
       (drop n)
       (first)))

(make-grid (mapv vec ["..#" "#.." "..."]))

(def seed
  {:grid (make-grid (mapv vec ["..#" "#.." "..."]))
   :pos [0 0]
   :dir [0 1]
   :infections 0})


(draw 4 (simulate 0 seed))
(draw 4 (simulate 1 seed))
(draw 4 (simulate 2 seed))
(draw 4 (simulate 3 seed))
(draw 4 (simulate 4 seed))
(draw 4 (simulate 5 seed))
(draw 4 (simulate 6 seed))

(draw 4 (simulate 7 seed))
(draw 4 (simulate 70 seed))
(draw 4 (simulate 10000 seed))

(def input (->> (io/resource "input22.txt")
                (slurp)
                (str/split-lines)
                (mapv vec)))

(defn make-grid
  [g]
  (let [n (count g)
        g (vec (reverse g))]
    (set
          (for [x (range n)
                y (range n)
                :when (= \# (get-in g [y x]))]
            [(- x (quot n 2)) (- y (quot n 2))]))))


(def seed
  {:grid (make-grid input)
   :pos [0 0]
   :dir [0 1]
   :infections 0})

(simulate 10000 seed)
