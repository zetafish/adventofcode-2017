(ns advent.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Day 3: Spiral Memory

(def input 347991)

(defn spiral
  [[x y]]
  (let [m (max (Math/abs x) (Math/abs y))]
    (cond

      ;; on the bottom
      (= y (- m))
      [(inc x) y]

      ;; on the right
      (= x m)
      (if (< y m)
        [x (inc y)]  ;; go up if possible
        [(dec x) y]  ;; go left if top reached
        )

      ;; on the top
      (= y m)
      (if (> x (- m))
        [(dec x) y]  ;; go left if possible
        [x (dec y)]  ;; go down if left reached
        )

      ;; on the left side
      (= x (- m))
      (if (> y (- m))
        [x (dec y)]  ;; go down if possible
        [(inc x) y]  ;; go right if bottom reached
        ))))

(def d
  (->> (for [x [-1 0 1] y [-1 0 1]] [x y])
       (remove #(= [0 0] %))))

(defn neighbours
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) d))

(defn sum-neighbours
  [world [x y]]
  (->> (neighbours [x y])
       (map world)
       (filter identity)
       (reduce +)))

(defn fill
  [{:keys [world xy value]}]
  (let [xy' (spiral xy)
        world' (assoc world xy value)]
    {:world world'
     :xy xy'
     :value (sum-neighbours world' xy')}))

(defn locate
  [n]
  (->> (iterate spiral [0 0])
       (drop (dec n))
       (first)))

(defn manhattan
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))


(defn part1
  [n]
  (-> n locate manhattan))

(defn part2
  [n]
  (->> (iterate fill {:world {} :xy [0 0] :value 1})
       (drop-while #(< (:value %) n))
       (first)
       (:value)))

#_ (part1 input)
#_ (part2 input)
