(ns advent.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Day 3: Spiral Memory

(def input 347991)

(defn integrate
  [a [x & xs]]
  (lazy-seq (cons a (integrate (+ a x) xs))))

(def corners
  (->> (cycle [0 0 2 0])
       (integrate 0)
       (integrate 1)))

(defn distance-to-portal
  [n]
  (let [[a b] (split-with #(< % n) corners)
        ring (-> a count (quot 4))
        bound (first b)
        to-axis (Math/abs (- bound ring n))]
    (+ to-axis ring)))

;; Part 1
(distance-to-portal input)
(distance-to-portal 12)
(distance-to-portal 12345678901)


(def dirs (cycle [[1 0] [0 1] [-1 0] [0 -1]]))

(def lengths (integrate 1 (cycle [0 1])))

(defn stride
  [from dir n]
  (->> (iterate #(mapv + % dir) from)
       (drop 1)
       (take n)))

(defn spiral
  ([] (spiral [0 0]))
  ([pos] (cons pos (spiral pos dirs lengths)))
  ([pos dirs lengths]
   (let [s (stride pos (first dirs) (first lengths))]
     (concat s (lazy-seq (spiral (last s)
                                 (rest dirs)
                                 (rest lengths)))))))

(defn neighbours
  [pos]
  (->> (for [x [-1 0 1]
             y [-1 0 1]
             :when (not (and (zero? x) (zero? y)))] [x y])
       (map #(map + % pos))))

(defn count-tokens
  [world places]
  (->> (map world places)
       (filter identity)
       (reduce +)))

(defn walk-spiral
  ([] (cons 1 (walk-spiral (rest (spiral)) {[0 0] 1})))
  ([spiral world]
   (let [pos (first spiral)
         value (count-tokens world (neighbours pos))]
     (cons value (lazy-seq (walk-spiral (rest spiral)
                                        (assoc world pos value)))))))

;; Part 2
(->> (walk-spiral)
     (drop-while #(<= % input))
     (first))
