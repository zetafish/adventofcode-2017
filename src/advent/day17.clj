(ns advent.day17
  (:require [clojure.string :as str]))

(defn spinlock
  [step {:keys [buffer pos]}]
  (let [n (count buffer)
        next-pos (inc (rem (+ step pos) n))
        [x y] (split-at next-pos buffer)]
    {:buffer (doall (concat x [n] y))
     :pos next-pos}))

(defn part1
  [step]
  (let [{:keys [buffer pos] :as state}
        (->> (iterate (partial spinlock step) {:buffer [0] :pos 0})
             (drop 2017)
             (first))]
    (nth buffer (inc pos))))

(defn spinlock2
  [step {:keys [size pos at-one]}]
  (let [next-pos (inc (rem (+ step pos) size))]
    {:size (inc size)
     :pos next-pos
     :at-one (if (= 1 next-pos) size at-one)}))

(defn part2
  [step]
  (->> {:size 1 :pos 0}
       (iterate (partial spinlock2 step))
       (drop 50000000)
       (first)
       :at-one))

(part1 366)
(part2 366)
