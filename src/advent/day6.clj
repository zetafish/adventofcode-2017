(ns advent.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def banks [0 2 7 0])

(def input [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])

(defn max-by
  ([f x] x)
  ([f x y] (if (> (f y) (f x)) y x))
  ([f x y & more]
   (reduce (partial max-by f) (max-by f x y) more)))

(defn find-max-indexed
  [s]
  (apply max-by second (map-indexed vector s)))

(defn distribute-remainder
  [value from size]
  (->> (repeat (mod value size) 1)
                  (map-indexed (fn [i x] [(mod (+ 1 i from) size) x]))
                  (into {})))

(defn step
  [banks]
  (let [[from value] (find-max-indexed banks)
        size (count banks)
        more (distribute-remainder value from size)]
    (->> (assoc banks from 0)
         (map #(+ % (quot value size)))
         (map-indexed (fn [i x] (+ x (get more i 0))))
         (vec))))

(defn run-debugger
  [f banks]
  (loop [seen #{}
         order []
         banks banks]
    (if (seen banks)
      order
      (recur (conj seen banks) (conj order banks) (step banks)))))

(count (run-debugger step [0 2 7 0]))
(count (run-debugger step input))

(->> (run-debugger step input)
     (last)
     (run-debugger step)
     (count))
