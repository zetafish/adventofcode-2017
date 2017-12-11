(ns advent.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Day 11 - Crossing the bridge

(def input (str/trim (slurp (io/resource "input11.txt"))))

(defn parse
  [s]
  (map keyword (str/split s #",")))

(defn step
  [pos d]
  (let [delta (d {:n [0 2] :s [0 -2]
                  :e [2 0] :w [-2 0]
                  :ne [1 1] :se [1 -1]
                  :nw [-1 1] :sw [-1 -1]})]
    (mapv + pos delta)))

(defn dist
  [[x y]]
  (/ (+ (Math/abs x) (Math/abs y)) 2))

(defn furthest
  [p q]
  (if (> (dist p) (dist q))
    p
    q))

(defn step-2
  [{:keys [pos far-away]} d]
  (let [next-pos (step pos d)]
    {:pos next-pos
     :far-away (furthest far-away next-pos)}))

(defn compute
  [s]
  (->>
    (parse s)
    (reduce step-2 {:pos [0 0]
                    :far-away [0 0]})
    (vals)
    (map dist)))


(compute "ne,ne,ne")
(compute "ne,ne,sw,sw")
(compute "ne,ne,s,s")
(compute "se,sw,se,sw,sw")
(compute input)
