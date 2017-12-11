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

(defn compute-1
  [s]
  (->>
    (parse s)
    (reduce step [0 0])
    (map #(Math/abs %))
    (reduce +)
    (* 0.5)))

(compute-1 "ne,ne,ne")
(compute-1 "ne,ne,sw,sw")
(compute-1 "ne,ne,s,s")
(compute-1 "se,sw,se,sw,sw")
(compute-1 input)
