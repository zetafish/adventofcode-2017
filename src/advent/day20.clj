(ns advent.day20
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; Particle Swarm

(def grammar
  "S  = p <', '> v <', '> a
  p   = <'p='> xyz
  v   = <'v='> xyz
  a   = <'a='> xyz
  <xyz> = <'<'> num <','> num <','> num <'>'>
  num = #'-?[0-9]+'
  ")

(def transform-options
  {:S vector
   :p vector
   :v vector
   :a vector
   :num read-string})

(defn parse
  [s]
  (->> (insta/parse (insta/parser grammar) s)
       (insta/transform transform-options)))

(defn parse-items
  [lines]
  (->> (mapv parse lines)
       (map-indexed vector)
       (vec)))

(def sample
  (parse-items ["p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
                "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"]))


(def input
  (parse-items (-> (io/resource "input20.txt")
                   (slurp)
                   (str/split-lines))))

(defn tick
  [[p v a]]
  [(mapv + p v a)
   (mapv + v a)
   a])

(defn manhattan
  [p]
  (reduce + (map #(Math/abs %) p)))

(defn trajectory
  [particle n]
  (->> (iterate tick particle)
       (map (comp manhattan first))
       (drop n)
       (first)))

(defn simulate
  [particles]
  (letfn [(f [col]
            (lazy-seq
              (cons (sort-by (comp manhattan first second) col)
                    (f (map (fn [[i obj]] [i (tick obj)]) col)))))]
    (f particles)))


(def simulation (simulate input))

;; Brute forcing
#_(->> simulation
     (map (comp first first))
     (drop 402)
     (take 10))
