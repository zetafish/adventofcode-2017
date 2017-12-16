(ns advent.day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

;;s1, a spin of size 1: eabcd.
;;x3/4, swapping the last two programs: eabdc.
;;pe/b, swapping programs e and b: baedc.

(defn fs
  [n col]
  (let [k (- (count col) n)]
    (concat (drop k col) (take k col))))

(defn fx
  [a b col]
  (let [col (vec col)]
    (map-indexed  (fn [i v] (condp = i
                              a (get col b) b (get col a) v))
                  col)))

(defn fp
  [p q col]
  (map (fn [v] (condp = v
                 p q q p v))
       col))

(defn move
  [col [op x y]]
  (condp = op
    :s (fs x col)
    :x (fx x y col)
    :p (fp x y col)))

(defn parse-int
  [x]
  (Integer/parseInt x))

(def grammar
  "<S> = spin | exchange | partner
  <spin>     = 's' number
  <exchange> = 'x' number <'/'> number
  <partner>  = 'p' letter <'/'> letter
  <number>   = #'\\d+'
  <letter>   = #'[a-p]'")

(def parser (insta/parser grammar))

(defn parse
  [s]
  (let [[op x y] (insta/parse parser s)]
      (condp = op
        "s" [:s (parse-int x)]
        "x" [:x (parse-int x) (parse-int y)]
        "p" [:p (first x) (first y)])))

(def example "s1,x3/4,pe/b")

(def input (-> "input16.txt"
               io/resource
               slurp
               str/trim))

(defn part1
  [start moves]
  (->> (str/split moves #",")
       (map parse)
       (reduce move start)
       (str/join)))

(comment
  (println (part1 "abcde" "s1,x3/4,pe/b"))
  (println (part1 "abcdefghijklmnop" input)))
