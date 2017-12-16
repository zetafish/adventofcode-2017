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
  (str/join
    (condp = op
      :s (fs x col)
      :x (fx x y col)
      :p (fp x y col))))

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

(defn parse-move
  [s]
  (let [[op x y] (insta/parse parser s)]
      (condp = op
        "s" [:s (parse-int x)]
        "x" [:x (parse-int x) (parse-int y)]
        "p" [:p (first x) (first y)])))

(def moves-a (map parse-move (str/split "s1,x3/4,pe/b" #",")))

(def moves-b (map parse-move
                  (-> (io/resource "input16.txt")
                      (slurp)
                      (str/trim)
                      (str/split #","))))

(defn dance
  [moves start]
  (reduce move start moves))

;; part 1
(dance moves-a "abcde") ;; baedc
(dance moves-b "abcdefghijklmnop") ;; => "ebjpfdgmihonackl"

(defn find-cycle-length
  [moves start]
  (->> (iterate (partial dance moves) start)
       (drop 1)
       (take-while #(not= start %))
       (cons start)
       (count)))

(defn dance2
  [moves start total]
  (let [stop (reduce move start moves)
        n (find-cycle-length moves start)]
    (->>
      (iterate (partial dance moves) start)
      (drop (rem total n))
      (first))))

;; part 2
(dance2 moves-a "abcde" (* 1000 1000 1000))
(dance2 moves-b "abcdefghijklmnop" (* 1000 1000 1000))
