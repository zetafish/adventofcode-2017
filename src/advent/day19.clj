(ns advent.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; A Series of Tubes

(defn tr
  "Translate cell"
  [ch]
  (if (= \space ch)
    \.
    ch))

(def small
  (mapv (comp vec seq)
        ["    |         "
         "    |  +--+   "
         "    A  |  C   "
         "F---|----E|--+"
         "    |  |  |  D"
         "    +B-+  +--+"]))

(def input
  (mapv (comp vec seq)
        (str/split-lines
          (slurp
            (io/resource "input19.txt")))))

(defn cell
  [grid pos]
  (get-in grid (reverse pos) \space))

(defn start
  [grid]
  (let [x (->>
            (first grid)
            (take-while #(= \space %))
            (count))]
    [x 0]))

(defn backwards
  [[x y]]
  [(- 0 x) (- 0 y)])

(defn on-route?
  [grid point]
  (not= \space (cell grid point)))

(defn around
  [from dir]
  (->>
    [[0 1] [0 -1] [1 0] [-1 0]]
    (remove #(= (backwards dir) %))
    (remove #(= dir %))
    (cons dir)
    (map #(mapv + from %))))

(defn step
  [grid from dir]
  (let [to (->> (around from dir)
                (filter #(on-route? grid %))
                (first))]
    [to (mapv - to from)]))

(defn token?
  [ch]
  (re-matches #"[A-Z]" (str ch)))

(defn travel
  [grid]
  (letfn [(aux [from dir]
            (when from
              (lazy-seq
                (let [[from2 dir2] (step grid from dir)]
                  (cons [(tr (cell grid from)) from dir ]
                        (aux from2 dir2))))))]
    (aux (start grid) [0 1])))

(defn read-trail
  [trail]
  (->> trail
       (map first)
       (filter token?)
       (str/join)))

(defn part-1
  [grid]
  (->> grid
       travel
       read-trail))

(part-1 input)
