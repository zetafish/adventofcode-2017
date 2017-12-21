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

(defn manhattan
  [p]
  (reduce + (map #(Math/abs %) p)))

(defn parse
  [s]
  (let [[p v a]
        (->> (insta/parse (insta/parser grammar) s)
             (insta/transform transform-options))]
    {:p p :v v :a a}))

(defn add-distance
  [m]
  (assoc m :d (manhattan (:p m))))

(defn parse-items
  [lines]
  (->> (map parse lines)
       (map add-distance)
       (map-indexed (fn [i m] (assoc m :num i)))))

(def sample
  (parse-items ["p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
                "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"]))

(def input
  (parse-items (-> (io/resource "input20.txt")
                   (slurp)
                   (str/split-lines))))

(defn tick
  [{:keys [p v a] :as m}]
  (let [v (mapv + v a)
        p (mapv + v p)
        d (manhattan p)]
    (assoc m :p p :v v :d d)))

(defn remove-collissions
  [col]
  (->> (sort-by :p col)
       (partition-by :p)
       (remove #(> (count %) 1))
       (apply concat)))

(defn simulate
  [particles resolver]
  (letfn [(f [col]
            (let [col (resolver col)
                  col (sort-by :d col)]
              (lazy-seq
                (cons col
                      (f (map tick col))))))]
    (f particles)))

;; part-1
(comment

  (def sim1 (simulate input identity))

  (->> sim1
       (map (comp :num first))
       (drop 400)
       (take 10)))


;; part 2
(comment

  (def sim2 (simulate input remove-collissions))

  (->> sim2
       (drop 500)
       (first)
       (count)))
