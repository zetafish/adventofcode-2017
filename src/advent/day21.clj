(ns advent.day21
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn rot-2x2
  [[a b c d]]
  [c a d b])

(defn rot-3x3
  [[a b c d e f g h i]]
  [g d a h e b i f c])

(defn rot
  [g]
  (condp = (count g)
    4 (rot-2x2 g)
    9 (rot-3x3 g)))

(defn flip
  [g]
  (mapcat
    reverse
    (condp = (count g)
      4 (partition 2 g)
      9 (partition 3 g))))

(defn patterns-0
  [g]
  (let [g (seq g)]
    (distinct
      (concat (take 4 (iterate rot g))
              (take 4 (iterate rot (flip g)))))))

(def patterns (memoize patterns-0))

(defn parse-term
  [s]
  (vec (remove #(= % \/) s)))

(defn parse-rule
  [s]
  (let [[l r] (str/split s #" => ")]
    {:input (parse-term l)
     :output (parse-term r)}))

(defn sqrt
  [n]
  (int (Math/sqrt n)))

(defn zipper
  [n col]
  (->>
    (range n)
    (map #(take-nth n (drop % col)))
    (apply concat)))

(defn split-image
  [image]
  (let [n (sqrt (count image))
        d (if (even? n) 2 3)]
    (->> (partition d image)
         (zipper (/ n d))
         (partition d)
         (map #(apply concat %))
         (zipper (/ n d)))))

(defn unsplit-image
  [parts]
  (let [n (int (sqrt (count parts)))
        k (int (sqrt (count (first parts))))]
    (->> parts
         (map #(partition k %))
         (partition n)
         (mapcat #(apply interleave %))
         (apply concat))))

(defn enhance-0
  [rule-book image]
  (->> (patterns image)
       (keep rule-book)
       (first)))

(def enhance (memoize enhance-0))

(defn generate
  [rule-book image]
  (->> (split-image image)
       (map #(enhance rule-book %))
       (unsplit-image)))

(defn render
  ([image] (render "\n" image))
  ([sep image]
   (let [d (sqrt (count image))]
     (->> (partition d image)
          (map str/join)
          (str/join sep)))))

(defn count-pixels-on
  [image]
  (count
    (filter #(= \# %) image)))


(def seed (parse-term ".#./..#/###"))

(def rule-book (->> (-> (io/resource "input21.txt")
                        (slurp)
                        (str/split-lines))
                    (map parse-rule)
                    (map (juxt :input :output))
                    (into {})))

;; Slow
(time (->> (iterate (partial generate rule-book) seed)
           (drop 18)
           (first)
           (count-pixels-on)))


(defn show
  [x]
  (println (render x))
  (println "-------------"))
