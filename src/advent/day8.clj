(ns advent.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def pat #"(\w+) (dec|inc) (-?\d+) if (\w+) (<|>|>=|<=|==|!=) (-?\d+)")

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-op
  [s]
  (condp = s
    "dec" -
    "inc" +
    "<" <
    ">" >
    "<=" <=
    ">=" >=
    "==" =
    "!=" not=))

(defn parse-line
  [s]
  (let [[t op x s cmp y] (drop 1 (re-find pat s))]
    [(keyword t) (parse-op op) (parse-int x)
     (keyword s) (parse-op cmp) (parse-int y)]))

(defn run-cmd
  [{:keys [vars high] :as data} [t op x s cmp y]]
  (if (cmp (get vars s 0) y)
    (let [v (get vars t 0)]
      (-> data
          (assoc-in [:vars t] (op v x))
          (update :high max v)))
    data))


(def input (->> (io/resource "input8.txt")
                (slurp)
                (str/split-lines)
                (map parse-line)))


(def small (->> ["b inc 5 if a > 1"
                 "a inc 1 if b < 5"
                 "c dec -10 if a >= 1"
                 "c inc -20 if c == 10"]
                (map parse-line)))

(defn compute
  [cmds]
  (let [state (reduce run-cmd {:vars {} :high 0} cmds)]
    (-> state
        (assoc :max (apply max (-> state :vars vals)))
        (dissoc :vars))))

(compute small)
(compute input)
