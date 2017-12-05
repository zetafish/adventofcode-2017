(ns advent.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "input5.txt")
       (slurp)
       (str/split-lines)
       (mapv #(Integer/parseInt %))))

(defn step
  [f {:keys [jumps pos n]}]
  (when (and pos (>= pos 0) (< pos (count jumps)))
    {:jumps (update jumps pos f)
     :pos (+ pos (jumps pos))
     :n (inc n)}))

(defn solve
  [f jumps]
  (->> {:jumps jumps :pos 0 :n 0}
       (iterate (partial step f))
       (take-while identity)
       (last)
       (:n)))

#_ (solve f1 [0 3 0 1 -3])
#_ (solve f1 input)

#_ (solve f2 [0 3 0 1 -3])
#_ (solve f2 input)
