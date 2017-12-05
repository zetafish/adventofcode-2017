(ns advent.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "input5.txt")
       (slurp)
       (str/split-lines)
       (mapv #(Integer/parseInt %))))

(defn step
  [{:keys [jumps pos]}]
  (if (or (nil? pos)
          (neg? pos)
          (>= pos (count jumps)))
    nil
    {:pos (+ pos (get jumps pos))
     :jumps (update jumps pos inc)}))

(defn step-all
  [jumps]
  (->> {:jumps jumps :pos 0}
       (iterate step)
       (take-while #(not (nil? %)))))

(defn compute-1
  [jumps]
  (dec (count (step-all jumps))))

#_ (compute-1 [0 3 0 1 -3])
#_ (compute-1 input)
