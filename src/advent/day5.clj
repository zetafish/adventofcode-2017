(ns advent.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "input5.txt")
       (slurp)
       (str/split-lines)
       (mapv #(Integer/parseInt %))))

(defn make-stepper
  [updater]
  (fn [{:keys [jumps pos]}]
    (if (or (nil? pos)
            (neg? pos)
            (>= pos (count jumps)))
      nil
      {:pos (+ pos (get jumps pos))
       :jumps (update jumps pos updater)})))

(defn make-jumper
  [stepper]
  (fn [jumps]
    (->> {:jumps jumps :pos 0}
         (iterate stepper)
         (take-while #(not (nil? %))))))

(defn compute
  [jumper jumps]
  (dec (count (jumper jumps))))

#_ (-> (make-stepper inc)
       (make-jumper)
       (compute input))

#_ (-> (make-stepper #(if (>= % 3) (dec %) (inc %)))
       (make-jumper)
       (compute input))
