(ns advent.day10
  (:require [clojure.string :as str]))

(def input [165 1 255 31 87 52 24 113 0 91 148 254 158 2 73 153])

(def input-s "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153")

(def suffix [17 31 73 47 23])

(defn rotate
  [n col]
  (concat (drop n col) (take n col)))

(defn twist
  [n col]
  (concat (reverse (take n col))
          (drop n col)))

(defn knotter
  []
  (let [state (atom {:pos 0 :skip 0})]
    (fn [col length]
      (let [k (count col)
            {:keys [pos skip]} @state]
        (reset! state {:pos (mod (+ pos length skip) k)
                       :skip (inc skip)})
        (->> col
             (rotate pos)
             (twist length)
             (rotate (- k pos)))))))

(defn part-1
  [col lengths]
  (->>
    (reduce (knotter) col lengths)
    (take 2)
    (apply *)))

(part-1 (range 5) [3 4 1 5])
(part-1 (range 256) input)

(defn part-2
  [col lengths]
  (let [lengths (->> (concat lengths suffix)
                     (repeat 64)
                     (apply concat))]
    (->>
      (reduce (knotter) col lengths)
      (partition 16)
      (map #(apply bit-xor %))
      (map #(format "%02x" %))
      (str/join))))

(part-2 (range 256) [])
(part-2 (range 256) (map int "AoC 2017"))
(part-2 (range 256) (map int "1,2,3"))
(part-2 (range 256) (map int "1,2,4"))
(part-2 (range 256) (map int input-s))
