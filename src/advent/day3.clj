(ns advent.day3)

;; Day 3: Spiral Memory

(def inc2 (partial + 2))
(defn dec2 [n] (- n 2))

(defn square
  [n]
  (* n n))

(defn ring
  [n]
  (->> (iterate inc2 1)
       (drop-while #(< (* % %) n))
       (first)))

(defn position
  [n]
  (dec (- n (square (dec2 (ring n))))))

(defn distance-to-axis
  [n]
  (let [r (ring n)
        p (position n)
        k (inc (rem p (dec r)))
        d (Math/abs (- (/ (dec r) 2) k))]
    d))

(defn distance-to-portal
  [n]
  (let [r (/ (dec (ring n)) 2)
        d (distance-to-axis n)]
    (+ r d)))

(distance-to-portal 347991)
