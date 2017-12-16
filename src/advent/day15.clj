(ns advent.day15)

;; Duelling Generators

(def magic-constant 2147483647)

(defn generator
  [factor]
  (fn [previous]
    (rem (* previous factor)
         magic-constant)))

(defn last-16-bits
  [n]
  (bit-and 0xffff n))

(def a (generator 16807))

(def b (generator 48271))

(defn gen
  [g seed divisor]
  (->>
    (iterate g seed)
    (drop 1)
    (filter #(zero? (mod % divisor)))
    (map last-16-bits)))

(defn compute
  [c1 c2 n]
  (->> (map = c1 c2)
       (take n)
       (remove false?)
       (count)))

;; Part 1
(comment
  (compute (gen a 65 1)
           (gen b 8921 1)
           10)

  (compute (gen a 512 1)
           (gen b 191 1)
           (* 40 1000 1000))) ;; => 567

;; Part 2
(comment
  (compute (gen a 65 4)
          (gen b 8921 8)
          (* 5 1000 1000))

  (compute (gen a 512 4)
           (gen b 191 8)
           (* 5 1000 1000))) ;; => 323
