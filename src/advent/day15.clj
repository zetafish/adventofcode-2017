(ns advent.day15)

;; Duelling Generators

;; The generators both work on the same principle. To create its next
;; value, a generator will take the previous value it produced,
;; multiply it by a factor (generator A uses 16807; generator B uses
;; 48271), and then keep the remainder of dividing that resulting
;; product by 2147483647. That final remainder is the value it
;; produces next.

(def magic-constant 2147483647)

(defn generator
  [factor]
  (fn [previous]
    (mod (* previous factor)
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
  [c1 c2 pairs]
  (->> (map vector c1 c2)
       (take pairs)
       (filter (fn [[x y]] (= x y)))
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
