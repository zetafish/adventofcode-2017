(ns advent.day15)

;; Duelling Generators

;; The generators both work on the same principle. To create its next
;; value, a generator will take the previous value it produced,
;; multiply it by a factor (generator A uses 16807; generator B uses
;; 48271), and then keep the remainder of dividing that resulting
;; product by 2147483647. That final remainder is the value it
;; produces next.

(def magic-constant 2147483647)

(def total-pairs (* 40 1000 1000))

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

;; My input:
;;
;; Generator A starts with 512
;; Generator B starts with 191

(defn suffixes
  [f seed]
  (->> (iterate f seed)
       (map last-16-bits)))


(defn part-1
  [init-a init-b pairs]
  (->>
    (map vector (suffixes a init-a) (suffixes b init-b))
    (drop 1)
    (take pairs)
    (filter (fn [[x y]] (= x y)))
    (count)))

(comment
  (part-1 65 8921 10)
  (part-1 512 191 (* 40 1000 1000)))    ; => 567
