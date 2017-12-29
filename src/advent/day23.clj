(ns advent.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]))

;; --- Day 23: Coprocessor Conflagration ---
(def grammar
  "S = (set | sub | mul | jnz | mod) <#'.*'>
  set = <'set '> var <' '> (var | num)
  sub = <'sub '> var <' '> (var | num)
  mul = <'mul '> var <' '> (var | num)
  jnz = <'jnz '> (var | num) <' '> (var | num)
  mod = <'mod '> var <' '> (var | num)
  var = #'[a-z]'
  num = #'-?[0-9]+'")

(def parser (insta/parser grammar))

(def options
  {:S vec
   :num read-string
   :var keyword})

(defn parse
  [s]
  (insta/transform options (insta/parse parser s)))

(defn read-code
  []
  (mapv parse (-> (io/resource "input23.txt")
                  (slurp)
                  (str/split-lines))))

(defn safe-inc
  [v]
  (inc (or v 0)))

(defn value
  [state r]
  (cond
    (keyword? r) (get-in state [:var r] 0)
    (integer? r) r))

(defn set-op
  [state a b]
  (let [v (value state b)]
    (-> state
        (assoc-in [:var a] v)
        (update :ip inc))))

(defn sub-op
  [state a b]
  (let [v (- (value state a) (value state b))]
    (-> state
        (assoc-in [:var a] v)
        (update :ip inc))))

(defn mul-op
  [state a b]
  (let [v (* (value state a) (value state b))]
    (-> state
        (assoc-in [:var a] v)
        (update :mul safe-inc)
        (update :ip inc))))

(defn mod-op
  [state a b]
  (let [v (rem (value state a) (value state b))]
    (-> state
        (assoc-in [:var a] v)
        (update :ip inc))))

(defn sqrt-op
  [state a b]
  (let [v (int (Math/sqrt (value state b)))]
    (-> state
        (assoc-in [:var a] v)
        (update :ip inc))))

(defn jnz-op
  [state a b]
  (let [x (value state a)]
    (if-not (zero? x)
      (update state :ip + (value state b))
      (update state :ip inc))))

(defn step
  [code {:keys [ip var] :as state}]
  (cond
    (< ip 0) nil
    (>= ip (count code)) nil
    :else (let [[op a b] (get code ip)]
            (condp = op
              :set (set-op state a b)
              :sub (sub-op state a b)
              :mul (mul-op state a b)
              :mod (mod-op state a b)
              :sqrt (sqrt-op state a b)
              :jnz (jnz-op state a b)))))

(defn simulate
  [code initial]
  (letfn [(f [state]
            (when state
              (lazy-seq (cons state
                              (f (step code state))))))]
    (f initial)))

(->> (simulate (read-code) {:ip 0})
     (last)
     (println))

(defn dividers
  [primes n]
  (take-while #(<= (* % %) n) primes))

(defn prime?
  [primes n]
  (every? #(pos? (rem n %))
          (dividers primes n)))

(defn range-peek
  [coll]
  (iterate inc (-> coll peek inc)))

(defn sieve
  ([] (cons 2 (lazy-seq (sieve [2]))))
  ([primes]
   (let [p (->> primes
                range-peek
                (filter (partial prime? primes))
                first)]
     (cons p (lazy-seq (sieve (conj primes p)))))))

(def b 106500)
(def c 123500)
(def primes (take-while #(< % (Math/sqrt c)) (sieve)))


(defn fast-algorithm
  [b c]
  (->> (range b (inc c) 17)
       (filter #(not (prime? primes %)))
       (count)))

(fast-algorithm b c)
