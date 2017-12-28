(ns advent.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]))

;; --- Day 23: Coprocessor Conflagration ---
(def grammar
  "S = set | sub | mul | jnz
  set = <'set '> var <' '> (var | num)
  sub = <'sub '> var <' '> (var | num)
  mul = <'mul '> var <' '> (var | num)
  jnz = <'jnz '> (var | num) <' '> (var | num)
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

(def code (mapv parse (-> (io/resource "input23.txt")
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
  (-> state
      (assoc-in [:var a] (value state b))
      (update :ip inc)))

(defn sub-op
  [state a b]
  (-> state
      (assoc-in [:var a] (- (value state a) (value state b)))
      (update :ip inc)))

(defn mul-op
  [state a b]
  (-> state
      (assoc-in [:var a] (* (value state a) (value state b)))
      (update :mul safe-inc)
      (update :ip inc)))

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
              :jnz (jnz-op state a b)))))

(defn simulate
  [code]
  (letfn [(f [state]
            (when state
              (lazy-seq (cons state
                              (f (step code state))))))]
    (f {:ip 0})))


(->> (simulate code)
     (last)
     (println))
