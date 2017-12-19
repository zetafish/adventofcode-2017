(ns advent.day18
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

(def grammar
  "S = set|add|mul|mod|snd|rcv|jgz
  ws = #'\\s+'
  var = #'[a-z]'
  num = #'-?[0-9]+'
  add = <'add'> <ws> var <ws> (var | num)
  mul = <'mul'> <ws> var <ws> (var | num)
  set = <'set'> <ws> var <ws> (var | num)
  mod = <'mod'> <ws> var <ws> (var | num)
  snd = <'snd'> <ws> var
  rcv = <'rcv'> <ws> var
  jgz = <'jgz'> <ws> (var | num) <ws> (var | num)")

(def transform-options
  {:S vec
   :num read-string
   :var keyword})

(defn parse
  [s]
  (->> (insta/parse parser s)
       (insta/transform transform-options)))

(def example
  (mapv parse ["set a 1"
               "add a 2"
               "mul a a"
               "mod a 5"
               "snd a"
               "set a 0"
               "rcv a"
               "jgz a -1"
               "set a 1"
               "jgz a -2"]))

(def input
  (mapv parse (-> (io/resource "input18.txt")
                  (slurp)
                  (str/split-lines))))

(defn value-of
  [m x]
  (if (keyword? x)
    (m x 0)
    x))

(defn jump
  ([state] (update state :ip inc))
  ([state jmp] (update state :ip + jmp)))

(defn rcv
  [state x]
  (if-not (zero? x)
    (assoc state :rcv (:snd state))
    state))

(defn step
  [program state]
  (cond (< (:ip state) 0) nil
        (>= (:ip state) (count program)) nil
        :else (let [[op a b] (program (:ip state))
                    x (value-of state a)
                    y (value-of state b)]
                (condp = op
                  :set (-> state (assoc a y) (jump))
                  :add (-> state (assoc a (+ x y)) (jump))
                  :mul (-> state (assoc a (* x y)) (jump))
                  :mod (-> state (assoc a (rem x y)) (jump))
                  :snd (-> state (assoc :snd x) (jump))
                  :rcv (-> state (rcv x) (jump))
                  :jgz (-> state (jump (if (pos? x) y 1)))))))

(defn find-first-recover
  [program]
  (->> (iterate (partial step program) {:ip 0})
       (drop-while #(nil? (:rcv %)))
       (first)
       :rcv))

(find-first-recover input)
