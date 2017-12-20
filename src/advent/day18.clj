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

(def parser (insta/parser grammar))

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
  [state a]
  (update
    (if-not (zero? (value-of state a))
      (assoc state :rcv (:snd state))
      state)
    :ip inc))

(defn snd
  [state a]
  (-> state
      (assoc :snd (value-of state a))
      (update :ip inc)))

(defn step
  [state snd rcv cmd]
  (when cmd
    (let [[op a b] cmd
          x (value-of state a)
          y (value-of state b)]
      (condp = op
        :set (-> state (assoc a y) jump)
        :add (-> state (assoc a (+ x y)) jump)
        :mul (-> state (assoc a (* x y)) jump)
        :mod (-> state (assoc a (rem x y)) jump)
        :jgz (-> state (jump (if (pos? x) y 1)))
        :snd (-> state (snd a))
        :rcv (-> state (rcv a))))))

(defn stream
  [{:keys [program snd rcv initial]}]
  (letfn [(f [state]
            (when state
              (let [cmd (program (:ip state))]
                (lazy-seq
                  (cons state
                        (f (step state snd rcv cmd)))))))]
    (f initial)))

(defn solo
  [program]
  (->> (stream {:program program
                :snd snd :rcv rcv
                :initial {:ip 0}})))

(defn rcv!
  [from state a]
  (let [[old new] (swap-vals! from butlast)]
    (if-not old
      (assoc state :parked true)
      (-> state
          (assoc a (last old))
          (dissoc :parked)
          (update :ip inc)))))

(defn snd!
  [to state a]
  (let [x (value-of state a)]
    (swap! to conj x)
    (-> state
        (update :sent conj x)
        (update :ip inc))))

(defn blocked?
  [stream queue]
  (or (nil? stream)
      (and (empty? @queue)
           (:parked (first stream) false))))

(defn duet
  [program]
  (let [q0 (atom nil)
        q1 (atom nil)
        s0 (stream {:program program
                    :rcv (partial rcv! q0)
                    :snd (partial snd! q1)
                    :initial {:program 0 :ip 0 :p 0}})
        s1 (stream {:program program
                    :rcv (partial rcv! q1)
                    :snd (partial snd! q0)
                    :initial {:program 1 :ip 0 :p 1}})]
    (letfn [(aux [s0 s1]
              (lazy-seq
                (cons (map first [s0 s1])
                      (cond
                        (not (blocked? s0 q0)) (aux (rest s0) s1)
                        (not (blocked? s1 q1)) (aux s0 (rest s1))))))]
      (aux s0 s1))))

(defn part-1
  [program]
  (->> (solo program)
       (drop-while (comp nil? :rcv))
       (first)))

(part-1 input)

(defn part-2
  [program]
  (->> (duet input)
       (last)
       (map #(update % :sent count))))

(part-2 input)
