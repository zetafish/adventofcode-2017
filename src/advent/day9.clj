(ns advent.day9
  (:require [clojure.java.io :as io]))

;; Stream processing
(def input (-> (io/resource "input9.txt")
               (slurp)))

(defn scan
  [{:keys [score group garbage cancel count] :as state} ch]
  (if garbage
    (cond
      cancel (assoc state :cancel false)
      (= \! ch) (assoc state :cancel true)
      (= \> ch) (assoc state :garbage false)
      :else (update state :count inc))
    (condp = ch
      \< (assoc state :garbage true)
      \{ (-> state
             (update :group inc))
      \} (-> state
             (update :score + group)
             (update :group dec))
      state)))

(def seed {:score 0 :group 0
           :garbage false :cancel false :count 0})

(defn f
  [s]
  (reduce scan seed s))

(f "{}")
(f "{{}}")
(f "{{{}}}")
(f "{{},{}}")
(f "{{{},{},{{}}}}")
(f "{<a>,<a>,<a>,<a>}")
(f "{<>}")
(f "{{<ab>},{<ab>},{<ab>},{<ab>}}")
(f "{{<!!>},{<!!>},{<!!>},{<!!>}}")
(f "{<!!>}")
(f "{{<a!>},{<a!>},{<a!>},{<ab>}}")
(f "{{<!>},{<!>},{<!>},{<a>}}")
(f "<{!>}>")
(f input)
