(ns advent.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def pat #"(\w+) \((\d+)\)( -> (\w+)(, \w+)*)?")

(def s1 "pgvds (134) -> manpd, vkikri")
(def s2 "asdad (12)")

(re-matches pat s1)

(defn parse-line
  [s]
  (let [[name weight _ & above] (filter (complement empty?) (str/split s #" |,"))
        weight (Integer/parseInt (re-find #"\d+" weight))]
    {:name name
     :weight weight
     :above above}))

(defn find-bottom
  [programs]
  (let [names (map :name programs)
        non-bottom (mapcat :above programs)]
    (set/difference (set names) (set non-bottom))))


(def input (->> (io/resource "input7.txt")
                (slurp)
                (str/split-lines)
                (map parse-line)))

(find-bottom input)
