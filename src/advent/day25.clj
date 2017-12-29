(ns advent.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]))

;; --- Day 25: The Halting Problem ---

(def sample
  {:A {0 [1 :> :B] 1 [0 :< :B]}
   :B {0 [1 :< :A] 1 [1 :> :A]}})


(def blueprint
  {:A {0 [1 :> :B]
       1 [0 :< :F]}
   :B {0 [0 :> :C]
       1 [0 :> :D]}
   :C {0 [1 :< :D]
       1 [1 :> :E]}
   :D {0 [0 :< :E]
       1 [0 :< :D]}
   :E {0 [0 :> :A]
       1 [1 :> :C]}
   :F {0 [1 :< :A]
       1 [1 :> :A]}})


(defn step
  [blueprint {:keys [state pos tape] :as world}]
  (let [x (get tape pos 0)
        [w m s] (get-in blueprint [state x])]
    (-> world
        (update :pos (condp = m :> inc :< dec
                            #dbg (println "bad")))
        (update :tape assoc pos w )
        (assoc :state s))))

(defn cell
  [{:keys [pos tape]} n]
  (let [v (get tape n 0)]
    (if (= pos n)
      (str "[" v "]")
      (str " " v " "))))

(defn show
  [margin {:keys [state pos tape] :as world}]
  (str "..."
       (->>
         (range (- margin) margin)
         (map #(cell world %))
         (str/join))
       "..."))


(def state
 (->> (iterate (partial step blueprint) {:state :A :pos 0})
      (drop 12794428)
      (first)))

(->> state
     :tape
     vals
     (filter pos?)
     (count))
