(ns advent.day17)

(defn spinlock
  [step {:keys [buffer value pos]}]
  (let [next-pos (inc (rem (+ step pos) value))
        [x y] (split-at next-pos buffer)]
    {:buffer (doall (concat x [value] y))
     :value (inc value)
     :pos next-pos}))

(def seed {:buffer (list 0)
           :pos 0
           :value 1})

(defn after-2017
  [step]
  (let [state (->> (iterate (partial spinlock step) seed)
                   (drop 2017)
                   (first))]
    (nth (:buffer state) (inc (:pos state)))))

(after-2017 366)
