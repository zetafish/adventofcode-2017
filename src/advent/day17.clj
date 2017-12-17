(ns advent.day17)

(defn spinlock
  [step {:keys [buffer pos]}]
  (let [n (count buffer)
        next-pos (inc (rem (+ step pos) n))
        [x y] (split-at next-pos buffer)]
    {:buffer (doall (concat x [n] y))
     :pos next-pos}))

(defn part1
  [step]
  (let [{:keys [buffer pos] :as state}
        (->> (iterate (partial spinlock step) {:buffer [0] :pos 0})
             (drop 2017)
             (first))]
    (nth buffer (inc pos))))

(part1 366)
