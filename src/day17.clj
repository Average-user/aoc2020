(ns day17
  (:require [clojure.set :as s]))

(defn get-grid []
  (let [f (fn [y g [x v]] (if (= v \#) (conj g [x y 0 0]) g))]
    (->> (clojure.java.io/reader "../inputs/day17.txt")
         (line-seq)
         (map vector (range))
         (reduce (fn [g [y l]] (reduce (partial f y) g (map vector (range) l))) #{}))))

(defn neighbors [c part2]
  (let [r [-1 0 1]]
    (for [x r, y r, z r, w (if part2 r [0])
          :when (not= [x y z w] [0 0 0 0])]
      (mapv + c [x y z w]))))

(defn update-grid [g part2]
  (let [cn #(count (filter g (neighbors % part2)))]
    (set (filter #(let [c (cn %), inside (g %)]
                    (or (and inside (or (= c 3) (= c 2)))
                        (and (not inside) (= 3 c))))
                 (s/union g (set (mapcat #(neighbors % part2) g)))))))           

(defn execute [g part2]
  (count (nth (iterate #(update-grid % part2) (get-grid)) 6)))

(defn main- []
  (let [g (get-grid)]
    (do (println (execute g false))
        (println (execute g true)))))

(main-)
