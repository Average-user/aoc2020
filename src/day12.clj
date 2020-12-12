(ns day12
  (:require [clojure.core.match :refer [match]]))
       
(defn move [[[x y] dir] inst]
  (letfn [(rotate [b deg dir]
            (nth (drop-while #(not= dir %) (cycle [\N \E \S \W])) (/ deg 90)))]
    (match inst
           [\N n] [[x (+ y n)] dir]
           [\S n] [[x (- y n)] dir]
           [\E n] [[(+ x n) y] dir]
           [\W n] [[(- x n) y] dir]
           [\F n] (move [[x y] dir] [dir n])
           [\R d] [[x y] (rotate true d dir)]
           [\L d] [[x y] (rotate false (- 360 d) dir)])))

(defn move2 [[pos [j k]] inst]
  (letfn [(rotate [s [j k] deg]
            (nth (iterate (fn [[x y]] [y (- x)]) [j k]) (/ deg 90)))]
    (match inst
           [\N n] [pos [j (+ k n)]]
           [\S n] [pos [j (- k n)]]
           [\E n] [pos [(+ j n) k]]
           [\W n] [pos [(- j n) k]]
           [\F n] [(mapv + pos (mapv #(* n %) [j k])) [j k]]
           [\R d] [pos (rotate \R [j k] d)]
           [\L d] [pos (rotate \L [j k] (- 360 d))])))

(defn main- []
  (let [insts (->> (clojure.java.io/reader "../inputs/day12.txt")
                   (line-seq)
                   (map (fn [s] [(first s) (read-string (apply str (rest s)))])))
        d     (fn [[x y]] (+ (Math/abs x) (Math/abs y)))]
    (time (do (println (d (first (reduce move [[0 0] \E] insts))))
              (println (d (first (reduce move2 [[0 0] [10 1]] insts))))))))
