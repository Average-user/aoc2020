(ns day23
  (:require [clojure.string :refer [trim]]))

(def input (map (comp read-string str) (trim (slurp "../inputs/day23.txt"))))
                        
(defn f [n i] (if (zero? i) n i))
(defn g  [v i] (or (v i) (inc i)))

(defn build [n xs]
  (-> (reduce (fn [v [x y]] (assoc v x y))
              (vec (repeat (inc n) nil))
              (map vector xs (rest xs)))
      (assoc (last xs) (first xs))))

(defn game-step [n xs c]
  (let [[x1 x2 x3] (list (g xs c) (g xs (g xs c)) (g xs (g xs (g xs c))))
        d          (first (filter (complement #{x1 x2 x3})
                                  (next (iterate #(f n (dec %)) c))))]
    (assoc xs d x1 c (g xs x3) x3 (g xs d))))

(defn game [n xs c]
  (iterate (fn [[xs c]] (let [ys (game-step n xs c)] [ys (ys c)])) [xs c]))
                            
(defn main- []
  (let [xs  (build (count input) input)
        r   (first (nth (game (count input) xs (first input)) 100))
        xs' (assoc (build 1000000 input)
                   (last input) (inc (count input)) 1000000 (first input))
        r'  (first (nth (game 1000000 xs' (first input)) 10000000))]
    [(read-string (apply str (take (dec (count input)) (next (iterate r 1)))))
     (* (r' 1) (r' (r' 1)))]))

(time (print (main-)))
