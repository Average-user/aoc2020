(ns day23)

(defn f [n i] (if (zero? i) n i))

(defn build [xs]
  (conj (into {} (map vector xs (rest xs))) [(last xs) (first xs)]))

(defn game-step [n xs c]
  (let [g          (fn [m i] (or (m i) (inc i)))
        [x1 x2 x3] (list (g xs c) (g xs (g xs c)) (g xs (g xs (g xs c))))
        d          (first (filter (complement #{x1 x2 x3})
                                  (next (iterate #(f n (dec %)) c))))]
    (assoc xs d x1 c (g xs x3) x3 (g xs d))))

(defn game [n xs c]
  (iterate (fn [[xs c]] (let [ys (game-step n xs c)] [ys (ys c)]))
           [xs c]))
                            
(defn main- []
  (let [in (map (comp read-string str) (filter (set "0123456789")
                                               (slurp "../inputs/day23.txt")))
        xs  (build in)
        r   (first (nth (game (count in) xs (first in)) 100))
        xs' (assoc xs (last in) (inc (count in)) 1000000 (first in))
        r'  (first (nth (game 1000000 xs' (first in)) 10000000))]
    [(read-string (apply str (take (dec (count in)) (next (iterate r 1)))))
     (* (r' 1) (r' (r' 1)))]))

(time (print (main-)))
