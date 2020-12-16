(ns day16
  (:require [clojure.string :as cs]
            [clojure.set :as s]))

(defn parse-input []
  (let [lines (line-seq (clojure.java.io/reader "../inputs/day16.txt"))
        f (fn [[_ n a b x y]] (vec (cons n (mapv read-string [a b x y]))))]
    {:fields (map f (keep #(re-matches #"([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)" %)
                          lines))
     :my-ticket (as-> (drop-while #(not= "your ticket:" %) lines) $
                     (nth $ 1)
                     (cs/split $ #",")
                     (mapv read-string $))
     :nb-tickets
     (as-> (drop-while #(not= "nearby tickets:" %) lines) $
           (rest $)
           (map #(mapv read-string (cs/split % #",")) $))}))

(defn valid? [fields n]
  (some (fn [[_ a b x y]] (or (<= a n b) (<= x n y))) fields))

(defn find-invalid-sum [{:keys [fields nb-tickets]}]
   (reduce + (remove #(valid? fields %) (apply concat nb-tickets))))

(defn works [nb-ts [_ a b x y] i]
  (every? (comp #(or (<= a % b) (<= x % y)) #(nth % i)) nb-ts))

(defn correct-ordering [nb-ts fields]
  (let [k (count (first nb-ts))
        m' (map (fn [[n a b x y]]
                  [n (set (filter #(works nb-ts [n a b x y] %) (range k)))])
                fields)]
    (loop [m m', i 0]
      (let [singletons (map second (filter (fn [[k v]] (= 1 (count v))) m))
            rm (fn [m v] (map (fn [[k v']]
                                (if (= 1 (count v'))
                                    [k v']
                                    [k (s/difference v' v)]))
                              m))]
        (if (every? (fn [[k v]] (= 1 (count v))) m)
          (map (fn [[k s]] [k (first s)]) m)
          (recur (reduce rm m singletons) (inc i)))))))
         
(defn main- []
  (let [{:keys [fields my-ticket nb-tickets] :as input} (parse-input)
        nb-ts (filter #(every? (partial valid? fields) %) nb-tickets)
        ordering (correct-ordering nb-ts fields)]
    (do (println (find-invalid-sum input))
        (->> ordering
             (filter (fn [[k v]] (cs/starts-with? k "departure")))
             (map second)
             (map #(nth my-ticket %))
             (reduce *)
             (println)))))
(main-)
