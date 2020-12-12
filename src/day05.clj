(ns day05)

(defn get-id [xs]
  (Integer/parseInt (apply str (map {\F 0 \B 1 \L 0 \R 1} xs)) 2))

(defn find-missing [ids]
  (letfn [(is-missing? [id] (and (not (ids id)) (ids (inc id)) (ids (dec id))))]
    (first (filter is-missing? (range)))))

(defn main- []
  (let [ids (->> (clojure.java.io/reader "inputs/day05.txt")
                 (line-seq)
                 (map get-id)
                 (set))]
    (time (do (println (reduce max ids))
              (println (find-missing ids))))))

(main-)
