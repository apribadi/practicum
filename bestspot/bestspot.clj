(ns bestspot
  (:use [clojure.contrib.seq-utils]
        [clojure.contrib.combinatorics]))

(defn int-list []
  "Read a line of input of space-separated integers.  Strict."
  (doall (for [s (.split (read-line) " ")] (Integer/parseInt s))))

(defn map-from-pairs [pairs]
  (apply hash-map (apply concat pairs)))

(defmacro map-comp [& args]
  `(map-from-pairs (for ~@args)))

(defn f-w [nodes paths fun]
  "fun takes [prev a b k]"
  (reduce (fn [acc k] (map-comp [a nodes b nodes]
                                [[a b] (fun acc a b k)]))
          paths
          nodes))

(def inf Float/POSITIVE_INFINITY)

(let [[p f c] (int-list)
      nodes     (range 1 (inc p))
      favorites (doall (for [_ (range f)] (first (int-list))))
      paths-in  (map-comp [_ (range c)] (let [[a b t] (int-list)] [[a b] t]))
      paths     (conj paths-one 
                      (map-comp [[[a b] t] paths-one] [[b a] t])
                      (map-comp [a nodes] [[a a] 0]))
      rule      (fn [prev a b k] 
                  (min (prev [a b] inf) (+ (prev [a k] inf) (prev [k b] inf))))
      times     (f-w nodes paths rule)
      get-avg   (fn [start] 
                  (apply + (for [b favorites] (times [start b]))))
      avgs      (map-comp [a nodes] [a (get-avg a)])
      min-dist  (apply min (vals avgs))
      best      (apply min (filter #(= min-dist (avgs %)) (keys avgs)))
     ]
  (println best))
