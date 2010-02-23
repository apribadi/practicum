(ns hurdles
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

(let [[n m t] (int-list)
      paths   (map-comp [_ (range m)] (let [[s e h] (int-list)] [[s e] h]))
      tasks   (doall (for [_ (range t)] (int-list)))
      rule    (fn [prev a b k] (min (prev [a b] inf) (max (prev [a k] inf) (prev [k b] inf))))
      heights (f-w (range 1 (inc n)) paths rule)
     ]
  (doseq [task tasks]
    (let [answer (heights task inf)]
      (if (< answer inf)
        (println answer)
        (println -1)))))

