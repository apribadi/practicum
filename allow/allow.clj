(ns allow
  (:use [clojure.contrib.seq-utils]
        [clojure.contrib.combinatorics]))

(defn map-from-pairs [pairs]
  (apply hash-map (apply concat pairs)))

(defmacro map-comp [& args]
  `(map-from-pairs (for ~@args)))

(defn int-list []
  (for [s (.split (read-line) " ")] (Integer/parseInt s)))

(let [[n c] (int-list)
      coins   (atom (map-comp [_ (range n)] (int-list)))
      buckets (atom 0)
      needed  (atom 0)
     ]
  (while (not-empty @coins)
    (swap! needed (constantly c))
    (doseq [coin (-> @coins keys sort reverse)]
      (when (<= coin @needed)
        (let [left (@coins coin)
              to_take (min (quot @needed coin) left)]
          (if (= to_take left)
            (swap! coins #(dissoc % coin))
            (swap! coins #(assoc % coin (- left to_take))))
          (swap! needed #(- % (* to_take coin))))))
    (when (and (> @needed 0) (not-empty @coins))
      (let [coin (->> @coins keys (apply min))]
        (if (= 1 (@coins coin))
          (swap! coins #(dissoc % coin))
          (swap! coins #(assoc % coin (dec (% coin)))))
        (swap! needed #(- % coin))))
    (when (<= @needed 0)
      (swap! buckets inc)))
  (println @buckets))

