(ns allow
  (:use
    (clojure.contrib 
      core
      [seq           :only (partition-all group-by)]
      [combinatorics :only (cartesian-product subsets)]
      [math          :only (ceil expt abs)]
      [string        :only (split)]))
  (:import
    (java.awt.geom Line2D$Double Point2D$Double))
)

; utility definitions
(defmacro hash-c [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn parse-line []
  (ffor [s (split #"\s+" (read-line))] (read-string s)))

(defn parse-line-float []
  (ffor [s (split #"\s+" (read-line))] (Double/parseDouble s)))


(let [[n c] (parse-line)
      coins   (atom (hash-c [_ (range n)] (parse-line)))
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

