(ns skiing
  (:use 
    [clojure.contrib.seq-utils]
    [clojure.contrib.combinatorics]
    [clojure.contrib.math])
  (:require
    [clojure.contrib.str-utils2 :as string]))

; constants
(def inf Float/POSITIVE_INFINITY)

; utility functions
(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn int-list []
  "Read a line of input of space-separated integers."
  (ffor [s (string/split (read-line) #"\s+")] (Integer/parseInt s)))


(defn floyd-warshall [nodes paths]
  (letfn 
    [(new-row [distance k]
       (->>
         (map-comp
           [a nodes :let [a-dist (distance [a k])] :when a-dist
            b nodes :let [b-dist (distance [k b])] :when b-dist]
           [[a b] (+ a-dist b-dist)])
         (merge-with min distance ,,)))
    ]
    (reduce new-row paths nodes)))

(defn skiing-input []
  (let
    [global (atom {})
     add   #(swap! global assoc ,, %1 %2)
     add-m #(swap! global merge ,, %)
    ]
    (add-m 
      (zipmap [:v :r :c] (int-list)))
    (add :elevations
      (map-comp
        [row (range (@global :r)) :let [nums (vec (int-list))]
         col (range (@global :c)) :let [elv  (nums col)]
        ]
        [[row col] elv]))
    @global))

(defn skiing []
  (let 
    [
     {:keys [v r c elevations]} (skiing-input)
     initial-elev (elevations [0 0])
     nodes (cartesian-product (range r) (range c))
     velocity
       (map-comp
         [[node elev] elevations]
         [node (* v (expt 2 (- initial-elev elev)))])
     paths
       (map-comp
         [start nodes
          end   (map #(map + start %) [[-1 0] [0 -1] [0 1] [1 0]])
          :when (every? identity (map #(< -1 %1 %2) end [r c]))
         ]
         [[start end] (/ 1 (velocity start))])
     time  (floyd-warshall nodes paths)
    ]
    (println (time [[0 0] [(dec r) (dec c)]]))))

(skiing)

