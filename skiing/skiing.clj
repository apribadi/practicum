(ns skiing
  (:use 
    [clojure.contrib.seq-utils]
    [clojure.contrib.combinatorics]
    [clojure.contrib.math])
  (:require
    [clojure.contrib.str-utils2 :as string]))

; constants
(def inf Float/POSITIVE_INFINITY)
(def *threads* 4)

; utility functions
(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn int-list []
  "Read a line of input of space-separated integers."
  (ffor [s (string/split (read-line) #"\s+")] (Integer/parseInt s)))


(defn floyd-warshall [nodes paths]
  (let
    [new-row (fn [distance k]
       (let
         [rows       (vec (for [_ nodes] (atom nil)))
          chunk-size (-> (/ (count nodes) *threads*) ceil int)
          chunks     (partition-all chunk-size nodes)
          prev-dist  (fn [a b] (@(distance a) b))
          calc       (fn [a b]
                       (min (prev-dist a b)
                            (+ (prev-dist a k) (prev-dist k b))))
          dochunk    (fn [chunk]
                       (doseq [a chunk]
                         (reset! (rows a) 
                           (vec (for [b nodes] (calc a b))))))
         ]
         (dorun (pmap dochunk chunks))
         rows))
     paths-vec
       (->>
         (for [b nodes] (paths [a b] inf)) vec atom
         (for [a nodes] ,,) vec)
     answer-vec
       (reduce new-row paths-vec nodes)
    ]
    (map-comp
      [a nodes b nodes :let [dist (@(answer-vec a) b)] :when (not= dist inf)]
      [[a b] dist])))

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
     to-idx (fn [[a b]] (+ (* a c) b))
     paths
       (map-comp
         [start nodes
          end   (map #(map + start %) [[-1 0] [0 -1] [0 1] [1 0]])
          :when (every? identity (map #(< -1 %1 %2) end [r c]))
         ]
         [[(to-idx start) (to-idx end)] (/ 1 (velocity start))])
     time  (floyd-warshall (map to-idx nodes) paths)
    ]
    (println (time [(to-idx [0 0]) (to-idx [(dec r) (dec c)])]))
    (shutdown-agents)))

(skiing)

