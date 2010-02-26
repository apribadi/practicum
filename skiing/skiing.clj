(ns skiing
  (:use
    (clojure.contrib 
      core
      [seq           :only (partition-all group-by)]
      [combinatorics :only (cartesian-product)]
      [math          :only (ceil expt)]
      [string        :only (split)])))

; utility functions
(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn int-list []
  (ffor [s (split #"\s+" (read-line))] (Integer/parseInt s)))

; constants
(def inf Float/POSITIVE_INFINITY)
(def *threads* 4)


(defn floyd-warshall [nodes graph]
  (let
    [
     n (count nodes)
     indices (range n)
     node-to-index (zipmap nodes indices)
     index-to-node (zipmap indices nodes)

     chunk-size (-> (/ n *threads*) ceil int)
     chunks     (partition-all chunk-size indices)
    
     graph-by-index 
       (fn [& args] (apply graph (map index-to-node args)))

     new-row (fn [prev k]
       (let
         [rows (vec (for [_ indices] (atom nil)))

          dochunk (fn [chunk]
            (doseq [a chunk]
              (reset! (rows a)
                (vec (for [b indices] 
                       (min (prev a b) (+ (prev a k) (prev k b))))))))
         ]
         (dorun (pmap dochunk chunks))
         (fn [a b] (@(rows a) b))))

     soln (reduce new-row graph-by-index indices)
     soln-by-node
       (fn [& args] (apply soln (map node-to-index args)))
    ]
    soln-by-node))

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
     points (cartesian-product (range r) (range c))

     initial-elev (elevations [0 0])
     velocity (fn [point]
       (* v (expt 2 (- initial-elev (elevations point)))))

     graph-map
       (map-comp
         [start points
          delta [[-1 0] [0 -1] [0 1] [1 0]]
          :let [[x y :as end] (map + start delta)]
          :when (and (< -1 x r) (< -1 y c))
         ]
         (let [segment [start end]
               time (/ 1 (velocity start))]
           [segment time]))

     graph (fn [a b] (graph-map [a b] inf))

     soln (floyd-warshall points graph)
    ]
    (println (soln [0 0] [(dec r) (dec c)]))
    (shutdown-agents)))

(skiing)

