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

(defprotocol Graph
  (node-set [self])
  (path [self a b]))


; constants
(def inf Float/POSITIVE_INFINITY)
(def *threads* 4)


(defn floyd-warshall [graph]
  (let
    [
     nodes         (node-set graph)
     indices       (range (count nodes))
     node-to-index (zipmap nodes indices)
     index-to-node (zipmap indices nodes)
     chunk-size    (-> (/ (count indices) *threads*) ceil int)
     chunks        (partition-all chunk-size indices)
    
     graph-by-index (reify Graph
       (path [a b] (path graph (index-to-node a) (index-to-node b))))

     step (fn [prev k]
       (let
         [rows (vec (for [_ indices] (atom nil)))

          make-row (fn [a]
            (vec (for [b indices]
                   (min (path prev a b)
                        (+ (path prev a k) (path prev k b))))))

          dochunk (fn [chunk]
            (doseq [a chunk]
              (reset! (rows a) (make-row a))))
         ]
         (dorun (pmap dochunk chunks))
         (reify Graph (path [a b] (@(rows a) b)))))

     soln (reduce step graph-by-index indices)

     soln-by-node (reify Graph
       (node-set [] nodes)
       (path [a b] (path soln (node-to-index a) (node-to-index b))))
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

     graph (reify Graph
       (node-set [] points)
       (path [a b] (graph-map [a b] inf)))

     soln (floyd-warshall graph)
    ]
    (println (float (path soln [0 0] [(dec r) (dec c)])))
    (shutdown-agents)))

(skiing)

