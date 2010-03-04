(ns hurdles
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
(defn sum [coll] (apply + coll))

(defprotocol Graph
  (node-set [self])
  (path [self a b]))

; constants
(def inf Float/POSITIVE_INFINITY)
(def *threads* 4)


; modified to take the max rather than sum
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
                        (max (path prev a k) (path prev k b))))))

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


(defn main []
  (let
    [ [n m t]    (parse-line)
      paths      (hash-c [_ (range m)] 
                   (let [[s e h] (parse-line)] [[s e] h]))
      tasks      (ffor [_ (range t)] (parse-line))

      init-graph (reify Graph
                   (node-set [] (range 1 (inc n)))
                   (path [a b] (paths [a b] inf)))
      heights    (floyd-warshall init-graph)
    ]
    (doseq [[a b] tasks]
      (let [height (path heights a b)]
        (if (< height inf)
          (println height)
          (println -1))))
    (shutdown-agents)))

(main)
