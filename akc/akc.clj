(ns akc
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

(defn sum [coll] (apply + coll))


; main stuff
(defn Point [a b] (new Point2D$Double a b))
(defn Line [a b] (new Line2D$Double a b))

(defn cost [stores wh-set]
  (+ (sum (map :cost wh-set))
     (sum (for [store stores]
            (apply min 
              (for [wh (map :location wh-set)]
                (.distance wh store)))))))

(defn do-set [set-index]
  (let
    [
     [n m] (parse-line)

     stores     (ffor [_ (range n) :let [[x y] (parse-line)]]
                  (Point x y))
     warehouses (ffor [_ (range m) :let [[x y p] (parse-line)]]
                  {:cost p :location (Point x y)})
     wh-sets    (filter (complement empty?) (subsets warehouses))
     costs      (for [wh-set wh-sets] (cost stores wh-set))
     best-cost  (apply min costs)
    ]
    (printf "Data Set %d:\n" set-index)
    (printf "%.2f\n" best-cost)))

(defn main []
  (let [[k] (parse-line)]
    (dotimes [set-index k]
      (do-set set-index))))

(main)

