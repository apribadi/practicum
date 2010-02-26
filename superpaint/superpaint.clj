(ns superpaint
  (:use
    (clojure.contrib 
      core
      [seq           :only (partition-all group-by)]
      [combinatorics :only (cartesian-product)]
      [math          :only (ceil expt abs)]
      [string        :only (split)]))
  (:import
    (java.awt.geom Line2D$Double Point2D$Double))
)


; utility functions
(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args] `(doall (for ~@args)))

(defn line-list []
  (ffor [s (split #"\s+" (read-line))] (read-string s)))

; superpaint
(defn can-hit? [a b]
  (let [[x y] a 
        [u v] b
        dx (- x u) 
        dy (- y v)]
    (or (= 0 dx) (= 0 dy) (= (abs dx) (abs dy)))))

(defn superpaint []
  (let
    [
     [n k] (line-list)
     cows  (ffor [_ (range k)] (line-list))
     good? (fn [square]
       (every? #(can-hit? square %) cows))
     squares (cartesian-product (range 1 (inc n)) (range 1 (inc n)))
    ]
    (println (count (filter good? squares)))))

(superpaint)

