(ns boundary
  (:use
    (clojure.contrib 
      core
      [seq           :only (partition-all group-by)]
      [combinatorics :only (cartesian-product subsets)]
      [math          :only (ceil expt abs)]
      [string        :only (split)]))
  (:import
    (java.awt.geom Line2D$Double Point2D$Double)
    (java.awt Polygon))
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


(defn read-polygon []
  (let
    [
      [p] (parse-line)
      points (ffor [_ (range p)] (apply Point (parse-line)))
      lines  (for [[a b] (partition 2 1 (cons (last points) points))]
               (Line a b))
    ]
    lines))

(defn clear? [a b lines]
  (not-any? #(.intersectsLine (Line a b) %) lines))

(defn main []
  (let 
    [
     [n r] (parse-line)
     [x y] (parse-line)
     bob   (Point x y)
     polygons (ffor [_ (range r)] (read-polygon))
     lines (apply concat polygons)
     border (concat
              (for [x [0 n] y (range (inc n))] (Point x y))
              (for [x (range 1 n) y [0 n]] (Point x y)))
    ]
    (println (count (filter #(clear? bob % lines) border)))))

(main)

