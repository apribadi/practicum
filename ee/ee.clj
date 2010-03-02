(ns ee
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


; solution
(defn Line [a b] (new Line2D$Double a b))
(defn Point [a b] (new Point2D$Double a b))

(defn point-strength [point walls routers]
  (->>
    (for [router routers
          :when (not-any? #(.intersectsLine (Line point router) %) walls)
         ]
      (/ 1 (expt (.distance point router) 2)))
    (cons 0.0 ,,)
    (apply max ,,)))

(defn do-set [set-index]
  (let
    [
     [n r p] (parse-line)
     corners (ffor [_ (range n)] (apply Point (parse-line)))
     routers (ffor [_ (range r)] (apply Point (parse-line)))
     points  (ffor [_ (range p)] (apply Point (parse-line)))

     walls (map #(apply Line %) (partition 2 1 (cons (last corners) corners)))
     walls (map Line corners (cons (last corners) corners))
     walls (for [[a b] (partition 2 1 (cons (last corners) corners))]
             (Line a b))
    ]
    (printf "Data Set %d:\n" set-index)
    (doseq [point points]
      (printf "%.2f\n" (point-strength point walls routers)))))

(defn main []
  (let [[k] (parse-line)]
    (dotimes [set-index k]
      (do-set set-index))))

(main)

