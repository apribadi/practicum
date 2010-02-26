(ns ee
  (:use
    (clojure.contrib 
      core
      [seq           :only (partition-all group-by)]
      [combinatorics :only (cartesian-product)]
      [math          :only (ceil expt)]
      [string        :only (split)]))
  (:import
    (java.awt.geom Line2D$Double Point2D$Double))
  )


; utility functions
(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn int-list []
  (ffor [s (split #"\s+" (read-line))] (Integer/parseInt s)))

(defn float-list []
  (ffor [s (split #"\s+" (read-line))] (Double/parseDouble s)))

(defn Line [a b] (new Line2D$Double a b))
(defn Point [a b] (new Point2D$Double a b))

(def inf Float/POSITIVE_INFINITY)


; ee
(defn point-strength [point walls routers]
  (->>
    (for [router routers
          :when (not-any? #(.intersectsLine (Line point router) %) walls)
          ]
      (/ 1 (expt (.distance point router) 2)))
    (cons 0.0 ,,)
    (apply max ,,)))

(defn do-set [set-num]
  (let
    [
     [n r p] (int-list)
     corners (ffor [_ (range n)] (apply Point (float-list)))
     routers (ffor [_ (range r)] (apply Point (float-list)))
     points  (ffor [_ (range p)] (apply Point (float-list)))

     walls (for [[a b] (partition 2 1 (cons (last corners) corners))]
             (Line a b))
    ]
    (printf "Data Set %d:\n" set-num)
    (doseq [point points]
      (printf "%.2f\n" (point-strength point walls routers)))))

(defn ee []
  (let [k (first (int-list))]
    (dotimes [set-num k]
      (do-set set-num))))

(ee)

