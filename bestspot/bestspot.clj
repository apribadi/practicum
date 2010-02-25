(ns bestspot
  (:use 
    [clojure.contrib.seq-utils]
    [clojure.contrib.combinatorics]
    [clojure.contrib.generic.math-functions])
  (:require
    [clojure.contrib.str-utils2 :as string]))

; constants
(def *threads* 4)
(def inf Float/POSITIVE_INFINITY)


; utility functions
(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn int-list []
  "Read a line of input of space-separated integers.  Strict."
  (ffor [s (string/split (read-line) #"\s+")] (Integer/parseInt s)))


; bestspot
(defn ofloyd-warshall [nodes paths]
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

(defn floyd-warshall [nodes paths]
  (letfn
    [(new-row [distance k]
       (let
         [rows    (vec (for [_ nodes] (atom nil)))
          chunks  (partition-all 
                   (-> (/ (count nodes) *threads*) ceil int) 
                   nodes)
          prev-dist #((deref (distance %1)) %2)
          dochunk (fn [chunk]
                    (doseq [a chunk]
                      (reset! (rows a) 
                        (vec
                          (for [b nodes]
                            (min (prev-dist a b)
                                 (+ (prev-dist a k) (prev-dist k b))))))))
         ]
         ;(println "new row!!")
         ;(println distance)
         (dorun (pmap dochunk chunks))
         rows))
    ]
    (reduce new-row paths nodes)))

; get input zero-indexed
(defn bestspot-input []
  (let
    [global (atom {})
     add   #(swap! global assoc ,, %1 %2)
     add-m #(swap! global merge ,, %)
    ]
    (add-m 
      (zipmap [:p :f :c] (int-list)))
    (add :favorites
      (ffor [_ (range (@global :f))] 
          (dec (first (int-list)))))
    (add :paths-one-way
      (map-comp [_ (range (@global :c))]
        (let [[a b t] (int-list)] [[(dec a) (dec b)] t])))
    @global))

(defn bestspot []
  (let 
    [
     {:keys [p f c favorites paths-one-way]} (bestspot-input)
     nodes  (range p)
     pathsm (merge 
              paths-one-way
              (map-comp [[[a b] t] paths-one-way] [[b a] t])
              (map-comp [a nodes] [[a a] 0]))
     paths  (->>
              (for [b nodes] (pathsm [a b] inf))
              (vec ,,)
              (atom ,,)
              (for [a nodes] ,,)
              (vec ,,))
     times  (floyd-warshall nodes paths)
     totals (->
              #(apply + (for [b favorites] (@(times %) b)))
              (group-by ,, nodes))
     best   (->> 
              (keys totals) 
              (apply min ,,) 
              (totals ,,)
              (apply min ,,))
    ]
    (println totals)
    (println (inc best))
    (shutdown-agents)))

(bestspot)

