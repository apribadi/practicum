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
     paths  (merge 
              paths-one-way
              (map-comp [[[a b] t] paths-one-way] [[b a] t])
              (map-comp [a nodes] [[a a] 0]))
     times  (floyd-warshall nodes paths)
     totals (->
              #(apply + (for [b favorites] (times [% b] inf)))
              (group-by ,, nodes))
     best   (->> 
              (keys totals) 
              (apply min ,,) 
              (totals ,,)
              (apply min ,,))
    ]
    (println (inc best))
    (shutdown-agents)))

(bestspot)

