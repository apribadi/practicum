(ns bestspot
  (:use 
    [clojure.contrib.seq-utils]
    [clojure.contrib.combinatorics])
  (:require
    [clojure.contrib.str-utils2 :as string]))


(defn int-list []
  "Read a line of input of space-separated integers.  Strict."
  (doall (for [s (string/split (read-line) #"\s+")] (Integer/parseInt s))))

(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(def inf Float/POSITIVE_INFINITY)

(defn floyd-warshall [nodes paths]
  (letfn 
    [(new-row [distances k]
       (->>
         (map-comp 
           [a nodes :let [a-dist (distances [a k])] :when a-dist
            b nodes :let [b-dist (distances [k b])] :when b-dist
           ]
           [[a b] (+ a-dist b-dist)])
         (merge-with min distances ,,)))
    ]
    (reduce new-row paths nodes)))

(defn bestspot-input []
  (let [inputs (atom {})
        put (fn [key val] (swap! inputs assoc key val))
        get (fn [key] (@inputs key))]
    (let [[p f c] (int-list)]
      (put :p p)
      (put :f f)
      (put :c c))
    (->>
      (first (int-list))
      (for [_ (range (get :f))] ,,)
      (doall ,,)
      (put :favorites ,,))
    (->>
      [[a b] t]
      (let [[a b t] (int-list)] ,,)
      (map-comp [_ (range (get :c))] ,,)
      (put :paths-one-way ,,))
    @inputs))

(defn bestspot []
  (let 
    [
     {:keys [p f c favorites paths-one-way]} (bestspot-input)
     nodes  (range 1 (inc p))
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
    (println best)
    (shutdown-agents)))

(bestspot)

