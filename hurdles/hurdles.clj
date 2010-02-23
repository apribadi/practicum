(ns hurdles
  (:use 
    [clojure.contrib.seq-utils]
    [clojure.contrib.combinatorics]
    [clojure.contrib.generic.math-functions])
  (:require
    [clojure.contrib.str-utils2 :as string]))

(defn int-list []
  "Read a line of input of space-separated integers.  Strict."
  (doall (for [s (string/split (read-line) #"\s+")] (Integer/parseInt s))))

(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro for-times [[i times] & args]
  `(doall (for [~i (range ~times)] ~@args)))


(def inf Float/POSITIVE_INFINITY)

(defn floyd-warshall [nodes paths node-count]
  (letfn 
    [(new-row [distance k]
       (map-comp
         [a nodes :let [a-dist (distance [a k])] :when a-dist
          b nodes :let [b-dist (distance [k b])] :when b-dist]
         [[a b] (+ a-dist b-dist)]))
    ]
    (reduce new-row paths nodes)))

(defn hurdles-input []
  (let [inputs (atom {})
        in-assoc (fn [key val] (swap! inputs assoc key val))
        in-merge (fn [m] (swap! inputs merge ,, m)
        get (fn [key] (@inputs key))]
    (in-merge 
      (zipmap [:n :m :t] (int-list)))
    (in-assoc :favorites
      (for-times [_ (get :f)] (first (int-list)))))))uu
    (->>
      [[a b] t]
      (let [[a b t] (int-list)] ,,)
      (map-comp [_ (range (get :c))] ,,)
      (put :paths-one-way ,,))
    @inputs))

(let [[n m t] (int-list)
      paths   (map-comp [_ (range m)] (let [[s e h] (int-list)] [[s e] h]))
      tasks   (doall (for [_ (range t)] (int-list)))
      rule    (fn [prev a b k] (min (prev [a b] inf) (max (prev [a k] inf) (prev [k b] inf))))
      heights (f-w (range 1 (inc n)) paths rule)
     ]
  (doseq [task tasks]
    (let [answer (heights task inf)]
      (if (< answer inf)
        (println answer)
        (println -1)))))


(defmacro map-comp [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(def inf Float/POSITIVE_INFINITY)



(defn bestspot []
  (let 
    [
     {:keys [p f c favorites paths-one-way]} (bestspot-input)
     nodes  (range 1 (inc p))
     paths  (merge 
              paths-one-way
              (map-comp [[[a b] t] paths-one-way] [[b a] t])
              (map-comp [a nodes] [[a a] 0]))
     times  (floyd-warshall nodes paths p)
     totals (->
              #(apply + (for [b favorites] (times [% b] inf)))
              (group-by ,, nodes))
     best   (->> 
              (keys totals) 
              (apply min ,,) 
              (totals ,,)
              (apply min ,,))
    ]
;    (println (* p p))
;    (println totals)
    (println best)
    (shutdown-agents)))

(bestspot)

