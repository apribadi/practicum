(ns fire
  (:use
     [clojure.set])
  (:require
     [clojure.contrib.seq :as sq]
     [clojure.contrib.string :as string]))

; utility definitions
(defmacro hash-c [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))

(defmacro ffor [& args]
  `(doall (for ~@args)))

(defn parse-line []
  (ffor [s (string/split #"\s+" (read-line))] (read-string s)))

(defn input []
  (let [[height width] (parse-line)
        scene          (vec (ffor [_ (range height)] (vec (read-line))))
        coords         (for [r (range height) c (range width)] [r c])
        passable       (set (filter #(distinct? \# \F (get-in scene %)) coords))
        joe            (set (filter #(= \J (get-in scene %)) coords))
        fire           (set (filter #(= \F (get-in scene %)) coords))]
    [height width passable joe fire]))

(defn border? [rc height width]
  (let [[r c] rc]
    (or (= r 0) (= r (dec height))
        (= c 0) (= c (dec width)))))

(def deltas [[-1 0] [0 -1] [0 1] [1 0]])

(defn adjs [rc]
  (for [d deltas] (map + rc d)))


(defn time-to-escape [height width passable joe fire]
  (loop [t 0
         passable passable 
         joe joe 
         fire fire]
    (cond
      (some #(border? % height width) joe)
        (inc t)
      (empty? joe)
        false
      :else
        (let [newfire (-> (set (mapcat adjs fire))
                        (difference ,, fire) 
                        (intersection ,, passable))
              newjoe  (-> (set (mapcat adjs joe))
                        (difference ,, joe newfire)
                        (intersection ,, passable))
              newpass (difference passable newfire newjoe)]
          (recur (inc t) newpass newjoe newfire)))))

(if-let [t (apply time-to-escape (input))]
  (println t)
  (println "IMPOSSIBLE"))

