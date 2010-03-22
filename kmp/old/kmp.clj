(ns cmp
  (:use
    clojure.contrib.core)
  (:require
    clojure.contrib.string :as string))


; utility definitions
(defmacro hash-c [& args]
  `(->> (for ~@args) (apply concat ,,) (apply hash-map ,,)))
(defmacro ffor [& args]
  `(doall (for ~@args)))
(defn parse-line []
  (ffor [s (split #"\s+" (read-line))] (read-string s)))


; main stuff

(defn proper-prefixes [s]
  (for [k (range (count s))] (string/take k)))

(defn is-prefix [s prefix]
  (every? identity (map = s prefix)))

(defn main []
  (let 
    [ s (read-line)
      w (read-line)
    ]


(main)

