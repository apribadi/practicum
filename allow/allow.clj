(ns allow)

(defn int-list []
  (for [s (.split (read-line) " ")] 
       (Integer/parseInt s)))

(let [[n c] (int-list)
      coins (->> (repeatedly int-list)
              (take n)
              (apply concat)
              (apply hash-map))]
 ; (println
 ;   (loop [buckets 0 coins coins]
      
  (println coins))


              

