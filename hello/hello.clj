(ns allow)

(defn int-list []
  (for [s (.split (read-line) " ")] 
       (Integer/parseInt s)))

(defn digits [number base]
  "Returns the digits of number, least significant first."
  (when (< 0 number)
    (cons (mod number base) 
          (digits (quot number base) base))))

(defn divides? [divisor number]
  (= 0 (mod number divisor)))

(defn prime? [n]
  (and (not (= 1 n))
       (not-any? #(divides? % n) (range 2 (int (Math/sqrt n))))))

(defn response-lines [n]
  (let
    [prime     (prime? n)
     b-eleven  (some #(= 1 %) (digits n 11))
     binary    (divides? 4 (apply + (digits n 2)))
     growing   (for [i (range 1 8)] (.substring "I<3Cows" 0 i))
     shrinking (reverse growing)]
    (cond 
      (and prime b-eleven binary) shrinking
      (and prime b-eleven)        growing
      prime                       shrinking
      :else                       growing)))

(let [[low high] (int-list)]
  (doseq [n (range low (inc high))]
    (doseq [line (response-lines n)]
      (println line))))


