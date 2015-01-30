(ns alfa.one)

(defn euler1a [lim]
  (loop [i 3 res 0]
    (if (>= i lim)
      res
      (recur (inc i)
             (if (or (zero? (rem i 3))
                     (zero? (rem i 5)))
               (+ res i)
               res)))))

(defn euler1b [lim]
  (- (+ (reduce + (range 3 lim 3)) (reduce + (range 5 lim 5)))
     (reduce + (range 15 lim 15))))

(defn euler1c [lim]
  (reduce + (filter #(or (zero? (rem % 3))
                         (zero? (rem % 5))) (range 1 lim))))

(defn euler2a [lim]
  (loop [a 1 b 0 res 0]
    (if (> a lim)
      res
      (recur (+ a b) a (if (even? a) (+ a res) res)))))

(defn euler2b [lim]
  (->> (iterate #(conj % (+ (last %) (last (butlast %)))) [1 1])
       (take-while #(< (last %) lim))
       last (filter even?) (reduce +)))
