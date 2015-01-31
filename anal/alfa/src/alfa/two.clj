(ns alfa.two)

;; This is the second time taking the anal course

(def random-data
  (shuffle (range 1 10000)))

(defn msort
  "Merge sort non tail-recursion, will cause stack overflow obviously"
  [xs]
  (let [merge-parts
        (fn merge-parts [xs1 xs2]
          (cond
           (empty? xs1) xs2
           (empty? xs2) xs1
           :else (let [[a & as] xs1 [b & bs] xs2]
                   (if (<= a b)
                     (concat [a] (merge-parts as xs2))
                     (concat [b] (merge-parts xs1 bs))))))
        pivot (quot (count xs) 2)]
    (cond (empty? xs) []
          (empty? (rest xs)) xs
          :else
          (merge-parts (msort (take pivot xs))
                       (msort (drop pivot xs))))))

(def pr-data
  (->> (slurp "resources/IntegerArray.txt")
       (remove #{\return})
       (partition-by #(= % \newline))
       (remove #{[\newline]})
       (map #(read-string (apply str %)))))

(defn tail-msort
  "Semi tail-recursive msort, where the merge-parts is tail-recursive
  while the main sorting part is not tail-recursive"
  [xs]
  (let [merge-parts
        (fn [xs1 xs2]
          (loop [ls1 xs1 ls2 xs2 res []]
            (cond
             (empty? ls1) (concat res ls2)
             (empty? ls2) (concat res ls1)
             :else
             (let [[a & as] ls1 [b & bs] ls2]
               (if (<= a b)
                 (recur as ls2 (conj res a))
                 (recur ls1 bs (conj res b)))))))
        pivot (quot (count xs) 2)]
    (cond (empty? xs) []
          (empty? (rest xs)) xs
          :else
          (merge-parts (tail-msort (take pivot xs))
                       (tail-msort (drop pivot xs))))))



(def test-data [1 3 2 4 7 6 5 10 9 8])

(defn stupid-inversion
  [xs]
  (loop [[l & ls] xs res (int 0)]
    (if (empty? ls)
      res
      (recur ls
             (+ res (loop [[y & ys] ls res1 (int 0)]
                      (if (nil? y)
                        res1
                        (recur ys
                               (if (< y l) (inc res1) res1)))))))))

(def counter (ref 0))
;; This one runs in 171 seconds
(defn inversion
  "Semi tail-recursive inversion counting"
  [xs]
  (let [merge-parts
        (fn [xs1 xs2]
          (loop [ls1 xs1 ls2 xs2 res []]
            (cond
             (empty? ls1) (concat res ls2)
             (empty? ls2) (concat res ls1)
             :else
             (let [[a & as] ls1 [b & bs] ls2]
               (if (<= a b)
                 (recur as ls2 (conj res a))
                 (do (dosync (alter counter + (count ls1)))
                     (recur ls1 bs (conj res b))))))))
        pivot (quot (count xs) 2)]
    (cond (empty? xs) []
          (empty? (rest xs)) xs
          :else
          (merge-parts (inversion (take pivot xs))
                       (inversion (drop pivot xs))))))








