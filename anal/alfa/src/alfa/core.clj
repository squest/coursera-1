(ns alfa.core
  (:require [clojure.string :as cs]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]))

(def thefile "resources/raw.txt")
(def thefile2 "resources/raw1.edn")

(def sum (ref 0))
(def aone (atom 0))
(def refs (atom {}))

(def t2ls (read-string (slurp thefile2)))

(defn inversion
  [ls]
  (loop [[x & xs] ls i 1 res 0]
    (if (empty? xs)
      res
      (do (dosync (commute sum + (get @refs i 0)))
          (swap! refs dissoc i)
          (swap! refs assoc x (- x i))
          (let [dkeys (filter #(< % x) (keys @refs))]
            (swap! refs merge
                   (apply hash-map
                          (mapcat #(vector % (dec (get @refs %)))
                                  dkeys))))
          (recur xs (inc i) @sum)))))

(defn inverse1
  [ls]
  (loop [[x & xs] ls i 1 res 0]
    (if (empty? xs)
      res
      (if (> i x)
        (recur xs (inc i) res)
        (recur xs (inc i)
               (+ res
                  (loop [[y & ys] (take (- x i) xs) res1 0]
                    (if (empty? (rest ys))
                      res1
                      (recur ys (if (< y x)
                                  (inc res1)
                                  res1))))))))))

(defn inverse2
  [[idx n] ls]
  (let [size (- n idx)]
    (if (neg? size)
      0
      (loop [lst (->> (drop idx ls)
                      (take size)
                      (drop-while #(> (second %) n))) sum 0]
        (if (empty? lst)
          sum
          (recur (drop-while #(> (second %) n) (rest lst))
                 (inc sum)))))))

(def mt2ls (pmap vector (range) t2ls))

(defn solution
  [coll]
  (time (r/reduce + (pmap #(inverse2 % coll) coll))))

(defn inverse3
  [x xs]
  (count (filter (partial < x) xs)))

(defn sol1
  [ls]
  (loop [[x & xs] ls res 0]
    (if (empty? xs)
      res
      (recur xs (+ res (inverse3 x xs))))))

(defn merge'
  [l1 l2 res]
  (cond
   (empty? l1) (concat res l2)
   (empty? l2) (concat res l1)
   :else (let [[x & rl1] l1 [y & rl2] l2]
           (if (< x y)
             (merge' rl1 l2 (concat res [x]))
             (merge' l1 rl2 (concat res [y]))))))

(defn msort
  [xs]
  (if (= 1 (count xs))
    xs
    (let [counter (count xs)
          splitter (quot counter 2)]
      (if (odd? counter)
        (merge' (msort (take (inc splitter) xs))
                (msort (drop (inc splitter) xs))
                [])
        (merge' (msort (take splitter xs))
                (msort (drop splitter xs))
                [])))))


(comment

  (def cfile (slurp thefile))
  (def mls (pmap vector
                 (iterate inc 1)
                 (pmap read-string (cs/split cfile #"\r\n"))))

  (def dfile (read-string (slurp "testone.edn")))
  (def tls (pmap vector (iterate inc 1) dfile))
  (def ls (pmap vector (iterate inc 1) mls))
  (def t2ls (pmap vector
                  (iterate inc 1)
                  (read-string (slurp "testone.edn")))))




