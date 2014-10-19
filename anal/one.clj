(ns anal.one
  (:require [clojure.string :as cs]
            [clojure.core.reducers :as r]))

(comment

  (def cfile (slurp "raw.txt"))
  (def mls (pmap vector
                 (iterate inc 1)
                 (pmap read-string (cs/split cfile #"\r\n"))))

  (def dfile (read-string (slurp "testone.edn")))
  (def tls (pmap vector (iterate inc 1) dfile))
  (def ls (pmap vector (iterate inc 1) mls)))

(def t2ls (pmap vector
                (iterate inc 1)
                (read-string (slurp "testone.edn"))))

(def sum (ref 0))

(def refs (atom {}))



(defn inversion
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

(defn solution
  [coll]
  (time (r/reduce + (pmap #(inversion % coll) coll))))









