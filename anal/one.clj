(ns anal.one
  (:require [clojure.string :as cs]))

(def cfile (slurp "raw.txt"))
(def mls (map read-string (cs/split cfile #"\r\n")))

(defn convert
  [fname]
  (spit fname (into '() mls)))


