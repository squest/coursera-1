(load "clojure.lisp")

(defun counter (x ls res)
  (if (empty? ls)
      res
      (if (> x (first ls))
	  (counter x (rest ls) (inc res))
	  (counter x (rest ls) res))))

(defun dfile ()
  (read (open "raw.edn")))

(defun solution (ls)
  (sum (mapcar #'(lambda (x) (counter x
				 (rest ls)
				 0))
	       ls)))
