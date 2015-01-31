(defparameter test-data '(1 3 2 4 7 6 5 10 9 8))
(load "raw.lisp")

(defun take (n xs)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (zerop n)
      nil
      (labels ((looper (i ls res)
		  (declare (fixnum i))
		  (if (zerop i)
		      res
		      (looper (1- i)
			 (rest ls)
			 (append res (list (first ls)))))))
	(looper n xs nil))))

(defun drop (n xs)
  (declare (optimize (speed 3))
	   (fixnum n))
  (if (null xs)
      xs
      (if (zerop n)
	  xs
	  (drop (1- n) (rest xs)))))

(defvar counter 0)

(defun msort (xs)
  (declare (optimize (speed 3)))
  (labels ((looper (ls1 ls2 res)
	      (cond
		((null ls1) (append res ls2))
		((null ls2) (append res ls1))
		(:else
		 (if (<= (first ls1) (first ls2))
		     (looper (rest ls1)
			ls2
			(append res (list (first ls1))))
		     (looper ls1
			  (rest ls2)
			  (append res (list (first ls2)))))))))
    (cond ((null xs) xs)
	  ((null (rest xs)) xs)
	  (:else (let ((pivot (truncate (/ (length xs) 2))))
		   (looper (msort (take pivot xs))
		      (msort (drop pivot xs))
		      '()))))))

;; This one runs in 159 seconds
(defun inversion (xs)
  (declare (optimize (speed 3)))
  (labels ((looper (ls1 ls2 res)
	      (cond
		((null ls1) (append res ls2))
		((null ls2) (append res ls1))
		(:else
		 (if (<= (first ls1) (first ls2))
		     (looper (rest ls1)
			ls2
			(append res (list (first ls1))))
		     (progn
		       (setq counter (+ counter (length ls1)))
		       (looper ls1
			    (rest ls2)
			    (append res (list (first ls2))))))))))
    (cond ((null xs) xs)
	  ((null (rest xs)) xs)
	  (:else (let ((pivot (truncate (/ (length xs) 2))))
		   (looper
		      (inversion (take pivot xs))
		      (inversion (drop pivot xs))
		      '()))))))

(defun inverse (xs ctr)
  (declare (optimize (speed 3)))
  (labels ((looper (ls1 ls2 res count)
	      (cond
		((null ls1) (list (append res ls2) count))
		((null ls2) (list (append res ls1) count))
		(:else
		 (if (<= (first ls1) (first ls2))
		     (looper (rest ls1)
			ls2
			(append res (list (first ls1)))
			count)
		     (looper ls1
			  (rest ls2)
			  (append res (list (first ls2)))
			  (+ count (length ls1))))))))
    (cond ((null xs) (list xs ctr))
	  ((null (rest xs)) (list xs ctr))
	  (:else (let* ((pivot (truncate (/ (length xs) 2)))
			(tmp1 (inverse (take pivot xs) 0))
			(tmp2 (inverse (drop pivot xs) 0)))
		   (looper (first tmp1)
		      (first tmp2)
		      '()
		      (+ (second tmp1)
			 (second tmp2))))))))






