;;;; unio.lisp
(defpackage #:unio
    (:use #:cl)
    (:export :sets :seek :seek-files))
(in-package #:unio)

;; ok [2022-05-24 22:01:37]
(defun seek (key list &key (skin 0) (rm-dup t) (str t) &aux (depth 0))
  (loop :with k = (format nil "~a" key)
	:with list = (format nil "~a" list)
	:with k-len = (length k)
	:for i :across list
	:do (cond ((eq i #\() (incf depth))
		  ((eq i #\)) (decf depth)))
	:collect (list i depth) :into parsed-list
	:when (and (eq i (elt k 0))
		   (equal k (subseq list cnt (+ cnt k-len))))
	:collect (cons cnt depth) :into pos
	:count i :into cnt
	:finally (ignore-errors
		   (loop :for (p . d) :in pos
			 :for first-part = (subseq parsed-list 0 p)
			 :for rest-part = (subseq parsed-list p)
			 :for k-depth = (- d skin)
			 :for open-pos = (position `(#\( ,k-depth) first-part :test #'equal :from-end t)
			 :for close-pos = (+ p (position `(#\) ,(1- k-depth)) rest-part :test #'equal) 1)
			 :collect (subseq list open-pos close-pos) :into total
		      :finally (progn
				 (when rm-dup
				   (setf total (remove-duplicates total :test #'equal)))
				 (if str
				     (format t "~s~%" total)
				     (format t "~a~%" total)))))))

(defmacro seek-files (key &rest files)
  `(flet ((cat-files (&rest files)
	    (let (buff)
	      (dolist (f (directory ,@files))
		(with-open-file (in f :direction :input)
		  (loop
		    (if (setq buff (read in nil))
			(format t "~(~a~)~%" buff)
			(return))))))))
    (seek ,key (with-output-to-string (*standard-output*)
		 (cat-files ,@files))))) ; => SEEK-FILES

;; ok [2021-11-02 14:01:43]
(defmacro sets (var sexp)
  `(setf ,var (with-input-from-string
		  (s (with-output-to-string (*standard-output*)
		       ,sexp))
		(read s))))		; => SETS

