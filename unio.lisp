;;;; unio.lisp
(defpackage #:unio
    (:use #:cl)
    (:export :sets :seek :seek-files))
(in-package #:unio)

(defun split (dt str)
       (let ((pos (search dt str))
	     (size (length dt)))
	 (if pos
	     (cons (subseq str 0 pos)
		   (split dt (subseq str (+ pos size))))
	     (list  str))))	

;; ok [2021-11-21 12:40:45]
(defun seek (key list &optional (skin 0) (rm-dup t) (dt ".")
		 &aux (depth 0) pos)
  (loop :with key = (format nil "~(~a~)" key)
	:with list = (format nil "~(~a~)" list)
	:for k :in (split dt key)
	:do
	(loop :with k-len = (length k)
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
			       :finally (if rm-dup
					    (format t "~a" (remove-duplicates total :test #'equal))
					  (format t "~a" total))))))) ; => SEEK


(defmacro seek-files (key &optional (buff) &rest files)
  `(flet ((cat-files (&rest files)
	   (dolist (f files)
	     (with-open-file (in f :direction :input)
	       (loop
		 (if (setq buff (read in nil))
		     (format t "~(~a~)~%" buff)
		     (return)))))))
    (seek ,key (with-output-to-string (*standard-output*)
		  (cat-files ,@files))))) ; => SEEK-FILES

;; ok [2021-11-02 14:01:43]
(defmacro sets (var sexp)
  `(setf ,var (with-input-from-string
		  (s (with-output-to-string (*standard-output*)
		       ,sexp) :index len)
		(read s))))		; => SETS

