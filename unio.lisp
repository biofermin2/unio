;;; unio.lisp

(defpackage #:unio
    (:use #:cl)
    (:export :seek-files :seek))	; => #<PACKAGE "UNIO">

(in-package #:unio)			; => #<PACKAGE "UNIO">


;; 統合版
(defun seek (key list &optional (skin 0)
		    &aux (depth 0) (key-len (length key)) pos)
  (check-type key string)
  (check-type list string)
  (loop :for i :across list
	:do (cond ((eq i #\() (incf depth))
		  ((eq i #\)) (decf depth)))
	:collect (list i depth) :into parsed-list
	:when (and (eq i (elt key 0))
		   (equal key (subseq list cnt (+ cnt key-len))))
	  :collect (cons cnt depth) :into pos
	:count i :into cnt
	:finally (ignore-errors
		  (loop :for (p . d) :in pos
			:for first-part = (subseq parsed-list 0 p)
			:for rest-part = (subseq parsed-list p)
			:for key-depth = (- d skin)
			:for open-pos = (position `(#\( ,key-depth) first-part :test #'equal :from-end t)
			:for close-pos = (+ p (position `(#\) ,(1- key-depth)) rest-part :test #'equal) 1)
			:collect (subseq list open-pos close-pos) :into total
			:finally (print (remove-duplicates total :test #'equal)))))) ; => SEEK


;; 統合版
(defmacro seek-files (key &rest files)
  (check-type key string)
  (flet ((cat-files (&rest files)
	   (dolist (f files)
	     (with-open-file (in f :direction :input)
	       (let (buff)
		 (loop
		   (if (setq buff (read in nil))
		       (format t "~(~s~)~%" buff)
		       (return))))))))
    `(seek ,key (with-output-to-string (*standard-output*)
		  (cat-files ,@files))))) ; => SEEK-FILES

