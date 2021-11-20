;;;; unio.lisp
(defpackage #:unio
    (:use #:cl)
    (:export :sets :seek :seek-files))
(in-package #:unio)

(defun seek (key list &optional (skin 0) (rm-dup t)
		    &aux (depth 0) (key-len (length key)) pos)
  (check-type key string)
  (let ((list (format nil "~(~a~)" list)))
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
			  :finally (if rm-dup
				       (format t "~a" (remove-duplicates total :test #'equal))
				       (format t "~a" total)))))))


(defmacro seek-files (key &optional (buff) &rest files)
  `(check-type ,key string)
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

