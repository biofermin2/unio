(defpackage #:unio
    (:use #:cl)
    (:export :peel :sets :seek :seek-files))
(in-package #:unio)

(set-dispatch-macro-character #\# #\!
  (lambda (stream char n)
    (declare (ignore char n))
    (do* ((ch (read-char stream nil nil) (read-char stream nil nil)))
         ((or (null ch) (eql ch #\newline)) (values)))))

(defun seek (key list &key (skin 0) (rm-dup t) (str t) &aux (depth 0))
  (loop :with k = (format nil "~a" key)
	:and list = (format nil "~a" list)
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
		      :finally (when rm-dup
				 (setf total (remove-duplicates total :test #'equal)))
			(let ((spec (if str
					"~s~%"
					"~a~%")))
			  (format t spec total)))))) ; => SEEK

;; (defun seek (key list &key (skin 0) (rm-dup t) (str t) &aux (depth 0))
;;   (loop :with k = (format nil "~a" key)
;; 	:and list = (format nil "~a" list)
;; 	:with k-len = (length k)
;; 	:for i :across list
;; 	:do (cond ((eq i #\() (incf depth))
;; 		  ((eq i #\)) (decf depth)))
;; 	:collect (list i depth) :into parsed-list
;; 	:when (and (eq i (elt k 0))
;; 		   (equal k (subseq list cnt (+ cnt k-len))))
;;      :collect (cons cnt depth) :into pos
;;      :count i :into cnt
;; 	:finally (loop :for (p . d) :in pos
;; 		    :for first-part = (subseq parsed-list 0 p)
;; 		    :for rest-part = (subseq parsed-list p)
;; 		    :for k-depth = (- d skin)
;; 		    :for open-pos = (position `(#\( ,k-depth) first-part :test #'equal :from-end t)
;; 		    :for close-pos = (+ p (position `(#\) ,(1- k-depth)) rest-part :test #'equal) 1)
;; 		    :collect (subseq list open-pos close-pos) :into total
;; 		    :finally (return total))))
					; => SEEK


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

(defmacro sets (var sexp)
  `(setf ,var (with-input-from-string
		  (s (with-output-to-string (*standard-output*)
		       ,sexp))
		(read s))))		; => SETS

(defun peel (lst)
  (loop :for i :in lst
     :append i))			; => PEEL

(defun main (&rest args)
  `(seek-file ,args))


