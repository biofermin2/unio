(defpackage :unio
  (:use :cl)
  (:export :cat :peel :seek :seek-files :sets)) 
(in-package :unio)

(set-dispatch-macro-character #\# #\!
  (lambda (stream char n)
    (declare (ignore char n))
    (do* ((ch (read-char stream nil nil) (read-char stream nil nil)))
         ((or (null ch) (eql ch #\newline)) (values))))) ; => T


(defun parsed-list (l)
  (let ((depth 0)
	(cnt -1))
    (map 'list #'(lambda (x) (cond ((eq x #\() (incf depth))
				   ((eq x #\)) (decf depth)))
		   (list (incf cnt) x depth))
	 l)))				; => PARSED-LIST

(defun blank-p (c)
  (when (find c '(#\Space #\Return #\Newline #\| #\/ #\\))
      t))				; => BLANK-P
(defun paren-p (c)
  (when (find c '(#\( #\) #\[ #\] #\{ #\} #\< #\>))
    t))					; => PAREN-P

(defun blank/paren-p (c)
  (or (blank-p c)
      (paren-p c)))			; => BLANK/PAREN-P

(defmacro set-readtable (opt &body body)
  "opt:upcase,downcase,preserve,invert"
  `(let ((*readtable* (copy-readtable nil)))
     (setf (readtable-case *readtable*) ,opt)
     ,@body))				; => SET-READTABLE


(defun get-core (pl pos-lst &key (skin 0))
  (mapcar #'(lambda (x) (let* ((p (car x))
			       (cdr-pl (mapcar #'cdr pl))
			       (first-part (subseq cdr-pl 0 p))
			       (rest-part (subseq cdr-pl p))
			       (k-depth (- (third x) skin))
			       (open-pos (position `(#\( ,k-depth) first-part :test #'equal :from-end t))
			       (close-pos (+ p (position `(#\) ,(1- k-depth)) rest-part :test #'equal) 1)))
			  (coerce (subseq (mapcar #'cadr pl) open-pos close-pos) 'string)))
	  pos-lst))			; => GET-CORE

(defun seek (k l &key (skin 0) (dup nil) (str t) (exact nil) (opt :upcase))
  (check-type k string)
  (let ((sl (princ-to-string l))
	(c (elt k 0))
	pos
	(k-len (length k))
	(pl (parsed-list sl)))
    (mapcar #'(lambda (x) (let ((p (car x)))
			    (cond ((not exact) (when (and (string-equal c (cadr x))
							  (string-equal k (subseq sl p (+ p k-len))))
						 (push x pos)))
				  (exact (when (and (string-equal c (cadr x))
						    (string-equal k (subseq sl p (+ p k-len)))
						    (blank/paren-p (elt (subseq sl (+ p k-len)) 0))
						    (blank/paren-p (elt (subseq sl (- p 1)) 0)))
					   (push x pos))))))
	    pl)
    (let ((base (get-core pl (reverse pos) :skin skin)))
      (unless dup
	(setf base (remove-duplicates base :test #'string-equal)))
      (if str
	  base
	  (set-readtable opt (mapcar #'read-from-string base)))))) ; => SEEK

(defun cat (&rest files)
  (dolist (f (apply #'directory files))
	      (with-open-file (in f :direction :input)
		(loop :for line = (read in nil nil)
		   :while line
		   :do (print line *standard-output*))))) ; => CAT

(defmacro seek-files (key &rest files)
  `(seek ,key (with-output-to-string (*standard-output*)
		(cat ,@files)) :str nil))	; => SEEK-FILES

(defmacro sets (var sexp)
  `(setf ,var (with-input-from-string
		  (s (with-output-to-string (*standard-output*)
		       ,sexp))
		(read s))))		; => SETS

(defun peel (lst)
  (apply #'append lst))			; => PEEL


