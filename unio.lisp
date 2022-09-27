;;(ql:quickload '(:cl-debug-print) :silent t) ; => (:CL-DEBUG-PRINT)
(defpackage :unio                      
  (:use :cl)           
  (:export :scan :%s :mappend :string-cat :set-readtable :depth-lst :peel :seek :cat :sets)) ; =>#<PACKAGE "UNIO"> 
(in-package :unio)                      ; =>#<PACKAGE "UNIO"> 
;;(debug-print:use-debug-print)           ; =>T 
(defparameter depth-lst ())             ; =>DEPTH-LST 

(defun parsed-list (l)
  (let ((depth 0)
	(cnt -1))
    (map 'list #'(lambda (x) (cond ((eq x #\() (incf depth))
				   ((eq x #\)) (decf depth)))
		   (list (incf cnt) x depth))
	 l)))                           ; =>PARSED-LIST 

(defun blank-p (c)
  (when (find c '(#\Space #\Return #\Newline #\| #\/ #\\))
    t))                                 ; =>BLANK-P 

(defun paren-p (c)
  (when (find c '(#\( #\) #\[ #\] #\{ #\} #\< #\>))
    t))                                 ; =>PAREN-P 

(defun blank/paren-p (c)
  (or (blank-p c)
      (paren-p c)))                     ; =>BLANK/PAREN-P 


(defmacro set-readtable (opt &body body)
  "opt:upcase,downcase,preserve,invert"
  `(let ((*readtable* (copy-readtable nil))
	 *read-eval*)
     (set-dispatch-macro-character #\# #\!
                                   (lambda (stream character n)
                                     (declare (ignore character n))
                                     (read-line stream nil nil t)
                                     nil))
     (setf (readtable-case *readtable*) ,opt)
     ,@body))                           ; =>SET-READTABLE 


(defun scan (key obj)
  (let ((key-len (length key))
         (start (search key obj)))
    (when start
        (values start (+ start key-len))))) ; =>SCAN 

(defun %s (old new obj)
  (multiple-value-bind (start end)
      (scan old obj)
    (if start
        (let* ((rest (subseq obj end))
               (result (%s old new rest)))
          (concatenate 'string (subseq obj 0 start) new result))
        obj)))                          ; =>%S 


(defun get-core (pl pos-lst &key (skin 0))
  (mapcar #'(lambda (x) (let* ((p (car x))
			       (cdr-pl (mapcar #'cdr pl))
			       (first-part (subseq cdr-pl 0 p))
			       (rest-part (subseq cdr-pl p))
			       (k-depth (- (third x) skin))
			       (open-pos (handler-case (position `(#\( ,k-depth) first-part :test #'equal :from-end t)
                                           (type-error (c))))
			       (close-pos (handler-case (+ p (position `(#\) ,(1- k-depth)) rest-part :test #'equal) 1)
                                            (type-error (c)))))
			  (values (coerce (subseq (mapcar #'cadr pl) open-pos close-pos) 'string)
				  (push k-depth depth-lst))))
	  pos-lst))                     ; =>GET-CORE 

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))                           ; =>ALAMBDA 

;;objは文字列に固定 シンボルの使用は不可にする。[2022-09-02 22:46:34]
(defun seek (key obj &key (skin 0) (dup nil) (dep nil) (str t) (exact nil) (opt :preserve))
  (check-type key string)
  (check-type obj string)
  (let* ((c (elt key 0))
	pos
	(k-len (length key))
	(pl (parsed-list obj)))
    (mapcar #'(lambda (x) (let ((p (car x)))
			    (cond ((not exact) (when (and (string-equal c (cadr x))
							  (string-equal key (subseq obj p (+ p k-len))))
						 (push x pos)))
				  (exact (when (and (string-equal c (cadr x))
						    (string-equal key (subseq obj p (+ p k-len)))
						    (blank/paren-p (elt (subseq obj (+ p k-len)) 0))
						    (blank/paren-p (elt (subseq obj (- p 1)) 0)))
					   (push x pos))))))
	    pl)
    (let ((base (get-core pl (reverse pos) :skin skin)))
      (unless dup
	(setf base (remove-duplicates base :test #'string-equal)))
      (when dep
	(print (reverse depth-lst)))
      (setf depth-lst nil)
      (if str
	  base
	  (set-readtable opt (mapcar #'read-from-string base)))))) ; =>SEEK 

;;(hl-key (format nil "[31m~a[0m" key))

(defun mappend (fn lst)
  "from PAIP"
  (apply #'append (mapcar fn lst)))     ; =>MAPPEND 

(defun cat (&rest files)
  (dolist (f (mappend #'directory files))
    (with-open-file (in f :direction :input)
      (loop :for line = (read-line in nil nil)
            :while line                         
            :collect (format t "~a~%" line))))) ; =>CAT 

(defun string-cat (&rest files)
  (with-output-to-string (*standard-output*)
                        (apply #'cat files))) ; =>STRING-CAT 


;;---
(defmacro sets (var sexp)
  `(setf ,var (with-input-from-string
		  (s (with-output-to-string (*standard-output*)
		       ,sexp))
		(read s))))             ; =>SETS 

(defun peel (lst)
  (apply #'append lst))                 ; =>PEEL 
