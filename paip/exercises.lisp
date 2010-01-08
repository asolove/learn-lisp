(defun print-cons (cons)
  (cond ((null cons) (princ "()"))
	((atom cons) (princ cons))
	(t (princ "( ")
	   (princ (car cons))
	   (princ " . ")
	   (print-cons (cdr cons))
	   (princ ")")
	   nil)))

(defun print-list (cons)
  (labels ((print-inner-list (cons)
	     (cond ((null cons))
		   ((atom cons) (princ " .") (princ cons))
		   (t (princ " ")
		      (princ (car cons)) 
		      (print-inner-list (cdr cons))))))
    (princ "(")
    (print-inner-list cons)
    (princ " )")))