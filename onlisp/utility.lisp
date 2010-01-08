;;;; On Lisp utility functions
;;; 4.1
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (listp lst) (null (cdr lst))))

(defun append1 (lst a)
  (append lst (list a)))

(defun conc1 (lst a)
  (nconc lst (list a)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;; 4.2 
(defun longer (x y)
  (labels ((longer-list (x y)
	     (and (consp x)
		  (or (null y)
		      (longer-list (cdr x) (cdr y)))))) 
    (if (and (listp x) (listp y))
	(longer-list x y)
	(> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (atom lst)
      (let ((val (funcall fn atom)))
	(if val
	    (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "group by zero"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if rest
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;; 4.3 Tree functions
(defun flatten (tree)
  (cond ((null tree) nil)
	((listp tree) (append (flatten (car tree))
			      (flatten (cdr tree))))
	(t (list tree))))

(defun prune (fn tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree)
			 (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall fn (car tree))
			       acc
			       (cons (car tree) acc)))))))
    (rec tree nil)))














(defun tree-map (fn tree)
  (labels ((iter (val)
	     (if (consp val)
		 (mapcar #'iter val)
		 (funcall fn val))))
    (iter tree)))


(defun tree-map (fn tree)
  (cond ((null tree) nil)
	((consp tree) (cons (tree-map fn (car tree))
			    (tree-map fn (cdr tree))))
	(t (funcall fn tree))))

(defun tree-map (fn tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse tree))
		   ((consp (car tree))
		    (rec (cdr tree) 
			 (rec (car tree) acc)))
		   (t (rec (cdr tree)
			   (cons (funcall fn (car tree)) acc))))))
    (rec tree nil)))

