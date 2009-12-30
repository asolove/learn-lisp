(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped?")))

(defun add-cds ()
  (add-record (prompt-for-cd))
  (when (y-or-n-p "Add another?") (add-to-db)))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
		  (with-standard-io-syntax
		   (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
		  (with-standard-io-syntax
		   (setf *db* (read in)))))

(defun select (query)
  (remove-if-not query *db*))

(defun mapplist (f p-list)
  (mapcar #'(lambda (lst) (apply f lst)) (alist->plist p-list)))

(defun alist->plist (a-list)
  (if (null a-list)
      '()
    (cons (list (car a-list) (cadr a-list))
	  (alist->plist (cddr a-list)))))

(defun all (&rest vals)
  (cond ((null vals) t)
	((null (car vals)) '())
	(t (apply #'all (cdr vals)))))

(defun where (&rest filters)
  (lambda (cd) 
    (apply #'all (mapplist #'(lambda (key val)
			       (equal (getf cd key) val))
			   filters))))

(defun set-values (&rest vals)
  (lambda (cd)
    (mapplist #'(lambda (key val)
		  (setf (getf cd key) val))
	      vals)))

(defun update (set-f where-f)
  (mapcar set-f
	  (remove-if-not where-f *db*)))