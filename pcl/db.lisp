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
  (if (y-or-n-p "Add another?")
      (add-cds)
      *db*))

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

(defun make-comparison-expr (key val)
  `(equal (getf cd ,key) ,val))

(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest fields)
  `(lambda (cd) (and ,@(make-comparisons-list fields))))

(defun set-values (&rest vals)
  (lambda (cd)
    (mapplist #'(lambda (key val)
		  (setf (getf cd key) val))
	      vals)))

(defun update (set-f where-f)
  (mapcar set-f
	  (remove-if-not where-f *db*)))

(defun delete-rows (set-f)
  (setf *db* (remove-if set-f *db*)))