


;; Bindings forms

(defvar no-bindings '((t . t)))

(defun no-bindings? (bindings)
  (eql no-bindings bindings))

(defvar fail nil)

(defun fail? (bindings) (null bindings))

(defun get-binding (var bindings) (assoc var bindings))
(defun binding-var (binding) (car binding))
(defun binding-val (binding) (cdr binding))

(defun binding-push (var val bindings)
  (acons var val
	 (if (no-bindings? bindings)
	     nil
	     bindings)))


;; Pattern match
(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((fail? bindings) fail)
	((variable-pattern-p pattern) 
	 (variable-pattern-match pattern input bindings))
	((eql pattern input) bindings)
	((single-pattern-p pattern) 
	 (single-pattern-match pattern input bindings))
	((multiple-pattern-p pattern) 
	 (multiple-pattern-match pattern input bindings))
	((consp pattern) 
	 (pat-match (cdr pattern) (cdr input)
		    (pat-match (car pattern) (car input) bindings)))))

;; Variable pattern

(defun variable-pattern-p (pattern)
  (and (symbolp pattern)
       (eql (char (string pattern) 0) #\?)))

(defun variable-pattern-match (pattern input bindings)
  (let ((binding (get-binding pattern bindings)))
    (cond ((null binding) (binding-push pattern input bindings))
	  ((not (eql (binding-val binding) input)) fail)
	  (t bindings))))

;; Single (general) pattern
(defun single-pattern-p (pattern)
  (and (consp pattern) 
       (variable-pattern-p (first pattern))
       (single-pattern-fn (first pattern))))

(defun single-pattern-fn (name)
  (get name 'single-pattern))

(defun single-pattern-match (pattern input bindings)
  (funcall (single-pattern-fn (first pattern))
	   (rest pattern) input bindings))

(setf (get '?is 'single-pattern) 'match-is)

(defun match-is (pattern input bindings)
  "(?x oddp) '3' no-bindings => '((?x . 3))"
  (let* ((var (first pattern))
	 (f (second pattern))
	 (new-bindings (variable-pattern-match var input bindings)))
    (or (and new-bindings (funcall f input) new-bindings)
	fail)))

(setf (get '?or 'single-pattern) 'match-or)

(defun match-or (patterns input bindings)
  "(?x ?y) '2 '((?x . 4)) => '((?y . 2) (?x . 4))"
  (cond ((null patterns) fail)
	((pat-match (car patterns) input bindings))
	(t (match-or (cdr patterns) input bindings))))

(setf (get '?and 'single-pattern) 'match-and)

(defun match-and (patterns input bindings)
  "(?x ?y) '2 '((?x . 2) (?y . 2)) => succeeds"
  (if (null patterns)
      bindings
      (let ((new-bindings (pat-match (car patterns) input bindings)))
	(if new-bindings
	    (match-and (cdr patterns) input new-bindings)
	    fail))))

(setf (get '?not 'single-pattern) 'match-not)

(defun match-not (patterns input bindings)
  (cond ((null patterns) bindings)
	((pat-match (car patterns) input bindings) fail)
	(t (match-not (cdr patterns) input bindings))))
  
	  
  

;; Multiple (general) pattern 
(defun multiple-pattern-p (pattern)
  nil)