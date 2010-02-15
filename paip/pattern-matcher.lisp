


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
  nil)

;; Multiple (general) pattern 
(defun multiple-pattern-p (pattern)
  nil)