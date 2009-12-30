(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (listp lst) (null (cdr lst))))

(defun append1 (lst a)
  (append lst (list a)))

(defun conc1 (lst a)
  (nconc lst (list a)))