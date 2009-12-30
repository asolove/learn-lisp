(defmacro all (&body body)
  "Evaluates all subforms and returns NIL if any return NIL"
  `(let ((res t))
     (loop for form in ,body collecting (and form res))))