(defmacro all (&body body)
  "Evaluates all subforms and returns NIL if any return NIL"
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for form in body 
	    collecting `(unless ,form (setf ,result nil)))
       ,result)))