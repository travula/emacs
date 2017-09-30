(defun find-unbalanced-parentheses ()
  (interactive)
  (goto-char (point-min))
  (while (< (point) (point-max))
    (find-matching-scheme-paren)
    (next-open-paren-or-end)))

(defun next-open-paren-or-end ()
  (cond
   ((>= (point) (point-max))
    (error "All parentheses appear balanced"))
   ((eq (char-after (point)) ?( ;)
			       )
	nil) ;; do nothing
   (t (let ((ignore nil))
	(while (or (eq (char-after (point)) ?(;)
					    )
		       (>= (point) (point-max)))
	   (let ((c (char-after (point))))
	     (cond
	      ((and (not ignore) 
		    (eq c ?;))
	       (end-of-line))
	      ((eq c ?\\) (forward-char))
	      ((eq c ?" ;"
		   ) (setq ignore (not ignore))))
	       (forward-char))
	      ((and (not ignore)
		    (or (eq c ;(
			    ?))
                        (eq c ;[
			    ?])))
	       (error "Misplaced %c" c)))))))

	

(defun push-stack (stack elt)
  (print "Push" (get-buffer "*scratch*"))
  (let ((ptr (aref stack 0)))
    (aset stack 0 (+ 1 ptr))
    (if (>= ptr (length stack))
	nil
      (progn
	(aset stack (+ 1 ptr) elt)
	t))))
(defun pop-stack (stack)
  (print "Pop" (get-buffer "*scratch*"))
  (let ((ptr (aref stack 0)))
    (if (= ptr 0)
	nil
      (progn
	(aset stack 0 (- ptr 1))
	t))))
(defun top-stack (stack)
  (if (= (aref stack 0) 0)
      (error "Empty Stack")
    (aref stack (aref stack 0))))
(defun empty-stack (stack)
  (= (aref stack 0) 0))
(defun clear-stack (stack)
  (aset stack 0 0)
  stack)

(defvar *is* (make-vector 200 0))
(defvar *ps* (make-vector 200 0))

(defun find-matching-scheme-paren ()
  (interactive)
  (let ((p-stack (clear-stack *ps*))
	(i-stack (clear-stack *is*)))
    (let ((ignore nil))
      (let ((out nil))
	(while (not out)
	  (let ((c (char-after (point))))
	    (cond
	     ((and (not ignore) (eq c ?( ;)
		      )) (push-stack p-stack ?( ;)
					      )
			 (push-stack i-stack (point)))
	     ((and (not ignore) (eq c ?[ ;]
		      )) (push-stack p-stack ?[ ;]
					       )
                         (push-stack i-stack (point)))
	     
	     ((eq c ?\\ ) (next-char-or-error p-stack i-stack))
	     ((eq c  ;)
		  ?") ;"
              (setq ignore (not ignore)))
             ((and (not ignore) (eq c ?;))
	      (end-of-line))
	     ((and (not ignore)
		   (or (eq c ; (
		       ?))
		       (eq c ; [
		           ?])
		       ))
	     (pop-or-error p-stack i-stack c))

	     )
	  (if (empty-stack p-stack)
	      (setq out (not out)))
	  (next-char-or-error p-stack i-stack))))))
	  'ok)

(defun next-char-or-error (ps is)
  (or (and (< (point) (point-max))
	   (progn (forward-char) t))
      (if (not (empty-stack ps))
	  (progn 
	    (goto-char (aref is 1))
	    (error "No match for this %c" (aref ps 1)))
	  nil)))

(defun closer (c)
  (if (eq c ?() ?)
      (if (eq c ?[) ?]
	  nil)))

(defun pop-or-error (ps is c)
 (or (and (not (empty-stack ps))
	  (eq c (closer (top-stack ps)))
	   (progn 
	     (pop-stack ps)
	     (pop-stack is)
	     t))
        (error "Misplaced %c" c)))


		
  



 


