(defun comment-out-region-1 (start end)
  "This function places lisp-style comments at the
   beginning of each line in the region"
  (save-excursion 
    (goto-char start);      
    (catch 'commenting-at-end-of-buffer
      (while (<= (point) end)
	(insert comment-start)
	(setq end (+ 1 end))
	(if (= 1 (forward-line 1)) ;;; If end of buffer
	    (throw 'commenting-at-end-of-buffer nil))))))
(defun comment-out-region ()
  (interactive)
  (comment-out-region-1 (region-beginning) (region-end)))


(defvar o-paren (string-to-char "("))
(defvar o-bracket (string-to-char "["))
(defvar o-brace (string-to-char "{"))

(defvar c-paren (string-to-char ")"))
(defvar c-bracket (string-to-char "]"))
(defvar c-brace (string-to-char "}"))

(defvar dbl-quote (string-to-char "\""))

(defun complete-parens-backwards
 (search-point paren-weight bracket-weight brace-weight)
 (let ((not-in-quote 't))
   (while (> search-point 0) 
     (if not-in-quote
	 (let ((ch (char-after search-point)))
	   (if (equal ch c-paren)
	       (setq paren-weight (+ 1 paren-weight))
	     (if (equal ch c-bracket)
		 (setq bracket-weight (+ 1 bracket-weight))
	       (if (equal ch c-brace)
		   (setq brace-weight (+ 1 brace-weight))
		 (if (equal ch o-paren)
		     (let ((nw (- paren-weight 1)))
		       (if (< nw 0)
			   (insert ")")
		           (setq paren-weight nw)))
		   (if (equal ch o-bracket)
		       (let ((nw (- bracket-weight 1)))
			 (if (< nw 0)
			     (insert "]")
			     (setq bracket-weight nw)))
		     (if (equal ch o-brace)
			 (let ((nw (- brace-weight 1)))
			   (if (< nw 0)
			       (insert "}")
			       (setq brace-weight nw)))
		       (if (equal ch dbl-quote)
			   (setq not-in-quote (not not-in-quote)))))))))))
     (setq search-point (- search-point 1)))))
			   

(defun complete-parens-from-point
  ()
  (interactive)
  (complete-parens-backwards (point) 0 0 0))
		     
(global-set-key "]" 'complete-parens-from-point)
;; (global-set-key ";" 'comment-out-region)



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
   ((eq (char-after (point)) ?\( ;)
			       )
	nil) ;; do nothing
   (t (let ((ignore nil))
	(while (or (eq (char-after (point)) ?\(;)
					    )
		   (>= (point) (point-max)))
	   (let ((c (char-after (point))))
	     (cond
	      ((and (not ignore) 
		    (eq c ?\;))
	       (end-of-line))
	      ((eq c ?\\) (forward-char))
	      ((eq c ?\" ;"
		   ) (setq ignore (not ignore))
	       (forward-char))
	      ((and (not ignore)
		    (or (eq c ;(
			    ?\))
                        (eq c ;[
			    ?\])))
	       (error "Misplaced %c" c)))))))))

	

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
	     ((and (not ignore) (eq c ?\( ;)
		      )) (push-stack p-stack ?\( ;)
					      )
			 (push-stack i-stack (point)))
	     ((and (not ignore) (eq c ?\[ ;]
		      )) (push-stack p-stack ?\[ ;]
					       )
                         (push-stack i-stack (point)))
	     
	     ((eq c ?\\ ) (next-char-or-error p-stack i-stack))
	     ((eq c  ;)
		  ?\") ;"
              (setq ignore (not ignore)))
             ((and (not ignore) (eq c ?;))
	      (end-of-line))
	     ((and (not ignore)
		   (or (eq c ; (
		       ?\))
		       (eq c ; [
		           ?\])
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
  (if (eq c ?\() ?\)
      (if (eq c ?\[) ?\]
	  nil)))

(defun pop-or-error (ps is c)
 (or (and (not (empty-stack ps))
	  (eq c (closer (top-stack ps)))
	   (progn 
	     (pop-stack ps)
	     (pop-stack is)
	     t))
        (error "Misplaced %c" c)))


;; This is a function which places chosen tex-group commands
;; such as \em etc around a region.

(defun isolate-region (start-string end-string r-start r-end)
  (save-excursion
    (goto-char r-start)
    (insert start-string)
    (goto-char (+ r-end (length start-string)))
    (insert end-string)))


(defmacro deftexgrouper (name key ss es)
 (list 'progn
  (list 'defun name '() 
     '(interactive)
     (list 'isolate-region ss es '(region-beginning) '(region-end)))
  (list 'global-set-key key (list 'quote name))))

(deftexgrouper tex-math-region   "$"    "$" "$")
(deftexgrouper tex-math-region   "4"    "$" "$")
(deftexgrouper tex-emphasize-region   "/"    "{\\em " "\\/}")
(deftexgrouper tex-italicize-region   "?"    "{\\it " "\\/}")
(deftexgrouper tex-boldize-region     ","    "{\\bf " "\\/}")
(deftexgrouper tex-helveticize-region "<"    "{\\sf " "\\/}")
(deftexgrouper tex-enumerate-region   "."    "\\begin{enumerate}" 
                                               "\\end{enumerate}")
(deftexgrouper tex-verbatim-region    ">"    "\\begin{verbatim}"
                                               "\\end{verbatim}")
(deftexgrouper tex-center-region      "-"    "\\begin{center}"
                                               "\\end{center}")

(deftexgrouper tex-paragraph-region      "p"    "\\begin{paragraph}\n\\noindent"
                                               "\\end{paragraph}")

;;; Some emacs stuff.

(defun delete-all-whitespace ()
  (interactive)
  (let ((done nil)
	(deleted-once nil))
    (while (and (not done)
		(< (point-min) (point)))
      (let ((c (char-after (- (point) 1))))
	(cond 
	 ((char-equal c ? )
	  (delete-backward-char 1)
	  (setq deleted-once t))
	 ((char-equal c ?	)
	  (delete-backward-char 1) 
	  (setq deleted-once t))
	 ((not deleted-once)
	  (progn (delete-backward-char 1)
		 (setq done t)))
	 (t (setq done t)))))))

(global-set-key ""  'delete-all-whitespace)


(defun strip-forward-sexp ()
  "Removes outermost parentheses of the immediately following sexp"
  (interactive)
  (save-excursion 
    (let* ((close (progn (forward-sexp)  (- (point) 1)))
	   (open  (progn (backward-sexp) (point))))
      (goto-char close) (delete-char 1)
      (goto-char  open) (delete-char 1))))


(defun strip-backward-sexp ()
  "Removes outermost parentheses of the  immediately preceding sexp"
  (interactive)
  (save-excursion 
    (backward-sexp) (strip-forward-sexp)))









