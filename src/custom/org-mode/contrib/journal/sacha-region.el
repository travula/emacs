(defun org-calculate-tag-time (matcher &optional ts te)
  "Return the total minutes clocked in headlines matching MATCHER.
MATCHER is a string or a Lisp form to be evaluated, testing if a
given set of tags qualifies a headline for inclusion. TS and TE
are time start (inclusive) and time end (exclusive). Call with a
prefix to be prompted for TS and TE.

For example, to see how much time you spent on tasks tagged as
URGENT, call M-x wicked/org-calculate-tag-time RET URGENT RET. To
see how much time you spent on tasks tagged as URGENT today, call
C-u M-x wicked/org-calculate-tag-time RET URGENT RET . RET +1 RET."
  (interactive (list
		(read-string "Tag query: ")
		(if current-prefix-arg (org-read-date))
		(if current-prefix-arg (org-read-date))))
  ;; Convert strings to proper arguments
  (if (stringp matcher) (setq matcher (cdr (org-make-tags-matcher matcher))))
  (if (stringp ts)
      (setq ts (time-to-seconds (apply 'encode-time (org-parse-time-string ts)))))
  (if (stringp te)
      (setq te (time-to-seconds (apply 'encode-time (org-parse-time-string te)))))
  (let* ((re (concat "[\n\r]" outline-regexp " *\\(\\<\\("
		     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		     (org-re
		      "\\>\\)\\)? *\\(.*?\\)\\(:[[:alnum:]_@:]+:\\)?[ \t]*$")))
	 (case-fold-search nil)
         lspos
	 tags tags-list tags-alist (llast 0) rtn level category i txt p
	 marker entry priority (total 0))
    (save-excursion
      (org-clock-sum ts te)
      (goto-char 
	   (if (region-active-p)     ;; vc
		   (region-beginning)    ;; vc
		   (point-min)))
      (while (re-search-forward 
			  re 
			  (if (region-active-p)  ;; vc
				  (region-end)       ;; vc
				nil) t)
	(catch :skip
	  (setq tags (if (match-end 4) (match-string 4)))
	  (goto-char (setq lspos (1+ (match-beginning 0))))
	  (setq level (org-reduced-level (funcall outline-level))
		category (org-get-category))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the nex tags
	  (when tags
	    (setq tags (mapcar 'downcase (org-split-string tags ":"))
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr tags-alist))
		  tags))
	  (when (and (eval matcher)
		     (or (not org-agenda-skip-archived-trees)
			 (not (member org-archive-tag tags-list))))
	    ;; Get the time for the headline at point
	    (goto-char (line-beginning-position))
	    (setq total (+ total (or (get-text-property (1+ (point)) :org-clock-minutes) 0)))
	    ;; if we are to skip sublevels, jump to end of subtree
	    (org-end-of-subtree t)))))
    (if (interactive-p)
	(let* ((h (/ total 60))
	       (m (- total (* 60 h))))
	  (message "Time: %d:%02d (%d hours and %d minutes)" h m h m)))
    total))
