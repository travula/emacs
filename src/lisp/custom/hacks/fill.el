(defun fill-fix-prefix (the-fill-string)
  (interactive "sFill string? ")
  (let* ((par-begin (progn (backward-paragraph 1) (point)))
         (par-end (progn (forward-paragraph 1) (point))))

    ;;; Get ugly, since replace-regexp won't let you bound by point...

    (goto-char par-begin)
    (while (re-search-forward (concat "^" the-fill-string) par-end t)
      (setq par-end (- par-end 2))
      (kill-region (point) (- (point) (length the-fill-string))))

    ;;; Now that we've killed all the previous fill prefixes, fill the par.
    ;;; subtract the length of the-fill-string from fill-column, so we don't
    ;;; go past the end of a line again.

    (setq fill-column (- 78 (length the-fill-string)))
    (fill-region-as-paragraph par-begin par-end nil)

    ;;; Have to reset the paragraph-end after fill

    (goto-char par-begin)
    (setq par-end (progn (forward-paragraph 1) (point)))
    (goto-char par-begin)

    ;;; Now replace the fill prefixes 

    (while (re-search-forward "^" par-end t)
      (insert the-fill-string))
    (next-line 1)
    (beginning-of-line)
    (insert the-fill-string)))
