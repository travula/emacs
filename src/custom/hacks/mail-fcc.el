
(define-key mail-mode-map "\C-c\C-f\C-f" 'mail-fcc)

(defun mail-fcc ()
  "Move point to end of CC-field.  Create a FCC field if none."
  (interactive)
  (expand-abbrev)
  (or (mail-position-on-field "fcc" t)
      (progn (mail-position-on-field "to")
	     (insert "\nFCC: "))))




