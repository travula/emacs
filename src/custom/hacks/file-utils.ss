;; Usage: (replace-string-in-file "~/scratch/xxx" "xx" "bb")
(defun replace-string-in-file (file old-string new-string)
  "unconditionally replace OLD-STRING with NEW-STRING in FILE"
  (with-current-buffer  (find-file-noselect file)
    (replace-string old-string new-string)
    (save-buffer)))

