;; Additional dired operations
;; Author:  Venkatesh Choppella  choppell@iiitmk.ac.in
;; Date:   Jan 2004
;; Updated: Apr 2017

(require 'pathname)


(defun dired-trash-files ()
  "trashes selected files"
  (interactive)
  (let ((fns (dired-get-marked-files t)))
    (dired-do-shell-command "/home/choppell/bin/trash" nil fns)
    ))


(defun dired-acroread-file ()
  "Read pdf file at line using acrobat reader"
  (interactive)
  (let ((file-name (dired-get-filename)))
    (cd-buffer-dir-and-run-shell-command 
     (concat "ac " file-name " &"))))

