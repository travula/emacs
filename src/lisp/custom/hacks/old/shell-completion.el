;;; Functions to provide filename completion in the shell.
;;;

;; import

;;file-name-beginning-delimiter-regexp

;;; Do shell completion.

(require 'shell)
(defun shell-completion ()
  "\
  Provide filename completion for a c-shell command."
  (interactive)
  (catch 'ordinary-tab
    (progn
  (save-excursion
    (let* ((enable-recursive-minibuffers t)
           (end-of-filename (point))
           (bol (save-excursion
                  (beginning-of-line) (point)))
           (beginning-of-filename
	      (if (not (re-search-backward
			file-name-beginning-delimiter-regexp bol t))
		  (throw 'ordinary-tab nil)
		(progn 
		  (forward-char 1)
		  (point))))
           ;; Grab the last argument typed.
           (initial-string (buffer-substring beginning-of-filename
                                             end-of-filename))
           ;; Do completion.
           (pathname
              (read-file-name
                 "Filename: "
                 (expand-file-name
                    (concat (if (file-name-directory initial-string)
                                (expand-file-name
                                   (file-name-directory initial-string))
                                default-directory)
                            (file-name-completion
                               (file-name-nondirectory initial-string)
                               (if (file-name-directory initial-string)
                                   (expand-file-name
                                      (file-name-directory
                                         initial-string))
                                   default-directory))))
                 nil nil))
           ;; Get absolute pathname for testing against default directory.
           (abs-pathname (expand-file-name pathname))
           ;; Put default-directory value into dummy for manipulation.
           (test-dir default-directory))
          ;; Insert results of completion.
          (kill-region beginning-of-filename end-of-filename)
          (insert-string
             (cond ((string-equal test-dir abs-pathname) "")
                   ((and (< (length test-dir) (length abs-pathname))
                         (string-equal test-dir
                                       (substring abs-pathname
                                                  0
                                                  (length test-dir))))
                    (substring abs-pathname (length test-dir)))
                   ((and (<= (length (setq test-dir
                                       (file-name-directory
                                          (substring test-dir 0 -1))))
                            (length abs-pathname))
                         (string-equal test-dir
                                       (substring
                                          abs-pathname
                                          0
                                          (length test-dir))))
                    (concat "../" (substring abs-pathname (length test-dir))))
                   (t pathname)))))
    (end-of-line))))


(defun complete-or-default (default)
  (list 'lambda '()
	(list 'if '(not (shell-completion))
	      (list default))))
	      


(define-key shell-mode-map "\t" 'shell-completion)

(provide 'shell-completion)



