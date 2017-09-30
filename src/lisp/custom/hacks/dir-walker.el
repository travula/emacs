;; will break if directory structure is cyclic!!!
;; e.g., if a directory is a  symbolic link of one of its ancestors.

(defun directory-walker (file proc)
  (apply proc (list file))
  (if (file-directory-p file)
      ;; the first elements in the directory listing are "." and ".."
      (dolist (f  (cddr (directory-files file 'full-names)))
        (directory-walker f proc)))) 


;; (defvar ans '())

;; (directory-walker
;;    "/home/choppell/iiith/projects/virtual-labs/documents/2010-07-07-expert-meeting/report/all-slides" 
;;   (lambda (x)
;;     (setq ans (cons x ans))))

    




