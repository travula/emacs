;; this is a very temporary hack where the class files are
;; present in the right directories, but the java source files
;; are all in a single directory.  We want to place the java
;; source files in directories whose names are derived from the
;; package strucuture of the source files.



(defun mark-file (fname)
  (beginning-of-buffer)
  (let ((p (search-forward fname)))
    (dired-mark 1)))

(defun relocate-java-file ()
  "Relocates java files present from the other directory into the current buffer's directory.  The java files are identified as those corresponding to the class files marked in the current buffer." 
  (interactive)
  (dired-mark-extension ".class")
  (let* ((fns (dired-get-marked-files t))
	 (fns-sans-ext (mapcar 'file-name-sans-extension fns))
	 (fns-with-java-ext  (mapcar (function (lambda (x) (concat x ".java")))
				     fns-sans-ext)))
    (other-window 1)
    (mapcar (function (lambda (x) (mark-file x))) fns-with-java-ext)
    (dired-do-rename)))
  
  

    
    
    



	 

    
    
    
