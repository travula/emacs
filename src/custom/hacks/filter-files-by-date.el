(defun filter-files-by-date
	 (source-pattern dest-ext &optional source-dir dest-dir)
	"Filters a set of files in the current directory that match
	 source-pattern, checks if source-file is newer than the 
	 corresponding  destination file.  A destination file is obtained
	 by concatenating the (optional) dest-dir, source-file sans extension,  
	 and the dest-extension string. dest-dir must have a trailing slash."

  (let* ((source-dir   (if (null source-dir) "." source-dir))
	 (dest-dir     (if (null dest-dir) "" dest-dir))
	 (source-files (directory-files 
		        source-dir
			t  ;; t means full path name
			source-pattern))
	 (ans ""))
    (dolist (source-file source-files ans)
      (let* ((sans-ext    (file-name-sans-extension source-file))
	     (class-file  (concat dest-dir sans-ext dest-ext)))
	(if (file-newer-than-file-p source-file class-file)
	    (setq ans (concat (file-name-nondirectory source-file) 
			      " " ans)))))))

	  
;;; invoke this on the command line as
;;; EMACSLOADPATH should be set
;;; emacs --batch -l filter-files-by-date -eval 
;;;      "(filter-files-by-date pattern "*.class" "." "extreme/../")














