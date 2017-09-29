;;; org-postamble.el --- Postamble customization for HTML export for Org-mode

;; Author:  Venkatesh Choppella <venkatesh dot choppella at iiit dot ac dot in>

;;; Commentary:
;;; This code customizes postamble in HTML export to reflect the last
;;; modification date of corresponding org source file.  The
;;; org source file is the file currently open in a buffer
;;; when org-export-as-html is invoked from the current
;;; buffer containing the org file.
;;

(defvar my-postamble-time-format  "%d %b %Y, %r (GMT%z)")


(defun my-postamble-last-modification-time (fn)
  "Return the last modification-time of file with filename FN as a string in standard time format"
  (format-time-string my-postamble-time-format
		      (nth 5  (file-attributes fn))))


(defun my-postamble-current-time ()
  "Return the last modification-time of file with filename FN as a string in standard time format"
  (format-time-string my-postamble-time-format))




(defun postamble-string-with-file-last-modification-time ()
 "Create postamble string with the last modification time of the org-source-file"
 (let* ((ts (my-postamble-last-modification-time my-postamble-org-source-file)))
   (concat "<hr/>\n"

	   "<p class=\"creator\"> Generated on "
	   "<span class=\"date\">"
	   (my-postamble-current-time)
	   "</span>"

           " using "
	   "<span class=\"create-version\">"
	   "Emacs Org-mode"
	   (org-version)
           "</span>"
           "</p>\n"

	   "<p class=\"postamble\"> Source file " 
	   "<span class=\"source-file-name\"> "
	   (file-relative-name 
	    my-postamble-org-source-file 
	    my-postamble-org-source-base-dir)
	   "</span>"
	   " last updated " 
	    "<span class=\"date\"> "
	    ts "</span>"
	    ". </p>\n")))

(defvar my-postamble-org-source-file "")
(defvar my-postamble-org-source-base-dir "~/")

(add-hook 'org-export-first-hook
	  (lambda ()
	    (setq my-postamble-org-source-file buffer-file-name)))

(provide 'my-postamble)
