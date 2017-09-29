(defun cm/org-export-html-embed-style ()
    (make-local-variable 'org-export-html-style)
    (setq org-export-html-style "")
(let ((re "#\\+STYLE:\\s +<link\\s +rel=[\"']stylesheet[\"']\\s +type=[\"']text/css[\"']\\s +href=[\"']\\(.*\\)[\"']/>")
          (oldbuf (current-buffer))
          stylesheet
          css)
      (goto-char (point-min))
      (while (re-search-forward re (point-max) t)
        (setq stylesheet (match-string 1))
          (with-current-buffer (find-file stylesheet)
	    (setq css (format "\n<style>\n%s\n</style>\n"
			      (buffer-string)))) 
	  (setq org-export-html-style (concat org-export-html-style css)))
      (switch-to-buffer oldbuf)))

(org-add-hook 'org-export-first-hook 'cm/org-export-html-embed-style)
