;;; Using org with BibTeX
;;; See
;;; http://www-public.it-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/


;; (require 'org-exp-bibtex)

(defun my-rtcite-export-handler (path desc format)
  (message 
   "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" 
   path desc format)
  (let ((ans 
	 (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
			  (match-string 1 path)))
		(path (substring path 0 (match-beginning 0))))
	   (cond ((eq format 'latex)
		  (if (or (not desc)
			  (equal 0 (search "rtcite:" desc)))
		      (format "\\cite{%s}" search)
		    (format "\\cite[%s]{%s}" desc search)))
		 ((eq format 'html)
		  (format "<a href=\"#%s\">%s</a>" path desc))
		 (t nil)))))
    (message "my-rtcite-export-handler returning %s" ans)
    ans))


(require 'org)

(org-add-link-type "rtcite" 
                   'org-bibtex-open
                   'my-rtcite-export-handler)


;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (reftex-parse-all))
;;   (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
;;   )

;;; this is from http://bowenli37.wordpress.com/tag/org-exp-bibtex/
(defun org-mode-reftex-setup ()
  (print (format "Running org-mode-reftex-setup on buffer %s" buffer-file-name))
  (load-library "reftex")
  (reftex-mode)
  (and (buffer-file-name) 
       (file-exists-p (buffer-file-name))
       (progn
	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?t . "%t")
	    (?h . 
		"** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))

	 (print (format "Finished org-mode-reftex-setup on buffer %s" buffer-file-name)))))

(define-key org-mode-map (kbd "C-c r") 'reftex-citation)
(define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search)

;;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)


;;; see http://almost-published.com/?tag=org-mode


;;; As a good starting point, Mario shares some nice ideas on how to
;;; integrate RefTeX and org-mode in order to create a scientific
;;; word processor. One comment to that post points out that loading
;;; RefTeX in every org buffer creates some computational overhead
;;; and overwriting org-specific keybinding by RefTeX might be
;;; problematic in some cases (it was for me, too). A nice solution
;;; is to load RefTeX only in org buffers with certain TODO
;;; keywords. I chose "WRITE", and have the following lines in my
;;; .emacs to load RefTeX in those buffers:

;;; use reftex and other article helper modes in org-mode
;;; (but only in files with a WRITE todo keyword)

;;; adapted from the above.
(add-hook 'org-mode-hook
          (lambda ()
            (if (member "WRITE" org-todo-keywords-1)
                (org-mode-reftex-setup))))

;;; It comes in very handy to set a global bibliography for
;;; RefTeX, which can be accessed for all org files with the
;;; WRITE keyword (and from all TeX files also):

;;; master bibliography
;;; (setq reftex-default-bibliography
;;;      '("/Users/christian/Documents/Bibliothek.bib"))

;;; Typically, my org files related to paper writing have a
;;; file-specific TODO sequence:

;;; #+TODO: WRITE REVISE SHORTEN | DONE
;;; #+DRAWERS: PROPERTIES SYNOPSIS COMMENTS LOGBOOK

;;; The second line corresponds to the property drawers I set for the
;;; document. Clearly, this setup is borrowed from Scrivener, a
;;; popular Mac OSX writing app, where each snippet of text can have
;;; a synopsis and comments. The logbook drawer is set because I log
;;; changes in TODO states in this special drawer.

;;; This way, a nice writing environment is established, where I can
;;; profit from org's power, which is absolutely incredible and
;;; ranges from todo-list management to the evaluation of embedded
;;; code in different languages, and the power of RefTeX with all
;;; it's niceties like mode-line citation overview and regex citation
;;; search.



;;; This allows Section #'s to appear when
;;; cross-referencing.
;;; See
;;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
;;; Section 16 Cross References
(setq org-export-latex-hyperref-format "\\ref{%s}")

(provide 'my-org-bib)
