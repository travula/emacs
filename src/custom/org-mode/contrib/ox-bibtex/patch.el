;;; The main purpose of this file is to be able to
;;; accommodate multiple, comma separated, file
;;; specifications in a #+BIBLIOGRAPHY directive.  The
;;; advantage of the comma separated files is that it mimics
;;; LaTeX's way of considering multiple files in its
;;; \bibliography command.  Note that in LaTeX, only one
;;; style command is accepted and applies to all the
;;; bibliography files.  Unfortunately, ox-bibtex supports
;;; multiple #+BIBLIOGRAPHY directives, each with its own
;;; (and potentially distinct) bibliography style
;;; information.  LaTeX will not accept multiple
;;; \bibliographystyle commands.

;;; org-bibtex-get-files is a refinement of
;;; org-bibtex-get-file which allows a *list* of files to be
;;; specified as value of BIBLIOGRAPHY.

(defun org-bibtex-get-files (keyword)
  "Return bibliography files as a list of strings.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no file(s) is (are) found,
return nil instead."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
	 (let ((m (match-string 1 value)))
	   (org-split-string m ",")))))

;;; redefined to accommodate multiple bib files specified in
;;; a single #+BIBLIOGRAPHY directive.

(defun org-bibtex-process-bib-files (tree backend info)
  "Send each bibliography in parse tree to \"bibtex2html\" process.
Return new parse tree.  This function assumes current back-end is HTML."
  ;; Initialize dynamically scoped variables.  The first one
  ;; contain an alist between keyword objects and their HTML
  ;; translation.  The second one will contain an alist between
  ;; citation keys and names in the output (according to style).
  (setq org-bibtex-html-entries-alist nil
        org-bibtex-html-keywords-alist nil)
  (org-element-map tree 'keyword
    (lambda (keyword)
      (when (equal (org-element-property :key keyword) "BIBLIOGRAPHY")
        (let ((arguments (org-bibtex-get-arguments keyword))
              (files (org-bibtex-get-files keyword))
              temp-file)
          ;; limit is set: collect citations throughout the document
          ;; in TEMP-FILE and pass it to "bibtex2html" as "-citefile"
          ;; argument.
          (when (plist-get arguments :limit)
            (let ((citations
                   (org-element-map tree '(latex-fragment link)
                     (lambda (object)
                       (and (org-bibtex-citation-p object)
			    (org-bibtex-get-citation-key object))))))
              (with-temp-file (setq temp-file (make-temp-file "ox-bibtex"))
                (insert (mapconcat 'identity citations "\n")))
              (setq arguments
                    (plist-put arguments
                               :options
                               (append (plist-get arguments :options)
                                       (list "-citefile" temp-file))))))

          ;; Call "bibtex2html" on specified files
	  (mapcar (lambda (file)
		    (unless (eq 0 (apply 'call-process
                               (append '("bibtex2html" nil nil nil)
                                       '("-a" "-nodoc" "-noheader" "-nofooter")
                                       (list "--style"
                                             (org-bibtex-get-style keyword))
                                       (plist-get arguments :options)
                                       (list (concat file ".bib")))))
		      (error "Executing bibtex2html failed")))
		  files)
          (and temp-file (delete-file temp-file))
          ;; Open produced HTML file, wrap references within a block and
          ;; return it.
          (with-temp-buffer
            (insert "<div id=\"bibliography\">\n<h2>References</h2>\n")
	    (mapcar (lambda (file)
		      (insert-file-contents (concat file ".html")))
		    files)
            (insert "\n</div>")
            ;; Update `org-bibtex-html-keywords-alist'.
            (push (cons keyword (buffer-string))
                  org-bibtex-html-keywords-alist)
            ;; Update `org-bibtex-html-entries-alist'.
	    (populate-org-bibtex-html-entries-alist))))))
  ;; Return parse tree unchanged.
  tree)

(defun populate-org-bibtex-html-entries-alist ()
  (goto-char (point-min))
  (while (re-search-forward
	  "a name=\"\\([-_a-zA-Z0-9:]+\\)\">" nil t)
    (let* ((link (match-string 1))
	   (startpos (+ 2 (match-end 1))))
      (re-search-forward "\\(</a>\\)" nil t)
      (let* ((endpos (match-beginning 1))
	     (val (buffer-substring-no-properties startpos endpos)))
	(push (cons link val) org-bibtex-html-entries-alist)))))


    

(defun collect-html-entry ()
  (if (re-search-forward
	  "a name=\"\\([-_a-zA-Z0-9:]+\\)\">" nil t)
    (let* ((link (match-string 1))
	   (startpos (+ 2 (match-end 1))))
      (re-search-forward "\\(</a>\\)" nil t)
      (let* ((endpos (match-beginning 1))
	     (val (buffer-substring-no-properties startpos endpos)))
	(cons link val)))))

(defun collect-html-entries (file)
  (with-temp-buffer 
    (insert-file-contents (concat file ".html"))
    (let ((ans nil))
      (goto-char (point-min))
      (while (re-search-forward
	      "a name=\"\\([-_a-zA-Z0-9:]+\\)\">" nil t)
	(let* ((link (match-string 1))
	       (startpos (+ 2 (match-end 1))))
	  (re-search-forward "\\(</a>\\)" nil t)
	  (let* ((endpos (match-beginning 1))
		 (val (buffer-substring-no-properties startpos endpos)))
	    (setq ans (cons link val) ans))))
      ans)))






  ;; (goto-char (match-beginning 1))
  ;; (let ((a-xml (read-xml)))
  ;;   (let* ((a-tag (car a-xml))
  ;; 	   (link (cdr (cadr a-tag)))
  ;; 	   (val (cadr a-xml))
  ;; 	   (printval 
  ;; 	    (with-temp-buffer (insert-xml val)
  ;; 			      (buffer-string))))
  ;;     (cons link printval))))
      
	   
	   


;;  make-html-entry result: (Dinesh-2012-AFR-2207016-2207030 . ">DUS<sup>+</sup>12)
