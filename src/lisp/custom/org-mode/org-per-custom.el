;;; http://doc.norang.ca/org-mode.html#AgendaSetup
;;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php

;;(require 'org-element)
; (load "norang")

;; Remove empty LOGBOOK drawers on clock out
;; fixed bug in  norang.el
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at (point))))

;;; suppress norang css 
(setq org-html-head-extra nil)

;;; Norang unsets C-c;
;; Restore C-c; as org-toggle-comment
(define-key org-mode-map "\C-c;" 'org-toggle-comment)


;;; Norang sets org-id-method to 'uuidgen)
;; Restore to  org
(setq org-id-method (quote org))
;;; ================================#
;;; Org files structure for GTD.    #
;;; ================================#

;; (setq org-mode-user-lisp-path "~/")
;; (setq org-mode-user-contrib-lisp-path "~/")
(setq org-dir "~/top/org/")
;; (setq per-org-dir "~/top/per/org/")
(setq journal-dir "~/top/res/jnl/venkatesh/")

;;; org-directory: org parameter that contains name of default
;;; directory containing org files
(setq org-directory org-dir)
(setq org-agenda-dir (concat org-dir "agenda/"))
(setq org-user-agenda-files
	  (list
	   org-agenda-dir
	   journal-dir
;;	   per-dir
	   ))

(setq refile-file (concat org-agenda-dir "refile.org"))
(setq org-default-notes-file refile-file)
(setq journal-file (concat journal-dir "2017-journal.org"))

;;;    refile.org : location where captured tasks sit.
;;;    vlead.org: vlead related
;;;    iiit.org:  students, teaching, research, admin
;;;    sys.org:  sys and coding related

;;;    misc.org: Lending, borrowing, things to buy, etc.
;;;    home.org:  home, errands, etc.
;;;    fin.org : finance related
;;;    per.org:   other personal things



;;; agenda
;;; -------
(setq per-files 
	  (if (file-exists-p "~/per")
		  (split-string 
		   (shell-command-to-string 
			"find ~/per/people/venkatesh/finances -type d")
		   "\\\n")
		nil))

(setq org-agenda-files
      (append 
	   (list
		"~/org/refile.org"
		"~/org/agenda.org"
		"~/org/buy.org"
		"~/org/home.org"
		"~/org/fin.org" 
		"~/org/misc.org"
		"~/org/sys.org"
		"~/org/work.org"
		)
	   per-files))



(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "sienna" :weight normal)
              ("NEXT" :foreground "blue" :weight normal)
              ("DONE" :foreground "forest green" :weight normal)
              ("WAITING" :foreground "orange" :weight normal)
              ("HOLD" :foreground "magenta" :weight normal)
              ("CANCELLED" :foreground "forest green" :weight normal)
              ("MEETING" :foreground "forest green" :weight normal)
              ("PHONE" :foreground "forest green" :weight normal))))


;;; org-capture 
;;; ===========


;;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file refile-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

              ;; ("r" "respond" entry (file refile-file)
              ;;  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)

	      ("r" "respond" entry (file refile-file)
	       "* TODO Reply to %:fromname :email:\n%:fromaddress  Subject: %c\n%U\n"
	       :empty-lines 1)

	      ("c" "call" entry (file refile-file)
	       "* TODO call %? :ph:\n%U\n"
	       :empty-lines 1)

	      ("u" "urgent" entry (file refile-file)
	       "* TODO %? :urgent:\n%U\n%a\n"
	       :empty-lines 1)

              ("n" "note" entry (file refile-file)
               "* %? :NOTE:\n%U\n%a\n")

              ("j" "Journal" entry (file+datetree journal-file)
               "* %?\n%U\n" :clock-in t :clock-resume t)

              ("w" "org-protocol" entry (file refile-file)
               "* TODO Review %c\n%U\n")

              ("m" "Meeting" entry (file refile-file)
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)

              ;; ("p" "Phone call" entry (file refile-file)
              ;;  "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file refile-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(define-key global-map "\C-ck" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;;; org-protocol 
;;; ============
(require 'org-protocol)
(setq org-protocol-protocol-alist org-protocol-protocol-alist-default)
(add-to-list 'org-protocol-protocol-alist 
	     '("org-gmail" 
	       :protocol "gmail" 
	       :function org-protocol-capture-gmail 
	       :kill-client t))


;;;javascript:location.href='org-protocol://gmail://'+encodeURIComponent(location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(document.getElementsByClassName("gD")[document.getElementsByClassName("gD").length - 1].getAttribute("email"))+'/'+encodeURIComponent(document.getElementsByClassName("gD")[document.getElementsByClassName("gD").length - 1].getAttribute("name"))+'/'+encodeURIComponent(window.getSelection())

(defun org-protocol-capture-gmail (info)
  "Process an org-protocol://gmail:// style url

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function detects an URL, Title, Sender's name and email and
an optional selected region of text, all separated by '/'.  The
location for a browser's bookmark looks like this:

  javascript:location.href='org-protocol://gmail://'+\\
        encodeURIComponent(location.href)+'/'+\\
        encodeURIComponent(document.title)+'/'+\\
        encodeURIComponent(document.getElementsByClassName(\"gD\")[document.getElementsByClassName(\"gD\").length - 1].getAttribute(\"email\"))+'/'+\\
        encodeURIComponent(document.getElementsByClassName(\"gD\")[document.getElementsByClassName(\"gD\").length - 1].getAttribute(\"name\"))+'/'+\\
      encodeURIComponent(window.getSelection())
"

  (if (and (boundp 'org-stored-links)
	   (org-protocol-do-capture-gmail info))
      (message "Item captured."))
  nil)

(defun org-protocol-do-capture-gmail (info)
    "Support `org-capture-gmail'."
  (let* ((parts (org-protocol-split-data info t org-protocol-data-separator))
	 (template (or (and (>= 2 (length (car parts))) (pop parts))
		       org-protocol-default-template-key))
	 (url (org-protocol-sanitize-uri (car parts))) ;; 0th
	 (type (if (string-match "^\\([a-z]+\\):" url)
		   (match-string 1 url)))
	 (title (or (cadr parts) ""))
	 (orglink (org-make-link-string
		   url (if (string-match "[^[:space:]]" title) title url)))
	 (from-email (or (caddr parts) ""))
	 (from-name (or (cadddr parts) ""))
	 (region (or (nth 4 parts) ""))
	 (query (or (org-protocol-convert-query-to-plist (nth 5 parts)) ""))
	 (org-capture-link-is-already-stored t)) ;; avoid call to org-store-link
    (setq org-stored-links
	  (cons (list url title from-email from-name region) org-stored-links))
    (kill-new orglink)
    (org-store-link-props :type type
			  :link url
			  :description title
			  :annotation orglink
			  :initial region
			  :fromname from-name
			  :fromaddress from-email
			  :query query)
    (raise-frame)
    (funcall 'org-capture nil template)))
;; (load "my-capture")
