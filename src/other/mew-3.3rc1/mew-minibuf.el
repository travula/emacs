;;; mew-minibuf.el --- Minibuffer input methods for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion switch
;;;

(defvar mew-input-complete-function nil
  "A function to be called when TAB is typed in minibuffer.
This is used in 'mew-input-complete'.")

(defun mew-input-complete ()
  "Do completion according to the global variable
\"mew-input-complete-function\"."
  (interactive)
  (if (and mew-input-complete-function (fboundp mew-input-complete-function))
      (funcall mew-input-complete-function)))

(defvar mew-input-exit-minibuffer-function nil
  "A function to be called when RET is typed in minibuffer.
This function are used to check validity of 'case' and sort key.")

(defun mew-input-exit-minibuffer ()
  "Ensure the input meets a condition."
  (interactive)
  (if (or (not (and mew-input-exit-minibuffer-function
                    (fboundp mew-input-exit-minibuffer-function)))
          (funcall mew-input-exit-minibuffer-function))
      (exit-minibuffer)))
 
(defvar mew-input-comma-function nil
  "A function to be called when ',' is typed in minibuffer.
This function can be used to check validity of 'case'.")

(defun mew-input-comma ()
  "Ensure the input meets a condition."
  (interactive)
  (when (or (not (and mew-input-comma-function
		      (fboundp mew-input-comma-function)))
	    (funcall mew-input-comma-function))
    (insert ",")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clean up side effects
;;;

(defun mew-input-clear ()
  "A function to clean up side effects of window configuration
at completions."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    ;; (mew-ainfo-get-win-cfg) is shared by many functions
    ;; because minibuffer is just one!
    (mew-ainfo-set-win-cfg nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defvar mew-input-folder-search-buf " *Mew* folder search")

(defvar mew-input-folder-hist nil)

(defvar mew-input-folder-circular-case nil)
(defvar mew-input-folder-refile nil)

(defvar mew-input-folder-search-direction nil)
(defvar mew-input-folder-search-key nil)
(defvar mew-input-folder-search-match nil)
(defvar mew-input-folder-search-original nil)
(defvar mew-input-folder-search-point nil)
(defvar mew-input-folder-search-multi nil)

(defun mew-input-folder-clean-up ()
  (setq mew-input-folder-search-direction nil)
  (setq mew-input-folder-search-key nil)
  (setq mew-input-folder-search-match nil)
  (setq mew-input-folder-search-original nil)
  (setq mew-input-folder-search-point nil))

(defun mew-highlight-folder-comp-search-window ()
  (let* ((win (get-buffer-window mew-buffer-completions))
	 (match mew-input-folder-search-match)
	 face)
    (when (and win
	       mew-input-folder-search-key
	       mew-input-folder-search-match)
      (save-excursion
	(set-buffer mew-buffer-completions)
	(mew-elet
	 (goto-char (point-min))
	 (when (looking-at "^Key: ")
	   (delete-region (point-min) (progn (forward-line) (point))))
	 (insert "Key: " mew-input-folder-search-key ",  "
		 "Match: " mew-input-folder-search-match "\n")
	 (put-text-property (point-min) (point-max) 'face nil)
	 (when (re-search-forward
		(format "\\(%s\\)\\([ \t]\\|$\\)" (regexp-quote match)) nil t)
	   (goto-char (match-beginning 1))
	   (setq face (if (not window-system)
			  'mew-face-header-from
			(if (or (and mew-xemacs-p (find-face 'isearch))
				(and (not mew-xemacs-p) (facep 'isearch)))
			    'isearch
			  'region)))
	   (put-text-property (point) (match-end 1) 'face face)
	   (unless (pos-visible-in-window-p (point) win)
	     (set-window-start win (progn (forward-line -2) (point))))))))))

(defun mew-input-folder-display (&optional msg)
  (mew-highlight-folder-comp-search-window)
  (mew-elet
   (delete-region (mew-minibuf-point-min) (point-max))
   (insert "(" (or mew-input-folder-search-match "") ") ")
   (insert (or mew-input-folder-search-key ""))
   (if msg (save-excursion (insert " [" msg "]")))
   (put-text-property (mew-minibuf-point-min) (point-max) 'read-only t)))

(defun mew-input-folder-search-setup (&optional min)
  (if mew-input-folder-refile
      (mew-input-folder-search-setup2 min)
    (mew-input-folder-search-setup1 min)))

(defun mew-input-folder-search-setup-buffer (alist min)
  (save-excursion
    (set-buffer (get-buffer-create mew-input-folder-search-buf))
    (setq buffer-read-only t)
    (mew-elet
     (mew-erase-buffer)
     (mapcar (lambda (x) (if (stringp (car x)) (insert (car x) "\n"))) alist))
    (if min (goto-char (point-min)))))

(defun mew-input-folder-search-setup1 (&optional min)
  (let (case:fld case fld alist)
    (save-excursion
      (goto-char (point-max))
      (if (search-backward "," nil t)
	  (setq mew-input-folder-search-point
		(- (match-end 0) (mew-minibuf-point-min)))))
    (setq mew-input-folder-search-original
	  (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
    (setq case:fld mew-input-folder-search-original)
    (if mew-input-folder-search-point
	(setq case:fld (substring case:fld mew-input-folder-search-point)))
    (setq case (car (mew-folder-case case:fld)))
    (if case
	(setq mew-input-folder-search-point
	      (+ (or mew-input-folder-search-point 0) (length case) 1))) ;; ":"
    (setq fld (mew-folder-folder case:fld))
    (mew-input-folder-display)
    (if (or (null fld) (string= fld ""))
	(setq fld (mew-proto case)))
    (cond
     ((mew-folder-popp fld)
      (setq alist (mew-pop-folder-alist)))
     ((mew-folder-nntpp fld)
      (setq alist (mew-nntp-folder-alist case)))
     ((mew-folder-imapp fld)
      (setq alist (mew-imap-folder-alist case)))
     (t
      (setq alist (mew-local-folder-alist))))
    (mew-input-folder-search-setup-buffer alist min)))

(defun mew-input-folder-search-setup2 (&optional min)
  (let (do-search alist)
    (save-excursion
      (goto-char (point-max))
      (if (search-backward "," nil t)
	  (progn
	    (goto-char (match-end 0))
	    (unless (looking-at mew-regex-case2)
	      (setq do-search t)
	      (setq mew-input-folder-search-point
		    (- (point) (mew-minibuf-point-min)))))
	(goto-char (mew-minibuf-point-min))
	(unless (looking-at mew-regex-case2)
	  (setq do-search t))))
    (if (not do-search)
	(progn
	  (mew-temp-minibuffer-message "Remove case!")
	  (mew-input-folder-clean-up))
      (setq mew-input-folder-search-original
	    (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
      (mew-input-folder-display)
      (cond
       ((eq mew-input-complete-function 'mew-complete-local-folder)
	(setq alist (mew-local-folder-alist)))
       ((eq mew-input-complete-function 'mew-complete-imap-folder)
	(setq alist (mew-imap-folder-alist mew-inherit-case))))
      (mew-input-folder-search-setup-buffer alist min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder search forward
;;;

(defun mew-input-folder-search-forward-1 (&optional again)
  (let (no-match pos)
    (save-excursion
      (set-buffer mew-input-folder-search-buf)
      (setq pos (point))
      (if again
	  (forward-line)
	(beginning-of-line))
      (if (search-forward mew-input-folder-search-key nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (setq mew-input-folder-search-match
		  (mew-buffer-substring
		   (save-excursion (beginning-of-line) (point))
		   (save-excursion (end-of-line) (point)))))
	(setq no-match t)
	(goto-char pos)))
    (if no-match 
	(mew-input-folder-display "no match")
      (mew-input-folder-display))))

(defun mew-input-folder-search-forward ()
  "Search a folder forward."
  (interactive)
  (cond
   ((and mew-input-folder-search-direction (null mew-input-folder-search-key))
    (mew-input-folder-display "no match"))
   ((eq mew-input-folder-search-direction 'forward)
    (mew-input-folder-search-forward-1 'again))
   ((eq mew-input-folder-search-direction 'backward)
    (setq mew-input-folder-search-direction 'forward)
    (mew-input-folder-search-forward-1 'again))
   (t
    (setq mew-input-folder-search-direction 'forward)
    (mew-input-folder-search-setup 'min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder search backward
;;;

(defun mew-input-folder-search-backward-1 (&optional again)
  (let (no-match pos)
    (save-excursion
      (set-buffer mew-input-folder-search-buf)
      (setq pos (point))
      (if again
	  (progn (forward-line -1) (end-of-line))
	(end-of-line))
      (if (search-backward mew-input-folder-search-key nil t)
	  (progn
	    (goto-char (match-end 0))
	    (setq mew-input-folder-search-match
		  (mew-buffer-substring
		   (save-excursion (beginning-of-line) (point))
		   (save-excursion (end-of-line) (point)))))
	(setq no-match t)
	(goto-char pos)))
    (if no-match 
	(mew-input-folder-display "no match")
      (mew-input-folder-display))))

(defun mew-input-folder-search-backward ()
  "Search a folder backward."
  (interactive)
  (cond
   ((and mew-input-folder-search-direction (null mew-input-folder-search-key))
    (mew-input-folder-display "no match"))
   ((eq mew-input-folder-search-direction 'forward)
    (setq mew-input-folder-search-direction 'backward)
    (mew-input-folder-search-backward-1 'again))
   ((eq mew-input-folder-search-direction 'backward)
    (mew-input-folder-search-backward-1 'again))
   (t
    (setq mew-input-folder-search-direction 'backward)
    (mew-input-folder-search-setup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder commands
;;;

(defun mew-input-folder-exit-minibuffer ()
  (interactive)
  (if mew-input-folder-search-direction
      (mew-elet
       (delete-region (mew-minibuf-point-min) (point-max))
       (cond
	(mew-input-folder-search-match
	 (when mew-input-folder-search-point
	   (insert (substring mew-input-folder-search-original
			      0 mew-input-folder-search-point)))
	 (insert mew-input-folder-search-match))
	(mew-input-folder-search-original
	 (insert mew-input-folder-search-original)))
       (mew-input-folder-clean-up)
       (mew-complete-window-delete))
    (exit-minibuffer)))

(defun mew-input-folder-abort-minibuffer ()
  "This function exits folder search mode if in folder search mode.
Otherwise, it exits minibuffer."
  (interactive)
  (if mew-input-folder-search-direction
      (mew-elet
       (delete-region (mew-minibuf-point-min) (point-max))
       (when mew-input-folder-search-original
	 (insert mew-input-folder-search-original))
       (mew-input-folder-clean-up)
       (mew-complete-window-delete))
    (abort-recursive-edit)))

(defun mew-input-folder-comma ()
  "This function inserts ',' in a single folder input
and does not insert anything in a multiple folder input."
  (interactive)
  (if (and (null mew-input-folder-search-direction)
	   mew-input-folder-search-multi)
      (insert ",")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Self insert
;;;

(defun mew-input-folder-self-insert ()
  "This function normally inserts its bound key to minibuffer.
When in folder search mode, this function searches a candidate
folder and displays it in addition to its bound key."
  (interactive)
  (let* ((key (if mew-xemacs-p
		  (event-key last-command-event)
		(this-command-keys)))
	 last-str gfunc)
    (cond
     ((mew-characterp key) ;; XEmacs
      (setq key (char-to-string key))
      (setq last-str key)
      (setq gfunc (lookup-key (current-global-map) key)))
     ((stringp key)
      (setq last-str key)
      (setq gfunc (lookup-key (current-global-map) key)))
     ((symbolp key) ;; XEmacs
      (setq gfunc (lookup-key (current-global-map) key)))
     ((vectorp key)
      (setq gfunc (lookup-key (current-global-map) key))
      (unless gfunc
	(setq key (lookup-key function-key-map key))
	(cond
	 ((vectorp key) ;; normal Emacs
	  (setq gfunc (lookup-key (current-global-map) key)))
	 ((stringp key) ;; Meadow
	  (setq last-str key)
	  (setq gfunc (lookup-key (current-global-map) key)))))))
    (if mew-input-folder-search-direction
	(cond
	 ((or (equal key "\177") (equal key [127])
	      (equal key 'delete) (equal key 'backspace))
	  (if (null mew-input-folder-search-key)
	      (mew-input-folder-display "not allowed")
	    (setq mew-input-folder-search-key
		  (substring mew-input-folder-search-key 0 -1))
	    (when (string= mew-input-folder-search-key "")
	      (setq mew-input-folder-search-key nil)
	      (setq mew-input-folder-search-match nil)
	      (save-excursion
		(set-buffer mew-input-folder-search-buf)
		(cond
		 ((eq mew-input-folder-search-direction 'forward)
		  (goto-char (point-min)))
		 ((eq mew-input-folder-search-direction 'backward)
		  (goto-char (point-max))))))
	    (mew-input-folder-display)))
	 ((not (string-match "self-insert-command" (symbol-name gfunc)))
	  (mew-input-folder-display "not allowed"))
	 ((eq mew-input-folder-search-direction 'forward)
	  (setq mew-input-folder-search-key
		(concat mew-input-folder-search-key last-str))
	  (mew-input-folder-search-forward-1))
	 ((eq mew-input-folder-search-direction 'backward)
	  (setq mew-input-folder-search-key
		(concat mew-input-folder-search-key last-str))
	  (mew-input-folder-search-backward-1)))
      (cond
       ((null gfunc)
	())
       ((string-match "self-insert-command" (symbol-name gfunc))
	(insert last-command-char))
       ((and (fboundp gfunc) (commandp gfunc))
	(call-interactively gfunc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Prefix hack
;;;

(defun mew-input-folder-complete-case-input (prefix-char)
  (when (and mew-use-case-input-completion
	     (eq mew-input-complete-function 'mew-complete-folder)
	     (mew-folder-remotep (char-to-string prefix-char))
	     (and mew-case-input
		  (not (string= mew-case-input mew-case-default))))
    (insert mew-case-input ":")))

(defun mew-input-folder-prefix ()
  "A function to insert a folder prefix in minibuffer.
If the previous character is another folder prefix, 
it is deleted automatically."
  (interactive)
  (cond
   (mew-input-folder-search-direction
    (mew-input-folder-self-insert))
   ;;
   ((or (eq mew-input-complete-function 'mew-complete-local-folder)
	(eq mew-input-complete-function 'mew-complete-imap-folder))
    ;; no case, not allow other prefixes.
    (let* ((pos (- (point) (mew-minibuf-point-min)))
	   (prefix (if (eq mew-input-complete-function
			   'mew-complete-local-folder)
		       mew-folder-local mew-folder-imap))
	   (insert-ok (string= (char-to-string last-command-char) prefix)))
      (cond
       ((or (= pos 0)
	    (save-excursion (forward-char -1) (looking-at ",")))
	(if insert-ok (insert prefix)))
       ((or (and (= pos 1)
		 (save-excursion (forward-char -1) (looking-at "[-+%$]"))) 
	    ;; excluding *
	    (save-excursion (forward-char -2) (looking-at ",[-+%$]")))
	;; A wrong prefix might accidentally be here. 
	;; So, replace it just in case
	(when insert-ok
	  (forward-char -1)
	  (delete-char 1)
	  (insert prefix)))
       (t
	(insert last-command-char)))))
   (t
    (let ((pos (- (point) (mew-minibuf-point-min))))
      (cond
       ((or (= pos 0)
	    (save-excursion (forward-char -1) (looking-at ",")))
	(mew-input-folder-complete-case-input last-command-char)
	(insert last-command-char))
       ;;
       ((or (and (= pos 1)
		 (save-excursion (forward-char -1) (looking-at "[-+%$]"))) 
	    ;; excluding *
	    (save-excursion (forward-char -2) (looking-at ",[-+%$]")))
	(forward-char -1)
	(delete-char 1)
	(mew-input-folder-complete-case-input last-command-char)
	(insert last-command-char))
       ;;
       ((save-excursion
	  (forward-char -2)
	  (looking-at ":[-+*%$]"))
	(forward-char -1)
	(delete-char 1)
	(when (memq last-command-char '(?+ ?*))
	  ;; delete case:
	  (delete-region (save-excursion (or (and (search-backward "," nil t)
						  (match-end 0))
					     (mew-minibuf-point-min)))
			 (point)))
	(insert last-command-char))
       ;;
       (t
	(insert last-command-char)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defun mew-input-folder-check (folders)
  (let (case:folder folder ret)
    (while folders
      (setq case:folder (car folders))
      (setq folders (cdr folders))
      (catch 'continue
	(if (string-match mew-regex-case case:folder)
	    (progn
	      (setq folder (mew-match-string 2 case:folder))
	      (if (= (length folder) 0) (throw 'continue nil))) ;; "case:" only
	  (setq folder case:folder))
	(if (and (= (length folder) 1)
		 (member folder mew-folder-prefixes)) ;; "prefix" only
	    (throw 'continue nil))
	(if (and (mew-folder-popp folder) ;; not $inbox
		 (not (string= mew-pop-inbox-folder folder)))
	    (throw 'continue nil))
	(setq case:folder (mew-canonicalize-case-folder case:folder))
	(setq case:folder (directory-file-name case:folder))
	(setq ret (cons case:folder ret))))
    (nreverse ret)))

(defun mew-input-local-folder (folder)
  "Input a local folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((default folder)
	 (init (mew-folder-prefix folder))
	 (mew-input-complete-function 'mew-complete-local-folder)
	 (mew-input-folder-circular-case t)
	 (ret (read-from-minibuffer (format "Folder name (%s): " default)
				    init mew-input-folder-map nil
				    'mew-input-folder-hist)))
    (when (or (string= ret "") (string= ret init))
      (setq ret default))
    (car (mew-input-folder-check (list ret)))))

(defun mew-input-folder (case folder)
  "Input a folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((default (mew-folder-case-folder case folder))
	 (init (mew-folder-case-folder case (mew-folder-prefix folder)))
	 (mew-input-complete-function 'mew-complete-folder)
	 (mew-input-folder-circular-case t)
	 ;; mew-inherit-case must be nil
	 (ret (read-from-minibuffer (format "Folder name (%s): " default)
				    init mew-input-folder-map nil
				    'mew-input-folder-hist)))
    (when (or (string= ret "") (string= ret init))
      (setq ret default))
    (car (mew-input-folder-check (list ret)))))

(defun mew-input-folders (case:folder)
  "Input a folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((init (mew-canonicalize-case-folder case:folder))
	 (mew-input-complete-function 'mew-complete-folder)
	 (mew-input-folder-circular-case t)
	 (mew-input-folder-search-multi t)
	 ;; mew-inherit-case must be nil
	 (ret (read-from-minibuffer "Folder name: "
				    init mew-input-folder-map nil
				    'mew-input-folder-hist)))
    (when (string= ret "")
      (setq ret init))
    (setq ret (mapcar 'mew-chop (mew-split ret ?,)))
    (mew-input-folder-check ret)))

(defun mew-input-refile-folder-check (folders proto)
  (let (case:folder folder ret)
    (while folders
      (setq case:folder (car folders))
      (setq folders (cdr folders))
      (catch 'continue
	;; If case is specified, just remove it at this moment...
	(if (string-match mew-regex-case case:folder)
	    (progn
	      (setq folder (mew-match-string 2 case:folder))
	      (if (= (length folder) 0) (throw 'continue nil))) ;; "case:" only
	  (setq folder case:folder))
	(cond
	 ((eq proto 'imap)
	  (unless (mew-folder-imapp folder)
	    (throw 'continue nil)))
	 ((eq proto 'local)
	  (unless (mew-folder-localp folder)
	    (throw 'continue nil))))
	(if (and (= (length folder) 1)
		 (member folder mew-folder-prefixes)) ;; "prefix" only
	    (throw 'continue nil))
	(setq folder (mew-canonicalize-case-folder folder))
	(setq folder (directory-file-name folder))
	(setq ret (cons folder ret))))
    (nreverse ret)))

(defun mew-input-refile-folders (folder-list singlep case proto)
  "Input refile folders from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let ((mew-input-complete-function (if (mew-folder-imapp proto)
					 'mew-complete-imap-folder
				       'mew-complete-local-folder))
	(mew-inherit-case case)
	(mew-input-folder-search-multi t)
	(mew-input-folder-refile t)
	default prompt init ret)
    (cond
     (singlep
      (setq default (car folder-list))
      (setq init (mew-folder-prefix default))
      (if case
	  (setq prompt (format "Folder name <%s:> (%s): " case default))
	(setq prompt (format "Folder name (%s): " default))))
     (t
      (if case
	  (setq prompt (format "Folder name <%s:>: " case))
	(setq prompt "Folder name: "))
      (setq init (mew-join "," folder-list))))
    (setq ret (read-from-minibuffer prompt
				    init mew-input-folder-map nil
				    'mew-input-folder-hist))
    (when (and singlep (string= ret init))
      (setq ret default))
    (setq ret (mapcar 'mew-chop (mew-split ret ?,)))
    (mew-input-refile-folder-check
     ret (if (mew-folder-imapp proto) 'imap 'local))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address
;;;

(defvar mew-input-address-hist nil)

(defun mew-input-address (prompt &optional default)
  (mew-input-clear)
  (let ((mew-input-complete-function 'mew-complete-address)
	val vals addrs ret)
    (setq val (read-from-minibuffer 
	       (if default (format prompt default) prompt)
	       ""
	       mew-input-map
	       nil
	       'mew-input-address-hist))
    (if (and default (string= val ""))
	(setq val default))
    (setq vals (mapcar 'mew-chop (mew-split-quoted val ?,)))
    (while vals
      (setq val (car vals))
      (setq vals (cdr vals))
      (setq addrs (mew-alias-expand val mew-addrbook-alist 0))
      (setq addrs (mapcar 'mew-addrstr-append-domain addrs))
      (setq ret (nconc ret addrs)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pick pattern
;;;

(defvar mew-input-pick-pattern-hist nil)

(defun mew-input-pick-pattern ()
  (mew-input-clear)
  (let ((mew-input-complete-function 'mew-complete-pick-pattern)
	(keymap (copy-keymap mew-input-map)))
    (define-key keymap " " nil)
    (mew-pick-macro-expand-string
     (read-from-minibuffer "Pick pattern: "
			   mew-pick-default-field
			   keymap
			   nil
			   'mew-input-pick-pattern-hist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grep pattern
;;;

(defun mew-input-grep-pattern ()
  (read-string "Grep pattern: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort key
;;;
;;; mew-sort-default-key-alist

(defvar mew-input-sort-key-hist nil)

(defun mew-input-sort-key-check ()
  (let* ((field:mode (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
	 (mode (car (cdr (mew-split field:mode ?:))))
	 err)
    (if mode
	(unless (member mode mew-sort-modes)
	  (setq err mode)))
    (if err
        (progn
          (mew-temp-minibuffer-message (format " [No match: %s]" err))
          nil)
      t)))

(defun mew-input-sort-key (key)
  (mew-input-clear)
  (let* ((mew-input-complete-function 'mew-complete-sort-key)
	 (mew-input-exit-minibuffer-function 'mew-input-sort-key-check)
	 (field:mode (read-from-minibuffer
		      (format "Sort by (%s)?: " key)
		      ""
		      mew-input-map
		      nil 
		      'mew-input-sort-key-hist))
	 field mode)
    (if (or (null field:mode) (string= field:mode ""))
	(setq field:mode key))
    (setq field (car (mew-split field:mode ?:)))
    (setq mode  (or (car (cdr (mew-split field:mode ?:)))
		    (cdr (assoc field mew-sort-key-alist))
		    "text"))
    (cons field mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remote file
;;;

(defvar mew-input-rfile-hist nil)

(defun mew-input-rfile (prompt) ;; prompt="To:"
  (mew-input-clear)
  (let ((mew-input-complete-function 'mew-complete-rfile))
    (read-from-minibuffer
     (concat prompt " ")
     ""
     mew-input-map
     nil
     'mew-input-rfile-hist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Range
;;;

(defun mew-range-update (folder)
  (when (get-buffer folder)
    (save-excursion
      (set-buffer folder)
      (goto-char (point-max))
      (if (bobp)
	  mew-range-all ;; buffer is empty.
	(forward-line -1)
	(mew-summary-goto-message)
	(concat (int-to-string
		 (1+ (string-to-int (mew-summary-message-number))))
		"-")))))

(defun mew-input-range (folder askp)
  (let ((default (or (car (mew-folder-spec folder mew-range-list
					   mew-range-list-string-type
					   mew-range-list-list-type))
		     mew-range-str-update))
	comp range ret)
    (when askp
      (setq comp (mapcar 'list mew-input-range-list))
      (setq range (completing-read (format "Range (%s): " default) comp)))
    (if (or (string= range "") (null range))
	(setq range default))
    (cond
     ((string= range mew-range-str-all)
      (setq ret (list mew-range-all 'erase)))
     ((string= range mew-range-str-update)
      (setq ret (list (mew-range-update folder) nil)))
     ((string-match "^last:" range)
      (setq ret (list range 'erase)))
     ((or (string-match "^[0-9]+$" range)
	  (string-match "^[0-9]+-$" range)
	  (string-match "^-[0-9]+$" range)
	  (string-match "^[0-9]+-[0-9]+$" range))
      (setq ret (list range 'erase)))
     (t
      (setq ret nil))) ;; a wrong range
    ret))

(defun mew-input-range-remote (folder)
  ;; t   all
  ;; nil update
  ;; n   last:<n>
  ;; 'sync
  (let* ((default (or (car (mew-folder-spec folder mew-range-list
					    mew-range-list-string-type
					    mew-range-list-list-type))))
	 (comp (mapcar 'list mew-input-range-remote-list))
	 (range (completing-read (format "Range (%s): " default) comp)))
    (if (or (string= range "") (null range))
	(setq range default))
    (cond
     ((string= range mew-range-str-sync)   'sync)
     ((string= range mew-range-str-all)      t)
     ((string= range mew-range-str-update) nil)
     ((string-match "^last:\\([0-9]+\\)$" range)
      (string-to-int (match-string 1 range)))
     (t nil)))) ;; update just in case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer
;;;

(defun mew-input-draft-buffer (default)
  (let* ((regex (mew-folder-regex (file-name-as-directory mew-draft-folder)))
	 (comp (mew-buffer-list regex t))
	 buf)
    (if (and (= (length comp) 1)
	     (string= default (car (car comp))))
	default
      (setq buf (completing-read (format "Buffer (%s): " default) comp))
      (if (string= buf "")
	  default
	buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File
;;;

(defun mew-input-file-name (&optional prompt default)
  (let ((msg (or prompt "File: "))
	(use-dialog-box nil)
	file)
    (cond
     ((null default)
      (setq file mew-home))
     ((mew-folder-absolutep default)
      (setq file default))
     (t
      (setq file (concat mew-home default))))
    (expand-file-name (read-file-name msg file file))))

(defun mew-input-directory-name (&optional default)
  (let ((dir (expand-file-name
	      (read-file-name "Directory: " default default t))))
    (if (file-directory-p dir)
	dir
      (mew-warn "%s is not directory" dir)
      (mew-input-directory-name default))))

(defun mew-convert-to-home-dir (dir)
  (let* ((chome (file-name-as-directory mew-home))
	 (ehome (expand-file-name chome)))
    (if (string-match ehome dir)
	(concat chome (substring dir (match-end 0) nil))
      dir)))

(defvar mew-summary-previous-directory nil)
(defvar mew-draft-previous-directory nil)

(defmacro mew-mode-input-file-name (prompt file preservep previous modedir)
  `(let (dir ret def)
     (if (and ,file (file-name-absolute-p ,file))
	 (setq def (mew-convert-to-home-dir ,file))
       (if ,preservep
	   (setq dir (or ,previous ,modedir))
	 (setq dir , modedir))
       (setq dir (and dir (file-name-as-directory dir)))
       (setq def (concat dir ,file)))
     (setq ret (mew-input-file-name ,prompt def))
     (if ,preservep
	 (setq ,previous (file-name-directory (mew-convert-to-home-dir ret))))
     ret))

(defun mew-summary-input-file-name (&optional prompt file)
  (mew-mode-input-file-name prompt file mew-summary-preserve-dir
			    mew-summary-previous-directory mew-save-dir))

(defun mew-draft-input-file-name (&optional prompt file)
  (mew-mode-input-file-name prompt file mew-draft-preserve-dir
			    mew-draft-previous-directory mew-copy-dir))

(defmacro mew-mode-input-directory-name (preservep previous modedir)
  `(if ,preservep
       (let (dir ret)
	 (setq dir (file-name-as-directory (or ,previous ,modedir)))
	 (setq ret (mew-input-directory-name dir))
	 (setq ,previous (mew-convert-to-home-dir ret))
	 ret)
     (mew-input-directory-name default-directory)))

(defun mew-summary-input-directory-name ()
  (mew-mode-input-directory-name
   mew-summary-preserve-dir mew-summary-previous-directory mew-save-dir))

(defun mew-draft-input-directory-name ()
  (mew-mode-input-directory-name
   mew-draft-preserve-dir mew-draft-previous-directory mew-copy-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; String
;;;

(defun mew-input-string (prompt subdir default)
  (let ((input (read-string (format prompt subdir default) "")))
    (if (string= input "") default input)))

(defun mew-input-general (prompt alist &optional require-match initial)
  (let* ((completion-ignore-case t)
	 (question (if initial (format "%s (%s): " prompt initial)
		     (format "(%s): " prompt)))
	 (value (completing-read question alist nil require-match nil)))
    (if (and initial (string= value "")) initial value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type
;;;

(defun mew-input-type (prompt filename default type-list)
  (let ((completion-ignore-case t)
	(type))
    (setq type (completing-read
		(format prompt filename default)
		(mapcar 'list type-list)
		nil
		t
		""))
    (if (string= type "") default type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Case
;;;

(defun mew-input-case-check ()
  (let* ((case (mew-buffer-substring (mew-minibuf-point-min) (point-max)))
         (lst (mew-split case ?,))
	 err)
    (catch 'nomatch
      (while lst
	(unless (member (car lst) mew-config-cases)
	  (throw 'nomatch (setq err (car lst))))
	(setq lst (cdr lst))))
    (if err
        (progn
          (mew-temp-minibuffer-message (format " [No match: %s]" err))
          nil)
      t)))

(defun mew-input-case (default msg &optional edit)
  (mew-input-clear)
  (unless default (setq default mew-case-default))
  (let ((mew-input-complete-function 'mew-complete-case)
	(mew-input-exit-minibuffer-function 'mew-input-case-check)
	(mew-input-comma-function 'mew-input-case-check)
	case lst ret)
    (if edit
	(setq case (read-from-minibuffer
		    (format "%s case value: " msg)
		    default
		    mew-input-map))
      (setq case (read-from-minibuffer
		  (format "%s case value (%s): " msg default)
		  ""
		  mew-input-map)))
    (if (string= case "")
	default
      (setq lst (mew-split case ?,))
      (while lst
	(if (member (car lst) mew-config-cases)
	    (setq ret (cons (car lst) ret)))
	(setq lst (cdr lst)))
      (mapconcat 'identity (nreverse ret) ","))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mark
;;;

(defun mew-input-mark ()
  (let ((ociea cursor-in-echo-area) char)
    (unwind-protect
	(progn
	  (message "Input mark: ")
	  (setq cursor-in-echo-area t)
	  (setq char (read-char))
	  (unless (char-equal char ?\r)
	    (message "Input mark: %s" (char-to-string char))))
      (setq cursor-in-echo-area ociea))
    (cond
     ((char-equal char ?\r) char)
     ((mew-markdb-by-mark char) char)
     (t (message "Mark %c is not supported" char)
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Password
;;;

(defvar mew-passwd-alist nil)
(defvar mew-passwd-timer-id nil)

(defsubst mew-passwd-get-passwd (key)
  (nth 1 (assoc key mew-passwd-alist)))
(defsubst mew-passwd-get-counter (key)
  (nth 2 (assoc key mew-passwd-alist)))

(defun mew-passwd-set-passwd (key val)
  (if (assoc key mew-passwd-alist)
      (setcar (nthcdr 1 (assoc key mew-passwd-alist)) val)
    (setq mew-passwd-alist (cons (list key val 0) mew-passwd-alist))))
(defun mew-passwd-set-counter (key val)
  (if (assoc key mew-passwd-alist)
      (setcar (nthcdr 2 (assoc key mew-passwd-alist)) val)))

(defun mew-passwd-get-keys ()
  (mapcar 'car mew-passwd-alist))

(defsubst mew-passwd-reset ()
  (setq mew-passwd-alist nil))

(defun mew-passwd-setup ()
  (if mew-passwd-timer-id (cancel-timer mew-passwd-timer-id))
  (setq mew-passwd-timer-id
	(mew-timer (* mew-passwd-timer-unit 60) 'mew-passwd-timer)))

(defun mew-passwd-clean-up ()
  (mew-passwd-reset)
  (if mew-passwd-timer-id (cancel-timer mew-passwd-timer-id))
  (setq mew-passwd-timer-id nil))

(defun mew-passwd-timer ()
  (let ((keys (mew-passwd-get-keys)) key)
    (while keys
      (setq key (car keys))
      (setq keys (cdr keys))
      (if (< (mew-passwd-get-counter key) mew-passwd-lifetime)
	  (mew-passwd-set-counter key (1+ (mew-passwd-get-counter key)))
	;; time out
	(mew-passwd-set-passwd key nil)
	(mew-passwd-set-counter key 0)))))

(defun mew-input-passwd (prompt key)
  (if (and key mew-use-cached-passwd)
      (if (mew-passwd-get-passwd key)
	  (progn
	    (mew-timing)
	    (if mew-passwd-reset-timer (mew-passwd-set-counter key 0))
	    (mew-passwd-get-passwd key))
	(let ((pass (mew-read-passwd prompt)))
	  (mew-passwd-set-passwd key pass)
	  (mew-passwd-set-counter key 0)
	  pass))
    (mew-read-passwd prompt)))

(defun mew-read-passwd (prompt)
  (let ((inhibit-input-event-recording t)
	;; A process filter sets inhibit-quit to t to prevent quitting.
	;; Set inhibit-quit to nil so that C-g can be used
	(inhibit-quit nil))
    (condition-case nil
	(read-passwd prompt)
      ;; If read-passwd causes an error, let's return "" so that
      ;; the password process will safely fail.
      (quit "")
      (error ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Language name
;;;

;; read-language-name should be used. Unfortunately, that of
;; XEmacs 21.1.14 is broken. This function should be obsoleted
;; when Mew quit support of XEmacs 21.1.14

(defun mew-input-language-name (prompt &optional default) 
  (let* ((completion-ignore-case t)
         (name (completing-read prompt
                                language-info-alist
                                nil
                                t nil nil)))
    (if (string= name "") (setq name default))
    name))

(provide 'mew-minibuf)

;;; Copyright Notice:

;; Copyright (C) 1997-2003 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-minibuf.el ends here
