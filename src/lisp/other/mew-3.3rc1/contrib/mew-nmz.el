;; -*- Mode: Emacs-Lisp -*-
;;
;; mew-nmz.el: 
;;
;;   Author: Hideyuki SHIRAI <shirai@mew.org>
;;
;;;; Usage: Put your ~/.emacs.
;; (eval-after-load "mew" '(require 'mew-nmz))
;;

(defconst mew-nmz-version "mew-nmz.el 0.70")

(eval-when-compile
  (require 'mew)
  (defvar namazu-history))

(and (locate-library "namazu")
     (eval-when-compile (require 'namazu)))

;; Variables
(defvar mew-nmz-index-path "~/Namazu" "*Namazu index top directory.")
(defvar mew-nmz-index-mail "Mail" "*Namazu index mail directory.")
(defvar mew-nmz-index-news "News" "*Namazu index local news directory.")

(defvar mew-nmz-namazu-version nil
  "*Automatically set 'v1 if Namazu version 1. Set 'v2 if Namazu version 2.")

(defvar mew-nmz-use-fast-pick t "*Use fast pick mode.")
(defvar mew-nmz-use-backslash
  (if (memq system-type '(OS/2 emx windows-nt)) t nil)
  "*If non-nil, convert / => \ for Windows.")

(defvar mew-nmz-prog "namazu" "*Namazu program name.")

(defvar mew-nmz-db-max 64 "*Namazu max index.")
(defvar mew-nmz-query-max-length 256 "*Namazu query string max length.")

(defvar mew-nmz-prog-mknmz "mknmz" "*Namazu make index program.")
(defvar mew-nmz-prog-mknmz-args '("-q")
  "*Mknmz's argment, in addition to \"-U\", \"-h\".")
(defvar mew-nmz-prog-mknmz-include "~/Namazu/mknmz-inc.pl" "*Include file for mknmz.")
(defvar mew-nmz-mknmz-use-timestamp nil
  "*If non-nil, use NMZ.stamp file (see. immknmz --timestamp=on).")

(defvar mew-nmz-prog-gcnmz "gcnmz" "*Namazu refresh index program.")
(defvar mew-nmz-use-gcnmz-folders (list mew-inbox-folder)
  "*Exec gcnmz after mknmz for index refresh, 't' means all folders.")

(defvar mew-nmz-mknmz-skip-folders
  (list mew-draft-folder mew-trash-folder mew-queue-folder mew-attach-folder
	"+schedule" "=draft")
  "*Skip folders regexp, when make index.")
(defvar mew-nmz-mknmz-skip-news t "*Skip local news folders, when make index.")

(defvar mew-nmz-mknmz-use-mode-line t "*Display \"nmz\" in mode line , when mknmzing.")
(defvar mew-nmz-line-id '("Mew(nmz): %7b")
  "*A value of mode-line-buffer-identification for Mew summary mode, when mknmzing.")

(defvar mew-nmz-pick-default-field nil
  "*Default prefix string to be appeared when inputing a namazu pick pattern.
A good example is \"+from:\".")
(defvar mew-nmz-pick-field-list
  '("+subject:" "+from:" "+to:" "+newsgroups:" "+date:"
    "+message-id:" "+cc:" "+in-reply-to:" "+references:")
  "*A list of key for namazu pick pattern.")

(defvar mew-nmz-pick-gather-field-list
  (list (list mew-from: 'address "+from:" "+to:" "+cc:")
	(list mew-to: 'address "+from:" "+to:" "+cc:")
	(list mew-cc: 'address "+from:" "+to:" "+cc:")
	(list mew-message-id: 'msgid "+message-id:" "+in-reply-to:" "+references:")
	(list mew-in-reply-to: 'msgid "+message-id:" "+in-reply-to:" "+references:")
	(list mew-references: 'msgid "+message-id:" "+in-reply-to:" "+references:"))
  "*A list of completion keyword from message.")

(defvar mew-nmz-search-parent-folder (list mew-inbox-folder)
  "*Search folder for parent or child, ")

(defvar mew-nmz-mark-pick mew-mark-review "*Mark for Namazu pick.")
(defvar mew-nmz-mark-unindexed mew-mark-review "*Mark for type unindexed messages.")

(defvar mew-nmz-use-namazu-el (locate-library "namazu")
  "*Use namazu-mode from mew.")
(defvar mew-nmz-namazu-full-window t "*Use namazu-mode full window.")

;; internal variable, don't modify.
(defconst mew-nmz-namazu-version1-str "^  Search Program of Namazu v1\.[34]\.")
(defconst mew-nmz-namazu-version2-str "^namazu of Namazu [1-9.]+")
(defvar mew-nmz-pick-pattern-hist nil)
(defvar mew-nmz-gather-header-list nil)
(defvar mew-nmz-namazu-last-folder nil)
(defvar mew-nmz-indexed-folders nil)

(defvar mew-nmz-mknmz-process nil)
(defvar mew-nmz-mknmz-process-file nil)
(defvar mew-nmz-mknmz-process-folder nil)
(make-variable-buffer-local 'mew-nmz-mknmz-process)
(make-variable-buffer-local 'mew-nmz-mknmz-process-file)
(make-variable-buffer-local 'mew-nmz-mknmz-process-folder)

;; Key Bind
(add-hook 'mew-summary-mode-hook
	  (lambda ()
	    (define-key mew-summary-mode-map "z?" 'mew-nmz-search-mark)
	    (define-key mew-summary-mode-map "z/" 'mew-nmz-virtual)
	    (define-key mew-summary-mode-map "zV" 'mew-nmz-virtual)
	    (define-key mew-summary-mode-map "zm" 'mew-nmz-mknmz)
	    (define-key mew-summary-mode-map "zu" 'mew-nmz-mark-unindexed)
	    (define-key mew-summary-mode-map "z^" 'mew-nmz-search-parent)
	    (define-key mew-summary-mode-map "zp" 'mew-nmz-search-parent)
	    (define-key mew-summary-mode-map "zn" (lambda (arg)
						    (interactive "P")
						    (mew-nmz-search-parent (not arg))))
	    (define-key mew-summary-mode-map "zN" 'mew-nmz-namazu)))

(add-hook 'mew-message-mode-hook
	  (lambda ()
	    (define-key mew-message-mode-map "zp" 'mew-nmz-search-msgid-at-point)
	    (define-key mew-message-mode-map "zr" 'mew-nmz-search-msgid-region)))

(add-hook 'mew-quit-hook
	  (lambda ()
	    (setq mew-nmz-indexed-folders nil)))

;; An addition for virtual mode.
(add-hook 'mew-virtual-mode-hook
	  (lambda ()
	    (define-key mew-summary-mode-map "zj" 'mew-virtual-original-message)))

(defun mew-virtual-original-message (&optional arg)
  "Show original message location.
If executed with 'C-u', jump to original message folder and number."
  (interactive "P")
  (if (not (mew-virtual-p))
      (message "This command can be used in Virtual mode only.")
    (save-excursion
      (let ((fld (mew-summary-folder-name))
	    (msg (mew-summary-message-number)))
	(message "Original message at %s/%s" fld msg)
	(when arg
	  (mew-nmz-goto-folder-msg fld msg)
	  (message "Original message at %s/%s...jump done" fld msg))))))

;; macros
(defmacro mew-nmz-expand-folder (fld)
  "Convert fld to namazu-index-dir."
  `(when (mew-folder-localp ,fld)
     (expand-file-name
      (substring ,fld 1 nil)
      (expand-file-name mew-nmz-index-mail mew-nmz-index-path))))

;;
;; Namazu Version check.
(defun mew-nmz-namazu-version ()
  (or mew-nmz-namazu-version (mew-nmz-version-set)))

(defun mew-nmz-version-set ()
  (interactive)
  (if mew-nmz-namazu-version
      mew-nmz-namazu-version
    (with-temp-buffer
      (mew-piolet
       mew-cs-autoconv mew-cs-text-for-write
       (apply (function call-process)
	      mew-nmz-prog nil t nil (list "-L" "en" "-v")))
      (goto-char (point-min))
      (if (re-search-forward mew-nmz-namazu-version2-str nil t)
	  (setq mew-nmz-namazu-version 'v2)
	(goto-char (point-min))
	(if (re-search-forward mew-nmz-namazu-version1-str nil t)
	    (setq mew-nmz-namazu-version 'v1)
	  (ding)
	  (error "Something error occor. (Namazu version check)")
	  (sit-for 1))))))

;;
;; "Make Index" functions.
(defun mew-nmz-mknmz (&optional fld remove callp nocheck)
  "Make namazu index for mew-nmz.
If executed with 'C-u', remove index files at the specified folder."
  (interactive)
  (save-excursion
    (let ((msgenb (interactive-p))
	  (figfld nil)
	  procname)
      (if (not (mew-which mew-nmz-prog-mknmz exec-path))
	  (message "Please install mknmz.")
	(and current-prefix-arg (setq remove t))
	(if (not fld)
	    (setq fld (mew-input-folder (mew-sinfo-get-case)
					(mew-summary-folder-name 'ext)))
	  (setq fld (directory-file-name fld)))
	(setq figfld (mew-nmz-have-figure-folder fld))
	(if (or (mew-folder-remotep fld)
		(mew-folder-virtualp fld)
		(mew-nmz-skip-folder fld))
	    (and msgenb (message "Can't make namazu index in %s." fld))
	  (if (and (not nocheck) (not remove) (mew-nmz-index-newp fld))
	      (message "%s has a newer namazu index." fld)
	    (let ((flddir (mew-expand-folder fld))
		  (nmzdir (mew-nmz-expand-folder fld))
		  (bufname (concat " *mew mknmz*" fld)))
	      (setq procname (concat mew-nmz-prog-mknmz "-" fld))
	      (if (get-process procname)
		  (and msgenb (message "Detect running mknmz process in %s." fld))
		(if (and flddir nmzdir (file-directory-p flddir))
		    (let* ((default-file-name-coding-system nil) ;; this make scan faster
			   (file-name-coding-system nil)
			   (flist (directory-files flddir 'full-name
						   mew-regex-message-files
						   'nosort))
			   (tmpf (expand-file-name
				  (mew-nmz-make-temp-name "mknmz_") mew-temp-dir))
			   (args mew-nmz-prog-mknmz-args)
			   (backslash (and mew-nmz-use-backslash
					   (eq (mew-nmz-namazu-version) 'v1)))
			   (exist-msg nil)
			   file)
		      (setq args (append args (list "-Uh")))
		      (while (and mew-nmz-use-backslash (file-exists-p tmpf))
			(message "Warning!! same name det.")
			(sit-for 1)
			(setq tmpf (expand-file-name
				    (mew-nmz-make-temp-name "mknmz_") mew-temp-dir)))
		      (with-temp-buffer
			(while flist
			  (setq file (car flist))
			  (setq exist-msg t)
			  (if (and figfld (file-directory-p file))
			      ()
			    (if backslash
				(insert (mew-nmz-slash-to-backslash file) "\n")
			      (insert file "\n")))
			  (setq flist (cdr flist)))
			(mew-frwlet
			 mew-cs-autoconv mew-cs-text-for-write
			 (write-region (point-min) (point-max) tmpf nil 'nomsg)))
		      (if (or (not exist-msg) remove)
			  (mew-nmz-index-delete nmzdir))
		      (if (not exist-msg)
			  (progn
			    (delete-file tmpf)
			    (and msgenb (message "%s has no message." fld)))
			(and (not (file-directory-p nmzdir))
			     (mew-make-directory nmzdir))
			(if mew-nmz-prog-mknmz-include
			    (let ((incfile (expand-file-name mew-nmz-prog-mknmz-include)))
			      (and (file-exists-p incfile)
				   (setq args (append args
						      (list "-I" incfile))))))
			(setq args (append args (list "-F" tmpf "-O" nmzdir)))
			(if (file-name-all-completions "NMZ.lock" nmzdir)
			    (progn
			      (message "Warning!! Something error in %s's index." fld)
			      (sit-for 1)))
			(set-buffer (get-buffer-create bufname))
			(buffer-disable-undo (current-buffer))
			(erase-buffer)
			;; folder set
			(if callp
			    (progn
			      (mew-nmz-timestamp-new fld)
			      (message "Mew mknmz (%s)..." fld)
			      (mew-piolet
			       mew-cs-autoconv mew-cs-text-for-write
			       (apply (function call-process)
				      mew-nmz-prog-mknmz
				      nil (current-buffer) nil args))
			      (mew-nmz-timestamp-rename fld)
			      (if (and mew-nmz-prog-gcnmz
				       (not remove)
				       (or (eq mew-nmz-use-gcnmz-folders t)
					   (member fld mew-nmz-use-gcnmz-folders))
				       (mew-which mew-nmz-prog-gcnmz exec-path))
				  (progn
				    (message "Mew mknmz (%s)...refresh" fld)
				    (mew-piolet
				     mew-cs-autoconv mew-cs-text-for-write
				     (apply (function call-process)
					    mew-nmz-prog-gcnmz
					    nil
					    (current-buffer)
					    nil
					    (list
					     (and (eq (mew-nmz-namazu-version) 'v2)
						  "--no-backup")
					     (expand-file-name "NMZ" nmzdir)))
				     (if (eq (mew-nmz-namazu-version) 'v1)
					 (let ((backfn (directory-files nmzdir t ".*.BAK$")))
					   (while backfn
					     (if (file-writable-p (car backfn))
						 (delete-file (car backfn)))
					     (setq backfn (cdr backfn))))))))
			      (delete-file tmpf)
			      (goto-char (point-min))
			      (if (search-forward-regexp "^ERROR:.*$" nil t)
				  (message "Mew mknmz (%s)...%s" fld (match-string 0))
				(message "Mew mknmz (%s) ...done" fld))
			      (set-buffer-modified-p nil)
			      (kill-buffer (current-buffer)))
			  ;; start-process
			  (and msgenb (message "Mew mknmz (%s)..." fld))
			  (mew-nmz-timestamp-new fld)
			  (mew-piolet
			   mew-cs-autoconv mew-cs-text-for-write
			   (setq mew-nmz-mknmz-process
				 (apply (function start-process)
					procname (current-buffer)
					mew-nmz-prog-mknmz args)))
			  (setq mew-nmz-mknmz-process-file tmpf)
			  (setq mew-nmz-mknmz-process-folder fld)
			  (if (and mew-nmz-mknmz-use-mode-line
				   fld (get-buffer fld) (buffer-name (get-buffer fld)))
			      (save-excursion
				(set-buffer (get-buffer fld))
				(setq mode-line-buffer-identification mew-nmz-line-id)
				(set-buffer-modified-p nil)))
			  (set-process-sentinel mew-nmz-mknmz-process 'mew-nmz-mknmz-sentinel))
			)))))))))))

(defun mew-nmz-mknmz-sentinel (process event)
  (save-excursion
    (set-buffer (process-buffer process))
    (let ((fld mew-nmz-mknmz-process-folder)
	  (tmpfile mew-nmz-mknmz-process-file)
	  msg)
      (goto-char (point-min))
      (if (search-forward-regexp "^ERROR:.*$" nil t)
	  (setq msg (format "Mew mknmz (%s)...%s" fld (match-string 0)))
	(setq msg (format "Mew mknmz (%s)...done" fld))
	(if (not (or (null mew-nmz-indexed-folders)
		     (member fld mew-nmz-indexed-folders)))
	    (setq mew-nmz-indexed-folders
		  (cons fld mew-nmz-indexed-folders))))
      (and (file-readable-p tmpfile)
	   (file-writable-p tmpfile)
	   (condition-case nil
	       (progn
		 (delete-file tmpfile)
		 (mew-nmz-timestamp-rename fld))
	     (error nil)))
      (setq mew-nmz-mknmz-process nil)
      (setq mew-nmz-mknmz-process-file nil)
      (setq mew-nmz-mknmz-process-folder nil)
      (if (and mew-nmz-mknmz-use-mode-line
	       fld (get-buffer fld) (buffer-name (get-buffer fld)))
	  (save-excursion
	    (set-buffer (get-buffer fld))
	    (setq mode-line-buffer-identification mew-mode-line-id)
	    (set-buffer-modified-p nil)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (message "%s" msg)
      (sit-for 1))))

;; mode-line-buffer-identification modifier
(add-hook 'mew-summary-mode-hook
	  (lambda ()
	    (if (and mew-nmz-mknmz-use-mode-line
		     (get-process (concat mew-nmz-prog-mknmz "-" (buffer-name))))
		(setq mode-line-buffer-identification mew-nmz-line-id)
	      (setq mode-line-buffer-identification mew-mode-line-id))
	    (set-buffer-modified-p nil)))

(defun mew-nmz-mknmz-all-folders (&optional arg)
  "Make namazu index all folders."
  (interactive "P")
  (if (not arg)
      ()
    (message "folder setup...")
    (mew-local-update (interactive-p))
    (message "folder setup...done"))
  (save-excursion
    (let ((flist mew-local-folder-alist))
      (while (car flist)
	(condition-case nil
	    (mew-nmz-mknmz (car (car flist)) nil 'call)
	  (error nil))
	(setq flist (cdr flist)))))
  (message "Namazu make index done."))

(defun mew-nmz-mark-unindexed ()
  "Mark unindexed messages."
  (interactive)
  (mew-summary-only
   (if (mew-summary-exclusive-p)
       (save-excursion
	 (if (and (mew-summary-mark-collect
		   mew-nmz-mark-unindexed (point-min) (point-max))
		  (y-or-n-p (format "Unmark '%c'? " mew-nmz-mark-unindexed)))
	     (mew-mark-undo-mark mew-nmz-mark-unindexed 'nomsg))
	 (let* ((ufname
		 (expand-file-name
		  (if (eq (mew-nmz-namazu-version) 'v2)
		      "NMZ.field.uri" "NMZ.field.url")
		  (mew-nmz-expand-folder (buffer-name))))
		(mmsgs 0)
		(umsgs 0)
		msgnums)
	   (if (not (file-exists-p ufname))
	       (message "%s has no index file." (buffer-name))
	     (with-temp-buffer
	       (message "checking %s..." (file-name-nondirectory ufname))
	       (insert-file-contents ufname)
	       (while (re-search-forward "/\\([1-9][0-9]*\\)$" nil t)
		 (setq msgnums (cons (string-to-number (match-string 1)) msgnums))))
	     (message "checking %s..." (buffer-name))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (if (and
		    (looking-at "^ *\\([1-9][0-9]*\\)")
		    (not (memq (string-to-number (match-string 1)) msgnums))
		    (not (mew-in-decode-syntax-p)))
		   (progn
		     (setq umsgs (1+ umsgs))
		     (if (mew-summary-marked-p)
			 ()
		       (mew-summary-mark-as mew-nmz-mark-unindexed)
		       (setq mmsgs (1+ mmsgs)))))
	       (forward-line))
	     (cond
	      ((= umsgs 1)
	       (message "%d message doesn't have index, %d marked."
			umsgs mmsgs))
	      ((> umsgs 1)
	       (message "%d messages don't have index, %d marked."
			umsgs mmsgs))
	      (t
	       (message "all messages have index.")))))))))

;;
;; "search Message-ID" functions.
(defun mew-nmz-search-parent (&optional child mid)
  "Search *parent* message and jump to that.
If executed with 'C-u', search *child* message."
  (interactive "P")
  (let ((fld (mew-summary-folder-name))
	(idh (list (list mew-in-reply-to: mew-references:)
		   (list mew-message-id:)))
	(message (if child "children" "parent"))
	(refilefld (copy-sequence mew-nmz-search-parent-folder))
	(proto (or (mew-proto-to-refile (mew-summary-folder-name 'ext))
		   "+"))
	(case (mew-sinfo-get-case))
	refiledir mess ref pid rh)
    (if mid
	(setq pid (list mid) idh nil)
      (if (not (or (mew-summary-message-number) (mew-syntax-number)))
	  (message "No message here.")
	(save-excursion
	  (mew-summary-display nil)
	  (setq mess (or (mew-cache-hit
			  (mew-summary-folder-name) (mew-summary-message-number))
			 (mew-buffer-message)))
	  (set-buffer mess)
	  (mew-sinfo-set-case case)
	  (mew-sinfo-set-proto proto)
	  (setq refilefld (append (car (mew-refile-guess nil t)) refilefld))
	  (if child
	      (setq idh (car (cdr idh)))
	    (setq idh (car idh)))
	  (while idh
	    (setq rh (car idh))
	    (setq ref (mew-header-get-value rh))
	    (while (and ref (string-match "<\\([^>]+\\)>" ref))
	      (setq pid (cons (concat "\"" (match-string 1 ref) "\"") pid))
	      (setq refilefld
		    (cons (nth 1 (assoc (car pid) mew-refile-msgid-alist)) refilefld))
	      (setq ref (substring ref (match-end 0))))
	    (setq idh (cdr idh)))
	  (setq refilefld (cons fld refilefld))
	  (setq refilefld (mew-uniq-list (delete nil refilefld)))
	  (setq refiledir
		(delete nil (mapcar
			     (lambda (x)
			       (mew-nmz-expand-folder x))
			     refilefld))))))
    (if (null pid)
	(message "No required header.")
      (if (mew-syntax-number)
	  (while (not (mew-summary-message-number))
	    (forward-line -1)))
      (set-marker mew-summary-inbox-position (point) (current-buffer))
      (let ((pattern1 "")
	    (pattern2 "")
	    (addpattern (if child "+in-reply-to:" "+message-id:"))
	    (range nil))
	(if (not child)
	    (setq pattern1 (concat addpattern (car pid)))
	  (setq pattern1 (concat addpattern (car pid)))
	  (setq addpattern "+references:")
	  (setq pattern1 (concat pattern1 " | " addpattern (car pid))))
	(setq pid (delete (car pid) pid))
	(while pid
	  (if (> (length (concat pattern2 addpattern (car pid)))
		 mew-nmz-query-max-length)
	      (setq pid nil)
	    (setq pattern2 (concat pattern2 addpattern (car pid)))
	    (setq addpattern (if child " | +references:" " | +message-id:"))
	    (setq pid (delete (car pid) pid))))
	(message "Searching %s..." message)
	(let ((pattern (list pattern1 pattern2)))
	  (while (and (null range) pattern)
	    (if mid
		()
	      (message "Searching %s...%s" message (mew-join ", " refilefld))
	      (setq range (mew-nmz-multi-pick refiledir (car pattern)))
	      (if range
		  (catch 'detect
		    (while refilefld
		      (if (null (setq idh (assoc (car refilefld) range)))
			  ()
			(setq fld (car idh))
			(if child
			    (setq range (cdr idh))
			  (setq range (nreverse (cdr idh))))
			(throw 'detect t))
		      (setq refilefld (cdr refilefld)))
		    nil)))
	    (if range
		()
	      ;; all folder search
	      (message "Searching %s...all folders" message)
	      (setq range (mew-nmz-multi-pick
			   (mew-nmz-expand-folder-regexp "*")
			   (car pattern) t))
	      (if (null range)
		  (setq pattern (cdr pattern))
		(setq fld (car (car range)))
		(setq range (cdr (car range)))
		(if (not child) (setq range (nreverse range)))
		))))
	(if (null range)
	    (message "No message found.")
	  (if (mew-virtual-p)
	      (save-excursion
		(goto-char (point-min))
		(if (re-search-forward
		     (concat " \006 " (regexp-quote fld) " " (car range) "$")
		     nil t)
		    (progn
		      (setq fld (buffer-name))
		      (goto-char (match-beginning 0))
		      (beginning-of-line)
		      (looking-at "^ *\\([1-9][0-9]*\\)[^0-9]")
		      (setq range (list (match-string 1)))))))
	  (if (listp (car range))
	      (setq fld (car (car range))
		    mess (car (cdr (car range))))
	    (setq mess (car range)))
	  (mew-nmz-goto-folder-msg fld mess)
	  (message "Searching %s...%s/%s" message fld mess))))))

(defun mew-nmz-search-msgid-at-point ()
  (interactive)
  (let (start end (pos (point)))
    (if (and (re-search-backward "<" (save-excursion (beginning-of-line) (point)) t)
	     (setq start (point))
	     (re-search-forward ">" (save-excursion (end-of-line) (point)) t)
	     (setq end (point)))
	(mew-nmz-search-msgid (buffer-substring start end))
      (message "No Message-ID."))))

(defun mew-nmz-search-msgid-region (start end)
  (interactive "r")
  (mew-nmz-search-msgid (buffer-substring start end)))

(defun mew-nmz-search-msgid (mid)
  (interactive "sMessage-ID: ")
  (if (string-match "<\\([^>]+\\)>" mid)
      (let ((mew-window-use-full t)
	    (mew-use-full-window t))
	(mew-nmz-search-parent nil (concat "\"" (match-string 1 mid) "\"")))
    (message "No Message-ID.")))

;;
;; "Search" functions.
(defun mew-nmz-search-mark ()
  "Namazu pick messages according to a pick pattern which you input,
then put the '*' mark onto them. "
  (interactive)
  (mew-summary-or-thread
   (if (eq (point-min) (point-max))
       (message "No messages in this buffer.")
     (let ((fld (mew-summary-folder-name))
	   (preline 0) (i 0)
	   pattern msgs msgsback threadmsgs total linenum msgtotal)
       (mew-sinfo-set-find-key nil) ;; force to ask a user
       (setq pattern (mew-nmz-input-pick-pattern))
       (message "Namazu picking messages in %s..." fld)
       (save-excursion
	 (setq msgs (cdr (car (mew-nmz-multi-pick
			       (list (mew-nmz-expand-folder fld)) pattern))))
	 (message "Namazu picking messages in %s...done" fld)
	 (if (null msgs)
	     (message "No message to be marked.")
	   (setq msgsback msgs)
	   (setq msgtotal (length msgs))
	   (message "Marking messages... 0/%d" msgtotal)
	   ;; Thread folder
	   (when (mew-thread-p)
	     (setq threadmsgs (mew-summary-thread-get-msglst
			       (mew-vinfo-get-top) 'separator))
	     (setq total (length threadmsgs))
	     (goto-char (point-min))
	     (while (and msgs (not (eobp)))
	       (setq linenum (member (car msgs) threadmsgs))
	       (if (null linenum)
		   (setq msgsback (delete (car msgs) msgsback))
		 (setq linenum (- total (length linenum)))
		 (forward-line (- linenum preline))
		 (setq preline linenum)
		 (mew-summary-mark-as mew-mark-review))
	       (setq i (1+ i))
	       (when (= (% i 50) 0)
		 (message "Marking messages...%d/%d" i msgtotal))
	       (setq msgs (cdr msgs)))
	     (set-buffer-modified-p nil))
	   ;; Physical folder
	   (when (get-buffer fld)
	     (set-buffer fld)
	     (save-excursion
	       (goto-char (point-min))
	       (setq i 0)
	       (setq msgs msgsback)
	       (while (and msgs (not (eobp)))
		 (when (re-search-forward (mew-regex-jmp-msg (car msgs)) nil t)
		   (mew-summary-mark-as mew-mark-review)
		   (forward-line))
		 (setq i (1+ i))
		 (when (= (% i 50) 0)
		   (message "Marking messages...%d/%d" i msgtotal))
		 (setq msgs (cdr msgs)))
	       (set-buffer-modified-p nil)))
	   (message "Marking messages...%d/%d done" i msgtotal)))))))

;;
;; "Namazu virtual" function.
(defun mew-nmz-virtual ()
  "Another virtual mode with namazu."
  (interactive)
  (if (not (mew-summary-or-virtual-p))
      (message "This command cannot be used in this mode.")
    (let ((vfld (concat
		 "*"
		 (mew-input-string "Namazu virtual folder name %s(%s): "
				   "" ;; dummy
				   "vnamazu")))
	  (flds (mew-input-refile-folders (list (concat (mew-summary-folder-name) "*"))
					  t nil "+"))
	  (pattern (mew-nmz-input-pick-pattern))
	  (buf (generate-new-buffer mew-buffer-prefix))
	  (file (mew-make-temp-name))
	  nmzdirs fldmsgs fld msgs fasts scans count func lra)
      (if (null mew-nmz-indexed-folders)
	  (mew-nmz-gather-indexed-folder))
      (setq nmzdirs (mew-nmz-flds-to-indexs (or flds '("+*"))))
      (if (null nmzdirs)
	  (message "Please make namazu index.")
	(setq fldmsgs (mew-nmz-multi-pick nmzdirs pattern))
	(if (null fldmsgs)
	    (message "No message pick.")
	  (while fldmsgs
	    (if (and mew-nmz-use-fast-pick
		     (mew-nmz-folder-newp (car (car fldmsgs))))
		(setq fasts (cons (car fldmsgs) fasts))
	      (setq scans (cons (car fldmsgs) scans)))
	    (setq fldmsgs (cdr fldmsgs)))
	  (setq fasts (nreverse fasts))
	  (setq scans (nreverse scans))
	  ;;
	  (mew-summary-switch-to-folder vfld)
	  (mew-erase-buffer)
	  (setq count 1)
	  (when fasts
	    (while (setq fld (car (car fasts)))
	      (message "Namazu picking...(%s)" fld)
	      (setq pattern "")
	      (setq msgs (cdr (car fasts)))
	      (set-buffer buf)
	      (buffer-disable-undo (current-buffer))
	      (mew-erase-buffer)
	      (mew-frwlet
	       mew-cs-m17n mew-cs-dummy
	       (insert-file-contents
		(expand-file-name mew-summary-cache-file (mew-expand-folder fld))))
	      (goto-char (point-min))
	      (while (and (not (eobp)) msgs)
		(when (re-search-forward (concat "^ *" (car msgs) "[^0-9]") nil t)
		  (beginning-of-line)
		  (when (looking-at	;; new format "\r <msgid> <par-id> \006 folder msg"
			 "^ *\\([1-9][0-9]*\\)[^0-9]\\([^\r]*\r\\)\\([^\n]*\\)")
		    (setq pattern
			  ;; 123 .... \r  <msgid> <par-id> folder num
			  (concat pattern
				  (format "%5d %s%s \006 %s %s\n"
					  count
					  (match-string 2)	;; don't use match-string
					  (match-string 3)	;; for fancy-hightlight
					  fld (car msgs))))
		    (setq count (1+ count))))
		(setq msgs (cdr msgs))
		(forward-line 1))
	      (mew-summary-switch-to-folder vfld)
	      (goto-char (point-max))
	      (mew-elet
	       (insert pattern))
	      (setq fasts (cdr fasts))))
	  (if (null scans)
	      (message "Namazu picking...done")
	    (set-buffer buf)
	    (buffer-disable-undo (current-buffer))
	    (mew-erase-buffer)
	    (while (setq fld (car (car scans)))
	      (setq lra (cons (cons fld fld) lra))
	      (insert "CD: " fld "\n")
	      (setq msgs (cdr (car scans)))
	      (while msgs
		(insert (car msgs) "\n")
		(setq msgs (cdr msgs)))
	      (setq scans (cdr scans)))
	    (mew-frwlet
	     mew-cs-text-for-read mew-cs-text-for-write
	     (write-region (point-min) (point-max) file nil 'nomsg))
	    (setq scans t))
	  (mew-remove-buffer buf)
	  (mew-summary-switch-to-folder vfld)
	  (when scans
	    (setq func `(lambda () (when (file-exists-p ,file)
				     (delete-file ,file))))
	    (mew-local-retrieve 'vir (list "-i" file) count func lra 'noerase)))))))

;;
;; Use namazu-mode.
(add-hook 'namazu-mode-hook
	  (lambda ()
	    (define-key namazu-mode-map "m" 'mew-nmz-namazu-goto-mew)
	    (define-key namazu-mode-map "M" 'mew-nmz-namazu-return-mew)
	    (if (featurep 'xemacs)
		(define-key namazu-mode-map [(shift button2)] 'mew-nmz-namazu-view-at-mouse)
	      (define-key namazu-mode-map [S-mouse-2] 'mew-nmz-namazu-view-at-mouse))))

(defun mew-nmz-namazu (&optional arg)
  "Use namazu-mode from mew."
  (interactive "P")
  (mew-summary-only
   (if (not mew-nmz-use-namazu-el)
       (message "Please install \"namazu.el\".")
     (if (null mew-nmz-indexed-folders)
	 (mew-nmz-gather-indexed-folder))
     (setq mew-nmz-namazu-last-folder (mew-summary-folder-name 'ext))
     (if (or arg (not (mew-nmz-namazu-goto-namazu)))
	 (let ((flds (mew-input-folders (concat (mew-summary-folder-name) "*")))
	       (pattern (mew-nmz-input-pick-pattern))
	       fld nmzdirs dirlen)
	   (setq nmzdirs (mew-nmz-flds-to-indexs (or flds '("+*"))))
	   (setq dirlen (length nmzdirs))
	   (if (not (> dirlen mew-nmz-db-max))
	       ()
	     (message "Warning: assigned indexes over DB_MAX.")
	     (sit-for 1))
	   (and (not (fboundp 'namazu))
		(require 'namazu))
	   (namazu 0 (mew-join " " nmzdirs) pattern)
	   (cond
	    ((boundp 'namazu-history)
	     (setq namazu-history (cons pattern namazu-history)))
	    ((boundp 'namazu-keyword-history)
	     (setq namazu-keyword-history (cons pattern namazu-keyword-history))))
	   (namazu-jump-next)
	   (if mew-nmz-namazu-full-window (delete-other-windows)))))))

(defun mew-nmz-namazu-goto-mew ()
  (interactive)
  (let ((pattern "^\\(~?/.*\\)/\\([1-9][0-9]*\\) .*$")
	fld msg)
    (beginning-of-line)
    (if (not (re-search-forward pattern nil t))
	(namazu-view)
      (setq fld (match-string 1))
      (setq msg (match-string 2))
      (setq fld (mew-nmz-url-to-folder fld))
      (beginning-of-line)
      (mew-nmz-goto-folder-msg fld msg))))

(defun mew-nmz-namazu-return-mew ()
  (interactive)
  (if mew-nmz-namazu-last-folder
      (mew-summary-visit-folder mew-nmz-namazu-last-folder nil 'no-ls)))

(defun mew-nmz-namazu-goto-namazu ()
  (if (not (and (boundp 'namazu-buffer)
		namazu-buffer
		(get-buffer namazu-buffer)
		(buffer-name (get-buffer namazu-buffer))
		(pop-to-buffer namazu-buffer)))
      nil
    (if mew-nmz-namazu-full-window (delete-other-windows))
    t))

(defun mew-nmz-namazu-view-at-mouse (event)
  "Namazu's mouse interface for Mew."
  (interactive "e")
  (set-buffer (event-buffer event))
  (goto-char (event-point event))
  (let ((pos (point))
	(mew-mail-pattern
	 (concat "^\\("
		 (if (not mew-nmz-use-backslash)
		     (expand-file-name mew-mail-path)
		   (concat "/"
			   (substring (expand-file-name mew-mail-path) 0 1)
			   "|"
			   (substring (expand-file-name mew-mail-path) 2)))
		 "\\|~/Mail\\)/.*/[1-9][0-9]*"))
	pos-title pos-url)
    (end-of-line)
    (and (re-search-backward namazu-output-title-pattern nil t)
	 (setq pos-title (point))
	 (goto-char pos)
	 (re-search-forward namazu-output-title-pattern nil t)
	 (re-search-backward namazu-output-url-pattern nil t)
	 (> (point) pos-title)
	 (setq pos-url (point))
	 (setq pos (point)))
    (goto-char pos)
    (beginning-of-line)
    (and (not pos-url)
	 (re-search-forward namazu-output-url-pattern nil t)
	 (setq pos-url (point)))
    (goto-char pos)
    (cond
     ((and pos-title pos-url
	   (looking-at mew-mail-pattern))
      (mew-nmz-namazu-goto-mew))
     ((and pos-title pos-url)
      (namazu-view))
     ((and pos-url (> namazu-current-page 0))
      (namazu-prev-page))
     ((and pos-title (< namazu-current-page namazu-max-page))
      (namazu-next-page))
     (t (message "nothing to do.")))))

;;
;; Input "Namazu pattern" funcions.
(defun mew-nmz-input-pick-pattern ()
  "Input mew-nmz pick pattern."
  (mew-input-clear)
  (let ((mew-nmz-gather-header-list (mew-nmz-pick-pattern-gather-header)))
    (setq mew-input-complete-function (function mew-nmz-pick-pattern))
    (let ((keymap (copy-keymap mew-input-map)) pattern)
      (define-key keymap " " nil)
      (setq pattern (read-from-minibuffer "Namazu pick pattern: "
					  mew-nmz-pick-default-field
					  keymap
					  nil
					  'mew-nmz-pick-pattern-hist))
      (mew-decode-syntax-delete)
      ;; for M-n, M-p
      (setq mew-input-pick-pattern-hist
	    (cons pattern mew-input-pick-pattern-hist))
      pattern)))

(defun mew-nmz-pick-pattern-gather-header ()
  (when mew-nmz-pick-gather-field-list
    (save-excursion
      (let ((buf (mew-cache-hit
		  (mew-summary-folder-name) (mew-summary-message-number)))
	    (gathers mew-nmz-pick-gather-field-list)
	    retlst gather header duplchk mid addrs addr prefix)
	(when (and buf (get-buffer buf) (buffer-name (get-buffer buf)))
	  (set-buffer buf)
	  (while gathers
	    (setq gather (car gathers))
	    (setq header (mew-header-get-value (car gather)))
	    (when (and header (car (cdr gather)))
	      (cond
	       ((eq (car (cdr gather)) 'msgid)
		(while (and header (string-match "<\\([^>]+\\)>" header))
		  (setq mid (match-string 1 header))
		  (setq header (substring header (match-end 0)))
		  (if (member mid duplchk)
		      ()
		    (setq prefix (nthcdr 2 gather))
		    (setq duplchk (cons mid duplchk))
		    (while prefix
		      (setq retlst (cons (concat (car prefix) mid) retlst))
		      (setq prefix (cdr prefix))))))
	       ((eq (car (cdr gather)) 'address)
		(setq addrs (mew-addrstr-parse-address-list header))
		(while (setq addr (car addrs))
		  (setq addr (downcase addr))
		  (if (not (member addr duplchk))
		      (let ((prefix (nthcdr 2 gather)))
			(setq duplchk (cons addr duplchk))
			(while prefix
			  (setq retlst (cons (concat (car prefix) addr) retlst))
			  (setq prefix (cdr prefix)))))
		  (setq addrs (cdr addrs))))))
	    (setq gathers (cdr gathers)))
	  (when retlst
	    (setq retlst (append
			  retlst
			  (list
			   (concat " " (make-string (- (window-width) 10) ?-))))))
	  (nreverse retlst))))))

(defun mew-nmz-pick-pattern ()
  (let* ((pat (mew-delete-pattern))
	 (clist (append mew-nmz-pick-field-list
			mew-nmz-gather-header-list)))
    (if (null pat)
	(mew-complete-window-show clist)
      (mew-complete
       pat
       (mapcar (function list) clist)
       "Namazu pick pattern "
       nil))))

;;
;; "Namazu search engine" funcions.
(defun mew-nmz-multi-pick (nmzdirs pattern &optional catch)
  "Get message numbers with many folders."
  (let ((tmpdirs nmzdirs) nxt
	prog-args intmsgs retmsgs sortfld cell)
    (setq pattern (mew-cs-encode-arg pattern))
    (setq nmzdirs nil)
    (while tmpdirs
      (setq nxt (nthcdr mew-nmz-db-max tmpdirs))
      (if nxt (setcdr (nthcdr (1- mew-nmz-db-max) tmpdirs) nil))
      (setq nmzdirs (cons tmpdirs nmzdirs))
      (setq tmpdirs nxt))
    (setq nmzdirs (nreverse nmzdirs))
    (with-temp-buffer
      (buffer-disable-undo)
      (while (and nmzdirs
		  (or (not catch)
		      (and catch (null intmsgs))))
	(setq prog-args (append (if (eq (mew-nmz-namazu-version) 'v1)
				    (list "-aS")
				  (list "--all" "--list"))
				(list pattern)
				(car nmzdirs)))
	(erase-buffer)
	(mew-piolet
	 mew-cs-text-for-read mew-cs-text-for-write
	 (let ((file-name-coding-system nil))
	   (apply (function call-process)
		  mew-nmz-prog nil t nil prog-args)))
	(goto-char (point-min))
	(let ((msgregex (concat "^\\(.*\\)"
				(regexp-quote mew-path-separator)
				"\\([1-9][0-9]*\\)")) ;; ???
	      dir msgnum)
	  (while (not (eobp))
	    (if (not (looking-at msgregex))
		()
	      (setq dir (mew-buffer-substring (match-beginning 1) (match-end 1)))
	      (setq msgnum (string-to-int
			    (mew-buffer-substring (match-beginning 2) (match-end 2))))
	      (if (not (setq cell (assoc dir intmsgs)))
		  (setq intmsgs (cons (list dir (list msgnum)) intmsgs))
		(setq intmsgs (delete cell intmsgs))
		(setq cell (cons (car cell) (list (cons msgnum (car (cdr cell))))))
		(setq intmsgs (cons cell intmsgs))))
	    (forward-line))
	  (setq nmzdirs (cdr nmzdirs))))
      (if (null intmsgs)
	  nil
	(setq retmsgs intmsgs)
	(while retmsgs
	  (setq sortfld (cons (car (car retmsgs)) sortfld))
	  (setq retmsgs (cdr retmsgs)))
	(setq sortfld (sort sortfld 'string<))
	(while sortfld
	  (setq cell (assoc (car sortfld) intmsgs))
	  (setq retmsgs
		(cons
		 (cons (mew-nmz-url-to-folder (car cell))
		       (mapcar 'int-to-string
			       (sort (car (cdr cell)) '<)))
		 retmsgs))
	  (setq sortfld (cdr sortfld)))
	;; '((folder msg ...) (folder msg ...) ...)
	(nreverse retmsgs)))))

;;
;; miscellaneous functions
(defun mew-nmz-flds-to-indexs (flds)
  (let (nmzdirs fld)
    (while (setq fld (car flds))
      (cond
       ((string-match "^.*\\*$" fld)
	(setq nmzdirs (append nmzdirs
			      (mew-nmz-expand-folder-regexp fld))))
       ((member fld mew-nmz-indexed-folders)
	(setq nmzdirs (cons (mew-nmz-expand-folder fld) nmzdirs))))
      (setq flds (cdr flds)))
    (mew-uniq-list nmzdirs)))

(defun mew-nmz-goto-folder-msg (fld msg)
  (mew-summary-visit-folder fld)
  (while (processp mew-summary-buffer-process)
    (sit-for 1)
    (discard-input))
  (mew-summary-jump-message msg)
  (mew-summary-thread-move-cursor)
  (mew-summary-display 'force))

(defun mew-nmz-make-temp-name (prefix)
  (let ((time (current-time)))
    (setq time (mapconcat '(lambda (x)
			     (format "%d" x))
			  time ""))
    (concat prefix time)))

(if (fboundp 'unix-to-dos-filename)
    (defalias 'mew-nmz-slash-to-backslash 'unix-to-dos-filename)
  (defun mew-nmz-slash-to-backslash (dir)
    "Convert '/' to '\'."
    (if (string-match "\\\\" mew-path-separator)
	dir
      (let ((backslash "\\")
	    (pos 0))
	(while (string-match (regexp-quote mew-path-separator) dir pos)
	  (setq dir (concat (substring dir 0 (match-beginning 0))
			    backslash
			    (substring dir (match-end 0))))
	  (setq pos (1+ (match-end 0))))
	dir))))

(defconst mew-nmz-url-to-folder-regex
  (concat "^"
	  (regexp-quote
	   (file-name-as-directory (expand-file-name mew-mail-path)))
	  "\\(.*\\)$"))

(defun mew-nmz-url-to-folder (url)
  "Convert namazu's output url to folder."
  (and mew-nmz-use-backslash
       (string-match "^/\\([a-zA-Z]\\)|\\(/.*\\)" url)
       (setq url (concat
		  (substring url (match-beginning 1) (match-end 1))
		  ":"
		  (substring url (match-beginning 2) (match-end 2)))))
  (setq url (expand-file-name url))
  (if (string-match mew-nmz-url-to-folder-regex url)
      (concat "+" (substring url (match-beginning 1) (match-end 1)))
    (message "Ignore url(%s) in %s." url "NMZ.field.uri")))

(defun mew-nmz-index-newp (fld)
  (let ((touchtime (if mew-touch-folder-p
		       (mew-file-get-time
			(expand-file-name mew-summary-touch-file
					  (mew-expand-folder fld)))
		     (mew-file-get-time
		      (file-chase-links (mew-expand-folder fld)))))
	(stamptime (if mew-nmz-mknmz-use-timestamp
		       (mew-file-get-time
			(expand-file-name
			 "NMZ.stamp" (mew-nmz-expand-folder fld)))
		     (mew-file-get-time
		      (file-chase-links (mew-nmz-expand-folder fld))))))
    (cond
     ((null touchtime) nil)
     ((null stamptime) nil)
     ((> (nth 0 stamptime) (nth 0 touchtime)) t)
     ((and (= (nth 0 stamptime) (nth 0 touchtime))
	   (> (nth 1 stamptime) (nth 1 touchtime))) t)
     (t nil))))

(defun mew-nmz-folder-newp (fld)
  (let ((buf (get-buffer fld)))
    (if (not buf)
	(let ((dir (file-chase-links (mew-expand-folder fld)))
	      (tdir (if mew-touch-folder-p
			(mew-file-get-time
			 (expand-file-name mew-summary-touch-file
					   (mew-expand-folder fld)))
		      (mew-file-get-time
		       (file-chase-links (mew-expand-folder fld)))))
	      (tfile (mew-file-get-time
		      (expand-file-name mew-summary-cache-file
					(mew-expand-folder fld)))))
	  (if (and mew-touch-folder-p (null tdir)
		   (file-directory-p dir)
		   (mew-dir-messages (mew-expand-folder dir)))
	      nil
	    (cond
	     ((null tdir) t)
	     ((null tfile) nil)
	     ((> (car tdir) (car tfile)) nil)
	     ((= (car tdir) (car tfile))
	      (if (> (nth 1 tdir) (nth 1 tfile)) nil t)) ;; nil if same
	     (t t))))
      (set-buffer buf)
      (if (and (not (mew-summary-folder-dir-newp))
	       (mew-summary-exclusive-p))
	  t nil))))

(defun mew-nmz-expand-folder-regexp (fld)
  "Convert folder to namazu-index-dir with '*' expand."
  (if (null mew-nmz-indexed-folders)
      (mew-nmz-gather-indexed-folder))
  (let ((nmzflds mew-nmz-indexed-folders)
	nmzdirs tmpfld)
    (if (not (string-match "^\\([^*]+\\)\\*?$" fld))
	(setq fld "^\\+")
      (setq fld (match-string 1 fld))
      (setq fld (concat (regexp-quote (directory-file-name fld)))))
    (while (setq tmpfld (car nmzflds))
      (and (string-match fld tmpfld)
	   (setq nmzdirs
		 (cons (mew-nmz-expand-folder tmpfld) nmzdirs)))
      (setq nmzflds (cdr nmzflds)))
    (nreverse nmzdirs)))

(defun mew-nmz-gather-indexed-folder ()
  "Gather indexed folder."
  (interactive)
  (let ((flds (mapcar 'car mew-local-folder-alist))
	nmzdir nmzflds)
    (while flds
      (when (and (setq nmzdir (mew-nmz-expand-folder (car flds)))
		 (file-directory-p nmzdir)
		 (file-exists-p (expand-file-name "NMZ.i" nmzdir)))
	(setq nmzflds (cons (directory-file-name (car flds)) nmzflds)))
      (setq flds (cdr flds)))
    (setq mew-nmz-indexed-folders (nreverse nmzflds))
    (and (interactive-p) (message "Gather indexed folder...done"))))

(defun mew-nmz-index-delete (nmzdir)
  "Delete namazu index files."
  (if (file-directory-p nmzdir)
      (let ((flist (file-name-all-completions "NMZ\." nmzdir))
	    file)
	(while flist
	  (setq file (expand-file-name (car flist) nmzdir))
	  (and (file-exists-p file)
	       (file-writable-p file)
	       (delete-file file))
	  (setq flist (cdr flist))))))

(defun mew-nmz-skip-folder (fld)
  (let ((flds mew-nmz-mknmz-skip-folders))
    (catch 'match
      (while flds
	(if (string-match (concat "^" (car flds)) fld)
	    (throw 'match t))
	(setq flds (cdr flds)))
      nil)))

(defun mew-nmz-have-figure-folder (fld)
  (let ((flds mew-local-folder-alist)
	(regex (concat "^" fld "/[1-9][0-9]*/?$")))
    (catch 'match
      (while flds
	(if (string-match regex (car (car flds)))
	    (throw 'match t))
	(setq flds (cdr flds)))
      nil)))

(defun mew-nmz-timestamp-new (fld)
  (if mew-nmz-mknmz-use-timestamp
      (let ((file (expand-file-name "NMZ.stamp.new" (mew-nmz-expand-folder fld))))
	(if (file-writable-p file)
	    (write-region "touched by Mew." nil file nil 'no-msg)))))

(defun mew-nmz-timestamp-rename (fld)
  (if mew-nmz-mknmz-use-timestamp
      (let ((nfile (expand-file-name "NMZ.stamp.new" (mew-nmz-expand-folder fld)))
	    (tfile (expand-file-name "NMZ.stamp" (mew-nmz-expand-folder fld))))
	(if (and (file-readable-p nfile) (file-writable-p tfile))
	    (rename-file nfile tfile 'ok)
	  (if (file-writable-p nfile)
	      (delete-file nfile))))))

(provide 'mew-nmz)

;;; Copyright Notice:

;; Copyright (C) 1999-2001 Hideyuki SHIRAI <shirai@mew.org>
;; Copyright (C) 1994-2001 Mew developing team.
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

;;; mew-nmz.el ends here
