;;; mew-summary4.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-summary-inbox-position (make-marker))
(defvar mew-last-shell-command "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Switching to folder
;;;

(defun mew-summary-switch-to-folder (folder &optional thread)
  (let ((ofolder (mew-summary-folder-name 'ext))
	case-folder new-folder)
    (cond
     ((get-buffer folder)
      (switch-to-buffer folder)
      (unless (mew-folder-virtualp folder) (mew-summary-folder-cache-load))
      (unless (string= ofolder folder) (mew-window-configure 'summary)))
     (t
      (setq new-folder t)
      (switch-to-buffer (get-buffer-create folder))
      (mew-buffers-setup folder)
      (if (null (setq case-folder (mew-folder-case folder)))
	  (mew-sinfo-set-folder folder)
	(mew-sinfo-set-case (car case-folder))
	(mew-sinfo-set-folder (cdr case-folder)))
      (if (mew-folder-virtualp folder)
	  (progn
	    (if thread (mew-vinfo-set-thread-p t))
	    (mew-virtual-mode))
	(mew-summary-mode)
	(if mew-summary-trace-directory (cd (mew-expand-folder folder)))
	(mew-summary-folder-cache-load))
      (mew-window-configure 'summary)))
    new-folder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to folder
;;;

(defun mew-summary-goto-folder (&optional goend)
  "Go to the folder which you specify.  If executed with
'\\[universal-argument]', the cursor always goes to the bottom of
Summary mode."
  (interactive "P")
  (let* ((proto (mew-proto-to-go (mew-summary-folder-name 'ext)))
	 (case (if (mew-folder-remotep proto)
		   (mew-sinfo-get-case) ;; a remote folder
		 mew-case-input)) ;; a local/virtual folder
	 (inbox (mew-proto-inbox-folder	proto case))
	 (folder (mew-input-folder case inbox))
	 buf win frame goend)
    (when folder
      (when mew-use-other-frame-for-summary
	(if (setq buf (get-buffer folder))
	    (if (setq win (get-buffer-window buf 0))
		(progn
		  (setq frame (window-frame win))
		  (raise-frame frame)
		  (select-frame frame)
		  ;; Ensure, if possible, that frame gets input focus.
		  (if (eq window-system 'w32)
		      (w32-focus-frame frame)
		    (when focus-follows-mouse
		      (set-mouse-position
		       (selected-frame) (1- (frame-width)) 0))))
	      (select-frame (make-frame)))
	  (select-frame (make-frame))))
      (mew-summary-visit-folder folder goend))))

(defun mew-summary-visit-folder (folder &optional goend no-ls)
  (let ((dir (mew-expand-folder folder))
	new-folder scanp)
    (cond
     ((mew-folder-virtualp folder)
      (if (get-buffer folder)
	  (if (mew-virtual-thread-p folder)
	      (if (mew-thread-cache-valid-p folder)
		  (mew-summary-switch-to-folder folder)
		(message "%s is old. " folder))
	    (mew-summary-switch-to-folder folder))
        (message "No such virtual folder: %s" folder)))
     (t ;; local/remote folders
      (if (null dir)
	  (message "Folder is wrong")
	(cond
	 ((mew-folder-remotep (mew-folder-folder folder))
	  (unless (file-directory-p dir)
	    (mew-make-directory dir))
	  (setq scanp t))
	 (t ;; +, ~, /, drive letter
	  (if (file-directory-p dir)
	      (setq scanp t)
	    (message "No such folder %s" folder))))
	(when scanp
	  (if no-ls
	      (progn
		(mew-summary-switch-to-folder folder) ;; Virtual mode
		(if goend (goto-char (point-max))))
	    (setq new-folder (mew-summary-switch-to-folder folder))
	    (mew-summary-ls nil (or goend new-folder)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to message
;;;

(defun mew-summary-jump-message (&optional msg)
  "Jump to a message according to the number which you input.
If 'mew-summary-jump-message-then-display' is non-nil,
the message is then displayed."
  (interactive "P")
  (if (integerp msg) (setq msg (int-to-string msg))
    (if (not (stringp msg)) (setq msg nil)))
  (let ((here (point)))
    (unless msg (setq msg (read-string "Message No.: " "")))
    (cond 
     ((string= msg "") ())
     ((eq msg t) ;; xxx
      (goto-char (point-max))) ;; (forward-line -1)
     (t 
      (goto-char (point-min))
      (if (not (re-search-forward (mew-regex-jmp-msg msg) nil t))
	  (goto-char here)
	(beginning-of-line)
	(if mew-summary-jump-message-then-display
	    (mew-summary-display nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to Message mode
;;;

(defun mew-summary-goto-msg-mode ()
  (interactive)
  (let (msgp)
    (save-excursion
      (set-buffer (window-buffer (next-window)))
      (if (mew-message-p) (setq msgp t)))
    (if msgp
	(other-window 1)
      (message "No Message mode displayed"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to +draft
;;;

(defun mew-max-draft-buffer ()
  (let* ((regex (mew-folder-regex (file-name-as-directory mew-draft-folder)))
	 (bufs (mew-buffer-list regex))
	 buf draft)
    (while bufs
      (setq buf (car bufs))
      (setq bufs (cdr bufs))
      (if (or (null draft) (string< draft buf))
	  (setq draft buf)))
    draft))

(defun mew-summary-jump-to-draft-buffer ()
  "Jump to one of drafts if exists."
  (interactive)
  (let* ((draft (mew-max-draft-buffer)) buf)
    (if (null draft)
	(message "No draft buffer exists!")
      (setq buf (mew-input-draft-buffer draft))
      (if (get-buffer buf)
	  (switch-to-buffer buf)
	(message "No such draft buffer!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to top and bottom
;;;

(defun mew-summary-jump-top ()
  "Go to the beginning of this Summary mode.
If 'mew-summary-jump-top-then-display' is non-nil, 
the top message is then displayed."
  (interactive)
  (goto-char (point-min))
  (if mew-summary-jump-top-then-display
      (mew-summary-display nil)))

(defun mew-summary-jump-bottom ()
  "Go to the end of this Summary mode.
If 'mew-summary-jump-bottom-then-display' is non-nil, 
the top message is then displayed."
  (interactive)
  (goto-char (point-max))
  (unless (bobp) (forward-line -1))
  (if mew-summary-jump-bottom-then-display
      (mew-summary-display nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Moving to the marker
;;;

(defun mew-summary-exchange-point ()
  "Get back to the position before typing '\\<mew-summary-mode-map>\\[mew-summary-retrieve]'."
  (interactive)
  (mew-summary-only
   (cond
    ((not (mew-folder-inboxp (mew-summary-folder-name 'ext)))
     (message "Cannot be used in this folder"))
    ((not (marker-position mew-summary-inbox-position))
     (message "No previous folder"))
    (t
     (let* ((buf (marker-buffer mew-summary-inbox-position))
	    (pos (marker-position mew-summary-inbox-position))
	    (folder (mew-summary-folder-name 'ext))
	    getback)
       (cond
	((equal (current-buffer) buf)
	 (goto-char pos))
	((not (get-buffer buf))
	 (message "Previous folder not exist"))
	((mew-folder-virtualp folder)
	 (if (mew-virtual-thread-p folder)
	     (if (mew-thread-cache-valid-p folder)
		 (setq getback t)
	       (message "%s is old. " folder))
	   (setq getback t)))
	(t
	 (setq getback t)))
       (when getback
	 (switch-to-buffer buf)
	 (goto-char pos)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Searching
;;;

(defun mew-summary-isearch-forward (&optional arg)
  "Incremental search forward in Message mode."
  (interactive "P")
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (widen)
	    (isearch-forward arg)
	    (mew-message-narrow-to-page))
	(select-window cwin)))))

(defun mew-summary-isearch-backward (&optional arg)
  "Incremental search backward in Message mode."
  (interactive)
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed")
      (select-window mwin)
      (unwind-protect
	  (progn
	    (widen)
	    (isearch-backward arg)
	    (mew-message-narrow-to-page))
	(select-window cwin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pipe
;;;

(defun mew-summary-pipe-message (prefix command)
  "Send the content of Message buffer to a command via pipe.
If called with '\\[universal-argument]', the body of the message
(excluding its header) is sent."
  (interactive
   (list current-prefix-arg 
	 (read-string "Shell command on message: " mew-last-shell-command)))
  (mew-summary-display 'redisplay)
  (when (y-or-n-p "Send this message to pipe? ")
    (save-excursion
      (set-buffer (mew-buffer-message))
      (save-restriction
	(widen)
	(if (string= command "") (setq command mew-last-shell-command))
	(goto-char (point-min)) ; perhaps this line won't be necessary
	(if prefix (search-forward "\n\n"))
	(let ((max-mini-window-height 1))
	  (shell-command-on-region (point) (point-max) command nil))
	(setq mew-last-shell-command command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print
;;;

(defun mew-summary-print (&optional arg)
  "Print the content of Message mode according to 'mew-print-function'.
If called with '\\[universal-argument]', you can specify a printer name."
  (interactive "P")
  (mew-summary-display 'redisplay)
  (when (y-or-n-p "Print this message? ")
    (let ((printer-name (if arg
			    (read-string "Printer name: ")
			  printer-name))
	  have-header str)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(save-restriction
	  (widen)
	  (setq have-header (mew-header-p))
	  (setq str (mew-buffer-substring (point-min) (point-max)))))
      (with-temp-buffer
	(insert str)
	(if have-header
	    (mew-header-delete-other-lines mew-field-for-printing))
	(funcall mew-print-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X-Face:
;;;

(defun mew-summary-x-face ()
  "Display xface."
  (interactive)
  (mew-summary-msg
   (let ((file (mew-make-temp-name)) xface)
     (save-excursion
       (set-buffer (mew-buffer-message))
       (setq xface (mew-header-get-value mew-x-face:)))
     (when xface
       (with-temp-buffer
	 (insert xface)
	 (mew-x-face-compface-to-xbm)
	 (mew-frwlet
	  mew-cs-dummy mew-cs-text-for-write
	  (write-region (point-min) (point-max) file nil 'no-msg)))
       (mew-mime-start-process
	mew-prog-image/*-ext mew-prog-image/*-ext-arg file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Calling a command
;;;

(defun mew-summary-cmd-msg ()
  "Executing an external command specifying this message as an
argument."
  (interactive)
  (mew-summary-msg-or-part
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (file (mew-expand-folder fld msg))
	  (command (read-string "Command: ")))
     (while (not (mew-which-exec command))
       (setq command (read-string "Command: ")))
     (message (format "Executing %s for %s..." command msg))
     (call-process command nil nil nil file)
     (message (format "Executing %s for %s...done" command msg)))))

(defun mew-summary-cmd-msgs ()
  "Executing an external command specifying messages
marked with '@' as arguments."
  (interactive)
  (mew-summary-multi-msgs
   (let ((command (read-string "Command: ")))
     (while (not (mew-which-exec command))
       (setq command (read-string "Command: ")))
     (message (format "Executing %s ..." command))
     (apply 'call-process command nil nil nil FILES)
     (message (format "Executing %s ...done" command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Citation
;;;

(defun mew-summary-cite (&optional arg)
  "Cite this message to one of drafts."
  (interactive "P")
  (let ((draft (mew-max-draft-buffer)) buf)
    (if (null draft)
	(message "No draft buffer exists!")
      (setq buf (mew-input-draft-buffer draft))
      (if (get-buffer buf)
	  (save-excursion
	    (set-buffer buf)
	    (mew-draft-cite arg))
	(message "No such draft buffer!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing external commands
;;;

(defun mew-summary-execute-external (&optional arg)
  "Execute an external command according to Content-Type:. 
If called with '\\[universal-argument]', Content-Type: is asked."
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-execute-base arg nil nil)))

(defun mew-summary-execute-command (&optional arg)
  "Execute an inputed command.
If called with '\\[universal-argument]', options are asked."
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-execute-base
    nil
    (read-string "Command: ")
    (if arg (delete "" (mew-split-quoted
			(read-string "Options: ") ?\  ?\" ?\"))))))

(defun mew-summary-execute-base (ask-type com args)
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number2))
	 (nums (mew-syntax-nums))
	 (cache (mew-cache-hit fld msg 'must-hit))
	 (syntax (mew-cache-decode-syntax cache))
	 (stx (mew-syntax-get-entry syntax nums))
	 (ctl (mew-syntax-get-ct stx))
	 (ct (mew-syntax-get-value ctl 'cap))
	 begin end params program fname cdpl options async doit pt fl
	 mew-inherit-ct)
    (if (not (string= ct mew-ct-msg))
	(setq doit t)
      ;; Message/Rfc822
      (setq stx (mew-syntax-get-part stx))
      (if (mew-syntax-multipart-p stx)
	  (progn
	    (setq stx (mew-syntax-get-entry stx '(1)))
	    (setq ctl (mew-syntax-get-ct stx))
	    (setq ct (mew-syntax-get-value ctl 'cap))
	    (if (mew-ct-textp ct)
		(setq doit t)))
	;; singlepart
	(setq ctl (mew-syntax-get-ct stx))
	(setq ct (mew-syntax-get-value ctl 'cap))
	(setq doit t)))
    (if (not doit)
	(message "No body")
      (setq begin   (mew-syntax-get-begin stx))
      (setq end     (mew-syntax-get-end stx))
      (setq params  (mew-syntax-get-params ctl))
      (setq cdpl    (mew-syntax-get-cdp stx))
      (setq fname   (mew-syntax-get-filename cdpl ctl))
      (when ask-type
	(setq mew-inherit-ct ct)
	(cond
	 (fname
	  (setq ct (or (mew-ctdb-ct (mew-ctdb-by-file fname))
		       (mew-content-type (mew-sinfo-get-case))))
	  (setq pt "Type for %s (%s): ")
	  (setq fl fname))
	 (t
	  (setq pt "Type %s(%s): ")
	  (setq fl "")))
	(setq ct (mew-input-type pt fl ct mew-mime-content-type-list)))
      (setq program (or com (mew-ctdb-prog (mew-ctdb-by-ct ct))))
      (cond
       ((and (listp program) (nth 1 program) (symbolp (nth 1 program)))
	(if (fboundp (nth 1 program))
	    (save-excursion
	      (if (or (mew-ct-imagep ct) (mew-ct-modelp ct))
		  (funcall (nth 1 program) cache begin end params fname)
		(funcall (nth 1 program) cache begin end params)))
	  (message (substitute-command-keys "Use '\\<mew-summary-mode-map>\\[mew-summary-execute-command]' or \\[universal-argument] \\<mew-summary-mode-map>\\[mew-summary-execute-external]'"))))
       (t
	(cond
	 ((listp program)
	  (setq options (nth 1 program))
	  (setq async (nth 2 program))
	  (setq program (nth 0 program)))
	 (t
	  (setq options args)
	  (setq async t)))
	(if (symbolp program)
	    (message "Function %s is not executable here" program)
	  (if (not (mew-which-exec program))
	      (message "Program %s is not found" program)
	    (let ((file (mew-make-temp-name fname))
		  rcs)
	      (save-excursion
		(set-buffer cache)
		;; NEVER use call-process-region for privacy reasons
		(if (mew-ct-linebasep ct)
		    (if (mew-ct-textp ct)
			(cond
			 ((string= mew-ct-htm ct)
			  (setq rcs (mew-text/html-detect-cs begin end ctl)))
			 ((string= mew-ct-xml ct)
			  (setq rcs (mew-text/xml-detect-cs begin end ctl)))
			 (t
			  (setq rcs (or (mew-charset-to-cs
					 (mew-syntax-get-param ctl "charset"))
					(if mew-decode-broken
					    (mew-charset-to-cs
					     (mew-charset-guess-region
					      begin end))
					  mew-cs-text-for-write)))))
		      (setq rcs mew-cs-text-for-write)) ;; xxx
		  (setq rcs mew-cs-binary))
		(mew-frwlet
		 mew-cs-dummy rcs
		 (write-region begin end file nil 'no-msg))
		(if async
		    (mew-mime-start-process program options file)
		  (mew-mime-call-process program options file)))))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flushing the queue
;;;

(defun mew-summary-send-message (&optional arg)
  "If in +queue, send the messages in +queue.
If in +postq, post the messages in +postq.
If in %queue, process the jobs in %queue.
Otherwise, flush the default queue.
If executed with '\\[universal-argument]', you can set the sending case."
  (interactive "P")
  (if (mew-folder-imap-queuep)
      (progn
	(mew-window-configure 'summary)
	(mew-imap-flush-queue))
    (let* ((fld (mew-summary-folder-name 'ext))
	   (proto (mew-proto-to-flush fld))
	   qfld case)
      (if (or arg mew-ask-flush-case)
	  (setq case (mew-input-case mew-case-output "Queue"))
	(setq case mew-case-output))
      (if (or (mew-folder-queuep fld) (mew-folder-postqp fld))
	  (progn
	    (setq qfld fld)
	    (mew-window-configure 'summary))
	(setq qfld (mew-proto-queue-folder proto case)))
      (if (and mew-ask-flush-queue
	       (not (y-or-n-p (concat "Flush " qfld  "? "))))
	  (message "The queue is not flushed")
	(cond
	 ((mew-folder-postqp qfld)
	  (mew-nntp2-flush-queue case qfld))
	 (t
	  (mew-smtp-flush-queue case qfld)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unlocking
;;;

(defun mew-summary-kill-subprocess (&optional kill-ssh)
  "\\<mew-summary-mode-map>
Kill a process in Summary mode.
Sometime a process accidentally remains in Summary mode. 
In this situation, you cannot execute '\\[mew-summary-retrieve]', '\\[mew-summary-ls]', nor '\\[mew-summary-exec]'.
Use this command to solve this problem.

If called with '\\[universal-argument]', corresponding SSH process, if
any, is also killed. Please note that the SSH process is shared by 
other network processes. In this situation, if you kill the SSH process,
the other network processes are also hung up."
  (interactive "P")
  (unwind-protect
      (let ((process mew-summary-buffer-process)
	    (disp-msg (interactive-p))
	    pnm ssh-process)
	(if (not (processp process))
	    (if disp-msg (message "No process to kill. This buffer is unlocked anyway"))
	  (cond
	   ((eq process t)
	    ;; Emacs is locking
	    )
	   ((memq (process-status process) '(open closed))
	    ;; Network process
	    (setq pnm (process-name process))
	    (setq ssh-process (mew-net-get-ssh-process pnm))
	    (mew-info-clean-up pnm)
	    (delete-process process)
	    (mew-summary-visible-buffer (current-buffer))
	    (when (mew-sinfo-get-refile-back)
	      (mew-sinfo-set-refile
	       (nconc (mew-sinfo-get-refile) (mew-sinfo-get-refile-back)))
	      (mew-sinfo-set-refile-back nil))
	    (if (and kill-ssh ssh-process) (kill-process ssh-process))
	    (mew-summary-unlock))
	   (t
	    ;; Local process or SSL
	    (kill-process process)))
	  (if disp-msg (message "The process was killed"))))
    (mew-summary-unlock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; De-composing with the old style
;;;

(defun mew-summary-unshar ()
  "Apply 'unshar' on messages marked with '@'."
  (interactive)
  (mew-summary-multi-msgs
   (if (y-or-n-p (format "Execute %s for these messages? " mew-prog-unshar))
       (let* ((default-directory (mew-summary-input-directory-name)))
	 (message "Executing %s..." mew-prog-unshar)
	 (apply 'call-process mew-prog-unshar nil nil nil FILES)
	 (message "Executing %s...done" mew-prog-unshar)))))

(defun mew-summary-uudecode ()
  "Apply 'uudecode' on messages marked with '@'."
  (interactive)
  (mew-summary-multi-msgs
   (cond
    ((not (mew-which-exec mew-prog-mime-decode))
     ())
    ((not (y-or-n-p "Uudecode these messages? "))
     ())
    (t
     (let ((dir (mew-summary-input-directory-name))
	   (fn nil)
	   (tmp (mew-make-temp-name))
	   (case-fold-search nil)
	   (files FILES) start)
       (with-temp-buffer
	 (mew-frwlet
	  mew-cs-text-for-read mew-cs-text-for-write
	  (while files
	    (mew-erase-buffer)
	    (insert-file (car files))
	    (goto-char (point-min))
	    (if (re-search-forward mew-eoh nil t)
		(forward-line))
	    (setq start (point))
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (write-region start (point-max) tmp 'append 'no-msg)
	    (setq files (cdr files))))
	 (mew-erase-buffer)
	 (cd dir)
	 ;; mew-folder-local is dummy to let mewencode use
	 ;; the embedded filiname
	 (call-process mew-prog-mime-decode tmp t nil
		       "-d" "-u" "-" mew-folder-local)
	 (mew-delete-file tmp)
	 (goto-char (point-min))
	 (if (not (looking-at "^filename: \\(.*\\)"))
	     (message "Failed to execute %s" mew-prog-mime-decode)
	   (setq fn (mew-match-string 1))
	   (setq fn (mew-summary-prog-exec mew-prog-compress "-df" "Z" fn))
	   (setq fn (mew-summary-prog-exec mew-prog-gzip "-df" "gz" fn))
	   (when (and (string-match "^\\(.*\\)\\.tar$" fn)
		      (y-or-n-p (format "Execute %s for %s? " mew-prog-tar fn)))
	     (message "Executing %s for %s..." mew-prog-tar fn)
	     (call-process mew-prog-tar nil nil nil "-xf" fn)
	     (message "Executing %s for %s...done" mew-prog-tar fn)))))))))

(defun mew-summary-prog-exec (prog opts suffix tarfile)
  (if (string-match (format "^\\(.*\\)\\.%s$" suffix) tarfile)
      (let ((basename (match-string 1 tarfile)))
	(if (not (y-or-n-p (format "Execute %s for %s? " prog tarfile)))
	    tarfile
	  (message "Executing %s for %s..." prog tarfile)
	  (call-process prog nil nil nil opts tarfile)
	  (message "Executing %s for %s...done" prog tarfile)
	  basename))
    tarfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; De-composing with the new style
;;;

(defun mew-summary-join ()
  "Concat Message/Partial fragments marked with '@' to an original
message."   (interactive)
  (mew-summary-only
   (mew-summary-with-mewls
    (mew-summary-multi-msgs
     (let ((cfld (mew-summary-folder-name))
	   (folder (mew-input-burst-folder))
	   (tfile (mew-make-temp-name))
	   (targets FLD-MSG-LIST)
	   fld-msg num ct med dbl vec len idx ttl TTL fid FID i ent
	   beg regex)
       (message "Joining...")
       (with-temp-buffer
	 (mew-set-buffer-multibyte t)
	 (insert "CD: " cfld "\n")
	 (while targets
	   (setq fld-msg (car targets))
	   (setq targets (cdr targets))
	   (insert (cdr fld-msg) "\n"))
	 (mew-frwlet
	  mew-cs-text-for-read mew-cs-text-for-write
	  (write-region (point-min) (point-max) tfile nil 'no-msg))
	 (mew-erase-buffer)
	 (call-process mew-prog-mewls nil t nil
		       "-b" mew-mail-path "-l" "0"
		       "-i" tfile "-d" "content-type")
	 (mew-delete-file tfile)
	 (goto-char (point-min))
	 (while (not (eobp))
	   (if (not (looking-at "^\\([0-9]+\\)[ \t]*:[ \t]*"))
	       (forward-line)
	     (setq num (mew-match-string 1))
	     (setq med (match-end 0))
	     (forward-line)
	     (mew-header-goto-next)
	     (setq ct (mew-param-decode
		       (mew-buffer-substring med (1- (point)))))
	     (setq dbl (cons (cons num ct) dbl))))
	 (setq len (length dbl))
	 (setq vec (make-vector len nil))
	 (while dbl
	   (setq ent (car dbl))
	   (setq dbl (cdr dbl))
	   (setq num (car ent))
	   (setq ttl (mew-syntax-get-param ent "total"))
	   (setq fid (mew-syntax-get-param ent "id"))
	   (setq idx (mew-syntax-get-param ent "number"))
	   (if TTL
	       (if (and ttl (not (string= TTL ttl)))
		   (error "total mismatch"))
	     (setq TTL ttl))
	   (if FID
	       (if (or (null fid) (not (string= FID fid)))
		   (error "fragment id mismatch"))
	     (setq FID fid))
	   (unless idx (error "no number"))
	   (setq idx (1- (string-to-int idx)))
	   (if (and (>= idx 0) (< idx len))
	       (aset vec idx num)
	     (error "illegal number")))
	 (unless TTL (error "no total"))
	 (setq i 0)
	 (while (< i len)
	   (unless (stringp (aref vec i)) (error "Not enough fragments"))
	   (setq i (1+ i)))
	 ;; now reassemble
	 (mew-erase-buffer)
	 (mew-frwlet
	  mew-cs-text-for-read mew-cs-text-for-write
	  ;; the first fragment
	  (goto-char (point-max))
	  (save-restriction
	    (narrow-to-region (point) (point))
	    (insert-file-contents (mew-expand-folder cfld (aref vec 0)))
	    ;; Removing unnecessary fields from the encapsulating
	    ;; (outer) header.
	    (goto-char (point-min))
	    (mew-header-delete-lines mew-field-delete-for-joining)
	    ;; Concatenating the two headers.
	    (goto-char (point-min))
	    (re-search-forward mew-eoh nil t)
	    (setq beg (point))
	    (while (looking-at mew-eoh) (forward-line))
	    (delete-region beg (point))
	    ;; Removing unnecessary fields from the encapsulated
	    ;; (inner) header.
	    (setq beg (point))
	    (when (re-search-forward mew-eoh nil t)
	      (setq regex (mew-make-field-regex mew-field-delete-for-joining))
	      (save-restriction
		(narrow-to-region beg (point))
		(goto-char (point-min))
		(let ((case-fold-search t))
		  (while (not (eobp))
		    (if (looking-at regex)
			(setq beg nil) ;; logic is reversed
		      (setq beg (point)))
		    (forward-line)
		    (mew-header-goto-next)
		    (if beg (delete-region beg (point))))))))
	  ;; the second and subsequent fragments.
	  (setq i 1)
	  (while (< i len)
	    (goto-char (point-max))
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (insert-file-contents (mew-expand-folder cfld (aref vec i)))
	      (goto-char (point-min))
	      (re-search-forward mew-eoh nil t)
	      (forward-line)
	      (delete-region (point-min) (point)))
	    (setq i (1+ i)))
	  (write-region (point-min) (point-max)
			(mew-folder-new-message folder)
			nil 'no-msg)))
       (mew-touch-folder folder)
       (message "Joining...done")
       (if (y-or-n-p (format "Go to %s? " folder))
	   (mew-summary-visit-folder folder 'goend)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning +trash
;;;

(defun mew-summary-clean-trash (&optional arg)
  "Remove all messages in +trash.
When called with '\\[universal-argument]', you can specify a target folder
to clean up."
  (interactive "P")
  (let* ((folder mew-trash-folder)
	 trashdir msgs)
    (if arg (setq folder (mew-input-folder nil mew-trash-folder)))
    (when folder
      (setq trashdir (mew-expand-folder folder))
      (setq msgs (mew-dir-messages trashdir))
      (if (null msgs)
	  (message "No messages to be removed in %s" folder)
	(when (y-or-n-p (format "Remove all messages in %s? " folder))
	  (message "Removing all messages in %s..." folder)
	  (mew-summary-unlink-msgs folder msgs)
	  (mew-summary-folder-cache-clean folder)
	  (message "Removing all messages in %s...done" folder))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copy local
;;;

(defun mew-summary-local-copy ()
  "Copy a cached message in a remote folder to a local folder."
  (interactive)
  (mew-summary-msg-or-part
   (let ((fld (mew-summary-folder-name))
	 (exclusivep t)
	 msg dstmsg dstfld file dstfile)
     (save-excursion
       (mew-summary-goto-message)
       (setq msg (mew-summary-message-number)))
     (setq file (mew-expand-folder fld msg))
     (setq dstfld (mew-input-local-folder mew-inbox-folder))
     (when (and dstfld (mew-local-folder-check dstfld 'ask))
       (setq dstmsg (mew-folder-new-message dstfld 'numonly))
       (setq dstfile (mew-expand-folder dstfld dstmsg))
       (when (get-buffer dstfld)
	 (set-buffer dstfld)
	 (setq exclusivep (mew-summary-exclusive-p)))
       (cond
	((not exclusivep)
	 (message "Try again later"))
	((string= file dstfile)
	 (message "You cannot copy this onto itself"))
	(t
	 (mew-summary-local-copy-one file dstfile)
	 (mew-touch-folder dstfld)
	 (message "Copied to %s/%s" dstfld dstmsg)))))))

(defun mew-summary-mark-local-copy (&optional arg)
  "Copy cached message marked with '*' in a remote folder to a local
folder."
  (interactive "P")
  (mew-summary-not-in-draft
   (let ((mew-use-highlight-x-face nil)
	 (fld (mew-summary-folder-name))
	 (exclusivep t)
	 msgs file dstmsg dstfld dstfile
	 beg end region)
     (cond
      (arg
       (setq region (mew-summary-get-region))
       (setq beg (car region))
       (setq end (cdr region)))
      (t
       (setq beg (point-min))
       (setq end (point-max))))
     (setq dstfld (mew-input-local-folder mew-inbox-folder))
     (when (and dstfld (mew-local-folder-check dstfld 'ask))
       (setq dstmsg (mew-folder-new-message dstfld 'numonly))
       (setq msgs (mew-summary-mark-collect mew-mark-review beg end))
       (when (get-buffer dstfld)
	 (save-excursion
	   (set-buffer dstfld)
	   (setq exclusivep (mew-summary-exclusive-p))))
       (cond
	((null msgs)
	 (message "No mark"))
	((not exclusivep)
	 (message "Try again later"))
	(t
	 (message "Copying...")
	 (while msgs
	   (setq file (mew-expand-folder fld (car msgs)))
	   (setq dstfile (mew-expand-folder dstfld dstmsg))
	   (mew-summary-local-copy-one file dstfile)
	   (setq dstmsg (number-to-string (1+ (string-to-number dstmsg))))
	   (setq msgs (cdr msgs)))
	 (mew-touch-folder dstfld)
	 (message "Copying...done")))))))

(defun mew-summary-local-copy-one (srcfile dstfile)
  (with-temp-buffer
    (mew-frwlet
     mew-cs-text-for-read mew-cs-text-for-write
     (insert-file-contents srcfile)
     (goto-char (point-min))
     (mew-header-delete-lines (list mew-x-mew-uidl:))
     (write-region (point-min) (point-max) dstfile nil 'no-msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary only vs Summary/Message
;;;

(defun mew-summary-toggle-disp-msg (&optional arg)
  "Toggle 'Summary mode only' and 'Summary & Message mode'. If 
you choose 'Summary mode only', you can quickly put the delete
	marks since the next message is not displayed."
  (interactive)
  (cond 
   ((eq arg 'on)
    (mew-sinfo-set-disp-msg t))
   ((eq arg 'off)
    (mew-sinfo-set-disp-msg nil)
    (mew-summary-reset-mode-line))
   (t
    (mew-sinfo-set-disp-msg (not (mew-sinfo-get-disp-msg)))
    (if (mew-sinfo-get-disp-msg)
	(mew-summary-display 'redisplay)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      (mew-window-configure 'summary)
      (mew-current-set nil nil nil)
      (mew-summary-cook-window)
      (mew-summary-reset-mode-line))))
  (run-hooks 'mew-summary-toggle-disp-msg-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 8bit clean
;;;

(defun mew-summary-toggle-8bit ()
  "Toggle 8bit mode(i.e. 'mew-use-8bit')."
  (interactive)
  (setq mew-use-8bit (not mew-use-8bit))
  (if mew-use-8bit
      (message "mew-use-8bit has been set to 't'")
    (message "mew-use-8bit has been set to 'nil'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mew cache clean up
;;;

(defun mew-summary-cache-clean-up ()
  "Clean-up caches of analyzed messages."
  (interactive)
  (mew-cache-clean-up)
  (message "Mew cache clean up...done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle warning
;;;

(defun mew-summary-toggle-warning ()
  "Toggle waring level.
If 'mew-warning-field-level' is 2, set it to 1.
If 'mew-warning-field-level' is 1, set it to 2."
  (interactive)
  (if (= mew-warning-field-level 2)
      (setq mew-warning-field-level 1)
    (setq mew-warning-field-level 2))
  (mew-cache-clean-up)
  (mew-summary-analyze-again)
  (message "Mew warning level %d" mew-warning-field-level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle decode policy
;;;

(defun mew-summary-toggle-policy (&optional arg)
  "Toggle decode policy(i.e. 'mew-decode-broken')."
  (interactive "P")
  (if arg
      (setq mew-decode-broken nil
	    mew-use-name-parameter nil)
    (setq mew-decode-broken (not mew-decode-broken))
    (setq mew-use-name-parameter (not mew-use-name-parameter)))
  (mew-cache-clean-up)
  (mew-summary-analyze-again)
  (message "Mew \"%s\" mode" (if mew-decode-broken "TOLERANT" "STRICT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toggle debug
;;;

(defun mew-summary-toggle-debug ()
  "Toggle 'mew-debug'."
  (interactive)
  (setq mew-debug (not mew-debug))
  (if mew-debug
      (message "'mew-debug' is now 't'")
    (message "'mew-debug' is now 'nil'")))

(provide 'mew-summary4)

;;; Copyright Notice:

;; Copyright (C) 1996-2003 Mew developing team.
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

;;; mew-summary4.el ends here
