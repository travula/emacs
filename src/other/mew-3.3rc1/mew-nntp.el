;;; mew-nntp.el for reading

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew)

(defvar mew-nntp-msgid-file ".mew-msgid")
(defvar mew-nntp-folder-alist-file ".mew-folder-alist")
(defvar mew-nntp-folder-alist nil)
;; without mew-folder-nntp
(defvar mew-nntp-folder-alist2-file ".mew-folder-alist2")
(defvar mew-nntp-folder-alist2 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NNTP info
;;;

(defvar mew-nntp-info-list
  '("server" "port" "process" "ssh-process" "ssl-process" "status"
    "directive" "bnm"
    "rtrs" "refs" "range"
    "rttl" "rcnt" "hlds"
    "user"
    "size" "get-body" "no-msg" "case" "msgdb" "done"
    "newsgroup" "msgid" "truncated"))

(mew-info-defun "mew-nntp-" mew-nntp-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-nntp-fsm
  '(("greeting"    nil ("20[01]" . "mode-reader"))
    ("mode-reader" nil (t        . "authinfo"))
    ("authinfo"    nil ("381"    . "authpass"))
    ("authpass"    nil ("281"    . "group") (t . "wpwd"))
    ("group"       nil ("211"    . "xover"))
    ("xover"	   t   ("224"    . "pre-article"))
    ("article"     t   ("22[01]" . "post-article") (t . "next-article"))
    ("list"        t   ("215"    . "post-list"))
    ("pre-quit"    nil (t        . "quit2"))
    ("quit"        nil ("205"    . "noop"))))

(defsubst mew-nntp-fsm-by-status (status)
  (assoc status mew-nntp-fsm))

(defsubst mew-nntp-fsm-next (status code)
  (cdr (mew-assoc-match2 code (nthcdr 2 (mew-nntp-fsm-by-status status)) 0)))

(defsubst mew-nntp-fsm-reply (status)
  (nth 1 (mew-nntp-fsm-by-status status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters
;;;

(defun mew-nntp-secure-p (pnm)
  (or (mew-nntp-get-ssh-process pnm) (mew-nntp-get-ssl-process pnm)))

(defun mew-nntp-command-mode-reader (pro pnm)
  (mew-net-status (mew-nntp-get-bnm pnm)
		  "Connecting"
		  nil
		  (mew-nntp-secure-p pnm))
  (mew-nntp-process-send-string pro "MODE READER"))

(defun mew-nntp-command-authinfo (pro pnm)
  (let ((user (mew-nntp-get-user pnm)))
    (if user
	(mew-nntp-process-send-string pro "AUTHINFO USER %s" user)
      (mew-nntp-set-status pnm "group")
      (mew-nntp-command-group pro pnm))))

(defun mew-nntp-command-authpass (pro pnm)
  (let ((pass (mew-input-passwd "AUTHINFO: " (mew-nntp-passtag pnm))))
    (mew-nntp-process-send-string pro "AUTHINFO PASS %s" pass)))

(defun mew-nntp-command-wpwd (pro pnm)
  (mew-nntp-message pnm "AUTHINFO password is wrong!")
  (mew-passwd-set-passwd (mew-nntp-passtag pnm) nil)
  (mew-nntp-set-status pnm "pre-quit"))

(defun mew-nntp-command-group (pro pnm)
  (let ((directive (mew-nntp-get-directive pnm))
	(newsgroup (mew-nntp-get-newsgroup pnm)))
    (cond
     ((eq directive 'list)
      (mew-nntp-set-status pnm "list")
      (mew-nntp-command-list pro pnm))
     (t
      (mew-nntp-process-send-string pro "GROUP %s" newsgroup)))))

(defun mew-nntp-command-xover (pro pnm)
  (mew-net-status (mew-nntp-get-bnm pnm)
		  "Checking"
		  nil
		  (mew-nntp-secure-p pnm))
  (let (beg end)
    (if (re-search-forward "^[0-9]+ +[0-9]+ +\\([0-9]+\\) +\\([0-9]+\\) +[-.a-zA-Z0-9]+" nil t)
	(progn
	  (setq beg (mew-match-string 1))
	  (setq end (mew-match-string 2))
	  (mew-nntp-process-send-string pro "XOVER %s-%s" beg end))
      (mew-nntp-set-status pnm "quit")
      (mew-nntp-command-quit pro pnm))))

(defun mew-nntp-command-pre-article (pro pnm)
  (let* ((directive (mew-nntp-get-directive pnm))
	 (refs (mew-nntp-get-refs pnm))
	 ;; (uid siz del (+fld msg))
	 (bnm (mew-nntp-get-bnm pnm))
	 (range (mew-nntp-get-range pnm))
	 (old-uidl (mew-lisp-load (mew-expand-folder bnm mew-nntp-msgid-file)))
	 uid uidl siz rtr rtrs rttl hlds)
    (goto-char (point-min))
    ;; num subj from date msg-id ref siz lines
    (while (re-search-forward "^[0-9]+\t[^\t]*\t[^\t]*\t[^\t]*\t<\\([^\t]+\\)>\t[^\t]*\t\\([0-9]+\\)" nil t)
      (setq uid (mew-match-string 1))
      (setq siz (mew-match-string 2))
      (if (string= uid "") (setq uid nil))
      (cond
       ((eq directive 'get)
	(setq rtr (assoc uid refs))
	(if rtr (setq rtrs (cons rtr rtrs))))
       ((eq directive 'scan)
	(when uid
	  (setq uidl (cons uid uidl))
	  (when (or range ;; all, last:n
		    (not (member uid old-uidl))) ;; update
	    (setq rtr (list uid siz))
	    (setq rtrs (cons rtr rtrs)))))
       ((eq directive 'sync)
	(when uid
	  (setq hlds (cons uid hlds))))))
    (mew-nntp-set-msgid pnm uidl)
    ;; last:n
    (when (and (eq directive 'scan) (numberp range))
      (if (> (length rtrs) range)
	  (setcdr (nthcdr (1- range) rtrs) nil)))
    (setq rtrs (nreverse rtrs))
    (setq hlds (nreverse hlds))
    (mew-nntp-set-rtrs pnm rtrs)
    (mew-nntp-set-hlds pnm hlds)
    (setq rttl (length rtrs))
    (mew-nntp-set-rttl pnm (length rtrs))
    (cond
     ((eq directive 'sync)
      (mew-nntp-set-status pnm "quit")
      (mew-nntp-command-quit pro pnm))
     ((= rttl 0)
      (mew-nntp-message pnm "No new messages")
      (mew-nntp-set-status pnm "quit")
      (mew-nntp-command-quit pro pnm))
     ((= rttl 1)
      (mew-nntp-message pnm "Retrieving 1 message in background...")
      (mew-nntp-set-status pnm "article")
      (mew-nntp-command-article pro pnm))
     (t
      (mew-nntp-message pnm "Retrieving %d messages in background..." rttl)
      (mew-nntp-set-status pnm "article")
      (mew-nntp-command-article pro pnm)))))

(defun mew-nntp-command-article (pro pnm)
  (mew-net-status2 (mew-nntp-get-bnm pnm) 
		   (mew-nntp-get-rttl pnm)
		   (mew-nntp-get-rcnt pnm)
		   (nth 1 (car (mew-nntp-get-rtrs pnm)))
		   'zero
		   (mew-nntp-secure-p pnm))
  (let* ((directive (mew-nntp-get-directive pnm))
	 (rtrs (mew-nntp-get-rtrs pnm))
	 (rtr (car rtrs))
	 (uid (nth 0 rtr))
	 (siz (nth 1 rtr))
	 (lim (mew-nntp-get-size pnm))
	 (get-body (mew-nntp-get-get-body pnm)))
    (cond
     ((or (null rtr) (eq directive 'biff))
      (mew-nntp-set-truncated pnm nil)
      (mew-nntp-set-status pnm "quit")
      (mew-nntp-command-quit pro pnm))
     ((eq directive 'get)
      (mew-nntp-set-truncated pnm nil)
      (mew-nntp-process-send-string pro "ARTICLE <%s>" uid))
     ((and (eq directive 'scan) (not get-body))
      (mew-nntp-set-truncated pnm t)
      (mew-nntp-process-send-string pro "HEAD <%s>" uid))
     ((or (= lim 0) (<= (string-to-int siz) lim))
      (mew-nntp-set-truncated pnm nil)
      (mew-nntp-process-send-string pro "ARTICLE <%s>" uid))
     (t
      (mew-nntp-set-truncated pnm t)
      (mew-nntp-process-send-string pro "HEAD <%s>" uid)))))

(defun mew-nntp-command-post-article (pro pnm)
  (let* ((width (1- (mew-scan-width)))
	 (rtrs (mew-nntp-get-rtrs pnm))
	 (rtr (car rtrs))
	 (uid (nth 0 rtr))
	 (siz (nth 1 rtr))
	 (fld-msg (nth 3 rtr))
	 (truncated (mew-nntp-get-truncated pnm))
	 fld msg vec file msg-file lmsg)
    (cond
     ((null fld-msg)
      (setq fld (mew-nntp-get-bnm pnm)))
     ((stringp fld-msg)
      (setq fld fld-msg))
     ((listp fld-msg)
      (setq fld (nth 0 fld-msg))
      (setq msg (nth 1 fld-msg))
      (setq lmsg msg)))
    (goto-char (point-min))
    (forward-line)
    (delete-region (point-min) (point))
    ;; line delimiters
    (mew-eol-fix-for-read)
    (mew-dot-delete)
    (setq msg-file (mew-net-get-new-message
		    pnm fld msg 'mew-nntp-get-msgdb 'mew-nntp-set-msgdb))
    (setq msg (car msg-file) file (cdr msg-file))
    (goto-char (point-min))
    (if truncated
	(mew-header-insert-xmu uid siz t)
      (mew-header-insert-xmu uid siz nil))
    (catch 'write-error
      (condition-case nil
	  (mew-frwlet 
	   mew-cs-dummy mew-cs-text-for-write
	   (write-region (point-min) (point-max) file nil 'no-msg))
	(error
	 (mew-nntp-set-status pnm "quit")
	 (mew-nntp-command-quit pro pnm)
	 (throw 'write-error nil)))
      (mew-set-file-modes file)
      ;; xxx links?
      (mew-set-buffer-multibyte t)
      (setq vec (mew-nntp-scan-header))
      (mew-scan-set-folder vec fld)
      (mew-scan-set-message vec msg)
      (mew-set-buffer-multibyte nil)
      (mew-scan-insert-line fld vec width lmsg nil)
      (mew-nntp-command-next-article pro pnm))))

(defun mew-nntp-command-next-article (pro pnm)
  (let* ((rtrs (mew-nntp-get-rtrs pnm)))
    (mew-nntp-set-rcnt pnm (1+ (mew-nntp-get-rcnt pnm)))
    (mew-nntp-set-rtrs pnm (cdr rtrs))
    (mew-nntp-set-status pnm "article")
    (mew-nntp-command-article pro pnm)))

(defun mew-nntp-command-list (pro pnm)
  (mew-net-status (mew-nntp-get-bnm pnm)
		  "Listing"
		  nil
		  (mew-nntp-secure-p pnm))
  (mew-nntp-message pnm "Collecting newsgroup list...")
  (mew-nntp-process-send-string pro "LIST"))

(defun mew-nntp-command-post-list (pro pnm)
  (let ((case (mew-nntp-get-case pnm))
	group group2 groups groups2)
    (goto-char (point-min))
    (forward-line)
    (delete-region (point-min) (point))
    ;; line delimiters
    (mew-eol-fix-for-read)
    (mew-dot-delete)
    (while (not (eobp))
      (when (looking-at "\\([a-z][^ \t]+\\)")
	(setq group2 (mew-match-string 1))
	(setq group (concat mew-folder-nntp group2)) 
	(setq groups (cons (mew-folder-func group) groups))
	(setq groups2 (cons (mew-folder-func group2) groups2)))
      (forward-line))
    (if (null case) (setq case mew-case-default))
    (setq groups (nreverse groups))
    (mew-nntp-folder-save case groups groups2)
    (mew-nntp-folder-alist-set case groups)
    (mew-nntp-folder-alist2-set case groups2)
    (mew-nntp-set-status pnm "quit")
    (mew-nntp-command-quit pro pnm)))

(defun mew-nntp-command-quit (pro pnm)
  (mew-nntp-set-done pnm t)
  (mew-nntp-process-send-string pro "QUIT"))

(defun mew-nntp-command-quit2 (pro pnm)
  (mew-nntp-set-status pnm "quit")
  (mew-nntp-process-send-string pro "QUIT"))

(defun mew-nntp-command-noop (pro pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-nntp-info-prefix "mew-nntp-info-")

(defsubst mew-nntp-info-name (case newsgroup)
  (let ((server (mew-nntp-server case))
	(port (mew-nntp-port case))
	(sshsrv (mew-nntp-ssh-server case))
	(name mew-nntp-info-prefix))
    (setq name (concat name server "/" newsgroup))
    (unless (string= port mew-nntp-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defsubst mew-nntp-buffer-name (pnm)
  (concat mew-buffer-prefix pnm))

(defun mew-nntp-process-send-string (pro &rest args)
  (let ((str (apply 'format args)))
    (mew-nntp-debug "=SEND=" str)
    (process-send-string pro (concat str mew-cs-eol))))

(defun mew-nntp-passtag (pnm)
  (let ((server (mew-nntp-get-server pnm))
	(port (mew-nntp-get-port pnm))
	(user (mew-nntp-get-user pnm)))
    (concat user "@" server ":" port)))

(defun mew-nntp-scan-header ()
  (goto-char (point-min))
  (unless (re-search-forward mew-eoh nil t)
    (goto-char (point-max)))
  (mew-scan-header))

(defsubst mew-nntp-message (pnm &rest args)
  (or (mew-nntp-get-no-msg pnm) (apply 'message args)))

(defsubst mew-bnm-to-newsgroup (bnm)
  (let ((case-folder (mew-folder-case bnm))
	folder)
    (if case-folder
	(setq folder (cdr case-folder))
      (setq folder bnm))
    (mew-folder-string folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening NNTP
;;;

(defun mew-nntp-open (pnm server port no-msg)
  (let ((sprt (mew-port-sanity-check port))
	pro tm)
    (condition-case emsg
	(progn
	  (setq tm (mew-timer mew-nntp-timeout-time 'mew-nntp-timeout))
	  (or no-msg (message "Connecting to the NNTP server..."))
	  (setq pro (open-network-stream pnm nil server sprt))
	  (process-kill-without-query pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (or no-msg (message "Connecting to the NNTP server...done")))
      (quit
       (or no-msg (message "Cannot connect to the NNTP server"))
       (setq pro nil))
      (error
       (or no-msg (message "%s, %s" (nth 1 emsg) (nth 2 emsg)))
       (setq pro nil)))
    (if tm (cancel-timer tm))
    pro))

(defun mew-nntp-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-nntp-retrieve (case directive bnm &rest args)
  (let* ((server (mew-nntp-server case))
	 (port (mew-nntp-port case))
	 (sshsrv (mew-nntp-ssh-server case))
	 (sslp (mew-nntp-ssl case))
	 (sslport (mew-nntp-ssl-port case))
	 (newsgroup (mew-bnm-to-newsgroup bnm))
	 (pnm (mew-nntp-info-name case newsgroup))
	 (buf (get-buffer-create (mew-nntp-buffer-name pnm)))
	 (no-msg (eq directive 'biff))
	 process sshname sshpro sslname sslpro lport)
    (if (mew-nntp-get-process pnm)
	(message "Another NNTP process is running. Try later")
      (cond
       (sshsrv
	(setq sshpro (mew-open-ssh-stream server port sshsrv))
	(when sshpro
	  (setq sshname (process-name sshpro))
	  (setq lport (mew-ssh-pnm-to-lport sshname))
	  (when lport
	    (setq process (mew-nntp-open pnm "localhost" lport no-msg)))))
       (sslp
	(setq sslpro (mew-open-ssl-stream server sslport))
	(when sslpro
	  (setq sslname (process-name sslpro))
	  (setq lport (mew-ssl-pnm-to-lport sslname))
	  (when lport
	    (setq process (mew-nntp-open pnm "localhost" lport no-msg)))))
       (t
	(setq process (mew-nntp-open pnm server port no-msg))))
      (when process
	(mew-summary-lock process "NNTPing" (or sshpro sslpro))
	(mew-sinfo-set-scan-form (mew-summary-scan-form bnm))
	(mew-sinfo-set-scan-id nil)
	(mew-info-clean-up pnm)
	(mew-nntp-set-no-msg pnm no-msg) ;; must come here
	(mew-nntp-message pnm "Communicating with the NNTP server...")
	(mew-nntp-set-process pnm process)
	(mew-nntp-set-ssh-process pnm sshpro)
	(mew-nntp-set-ssl-process pnm sslpro)
	(mew-nntp-set-server pnm server)
	(mew-nntp-set-port pnm port)
	(mew-nntp-set-user pnm (mew-nntp-user case))
	(mew-nntp-set-status pnm "greeting")
	(mew-nntp-set-directive pnm directive)
	(mew-nntp-set-bnm pnm bnm)
	(mew-nntp-set-rcnt pnm 1)
	(mew-nntp-set-rttl pnm 0)
	(mew-nntp-set-size pnm (mew-nntp-size case))
	(mew-nntp-set-newsgroup pnm newsgroup)
	(mew-nntp-set-case pnm case)
	;;
	(cond
	 ((eq directive 'get)
	  (mew-nntp-set-refs pnm (nth 0 args)))
	 ((eq directive 'scan)
	  (mew-nntp-set-range pnm (nth 0 args))
	  (mew-nntp-set-get-body pnm (nth 1 args))
	  (if (mew-nntp-get-range pnm) (mew-net-folder-clean)))
	 ((eq directive 'sync)
	  ))
	(mew-sinfo-set-start-point (point)) ;; after erase-buffer
	(set-process-sentinel process 'mew-nntp-sentinel)
	(set-process-filter process 'mew-nntp-filter)
	(set-process-buffer process buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-nntp-debug (label string)
  (when (mew-debug 'net)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-nntp-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-nntp-get-status pnm))
	 (mulrep (mew-nntp-fsm-reply status))
	 stay next func code)
    (mew-nntp-debug (upcase status) string)
    (mew-filter
     ;; Process's buffer
     (goto-char (point-max))
     (mew-set-buffer-multibyte nil)
     (insert string)
     (when (string= status "article")
       (mew-net-status2 (mew-nntp-get-bnm pnm)
			(mew-nntp-get-rttl pnm)
			(mew-nntp-get-rcnt pnm)
			(nth 1 (car (mew-nntp-get-rtrs pnm)))
			nil
			(mew-nntp-secure-p pnm)))
     (cond
      ((and (and (goto-char (1- (point-max))) (looking-at "\n$"))
	    (and (goto-char (point-min)) (looking-at "^\\([45][0-9][0-9]\\)")))
       ;; this is an error code. this cannot be a multiple-line reply.
       (setq code (mew-match-string 1))
       (setq next (mew-nntp-fsm-next status code)))
      ((and (or (and mulrep
		     (goto-char (point-max))
		     (forward-line -1)
		     (looking-at "^\\.\r?$"))
		(and (not mulrep)
		     (goto-char (1- (point-max)))
		     (looking-at "\n$")))
	    (and (goto-char (point-min)) (looking-at "^\\([0-9][0-9][0-9]\\)")))
       (setq code (mew-match-string 1))
       (setq next (mew-nntp-fsm-next status code)))
      (t
       (setq stay t)))
     (unless stay
       (unless next (setq next "quit"))
       (mew-nntp-set-status pnm next)
       (setq func (intern-soft (concat "mew-nntp-command-" next)))
       (goto-char (point-min))
       (if (fboundp func)
	   (and func (funcall func process pnm))
	 (error "No function called %s" (symbol-name func)))
       (mew-erase-buffer)))))

(defun mew-nntp-sentinel (process event)
  (let* ((pnm (process-name process))
	 (directive (mew-nntp-get-directive pnm))
	 (sshpro (mew-nntp-get-ssh-process pnm))
	 (sslpro (mew-nntp-get-ssl-process pnm))
	 (rttl (mew-nntp-get-rttl pnm))
	 (bnm (mew-nntp-get-bnm pnm))
	 (hlds (mew-nntp-get-hlds pnm))
	 (msgid (mew-nntp-get-msgid pnm))
	 (done (mew-nntp-get-done pnm))
	 (file (mew-expand-folder bnm mew-nntp-msgid-file)))
    (mew-nntp-debug "NNTP SENTINEL" event)
    (mew-filter
     (set-buffer bnm)
     (when done
       (cond
	((eq directive 'list)
	 (mew-nntp-message pnm "Collecting newsgroup list...done"))
	((eq directive 'sync)
	 (mew-nntp-message pnm "Synchronizing messages...")
	 (mew-net-folder-sync bnm hlds)
	 (mew-nntp-message pnm "Synchronizing messages...done")
	 (mew-summary-folder-cache-save))
	((eq directive 'get)
	 (cond
	  ((= rttl 0)
	   (mew-nntp-message pnm "No new messages"))
	  ((= rttl 1)
	   (mew-nntp-message pnm "1 message retrieved"))
	  (t
	   (mew-nntp-message pnm "%d messages retrieved" rttl))))
	((eq directive 'scan)
	 (cond
	  ((= rttl 0)
	   (mew-nntp-message pnm "No messages scanned"))
	  ((= rttl 1)
	   (mew-nntp-message pnm "1 message scanned")
	   (mew-lisp-save file msgid)
	   (mew-summary-folder-cache-save))
	  (t
	   (mew-nntp-message pnm "%d messages scanned" rttl)
	   (mew-lisp-save file msgid)
	   (mew-summary-folder-cache-save))))))
     ;;
     (mew-net-status-clear (mew-nntp-get-bnm pnm))
     (mew-info-clean-up pnm)
     (set-buffer-modified-p nil)
     (mew-summary-unlock)
     (if (and (processp sshpro) (not mew-ssh-keep-connection))
	 (process-send-string sshpro "exit\n"))
     (if (and (processp sslpro) (not mew-ssl-keep-connection))
	 (delete-process sslpro))
     (run-hooks 'mew-nntp-sentinel-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Newsgroup alist
;;;

(defun mew-nntp-folder-clean-up ()
  (setq mew-nntp-folder-alist nil)
  (setq mew-nntp-folder-alist2 nil))

(defun mew-nntp-folder-alist (&optional case)
  (let ((ent (assoc (or case mew-case-default) mew-nntp-folder-alist)))
    (if ent
	(cdr ent)
      (mew-nntp-folder-load case))))

(defun mew-nntp-folder-alist2 (&optional case)
  (let ((ent (assoc (or case mew-case-default) mew-nntp-folder-alist2)))
    (if ent
	(cdr ent)
      (mew-nntp-folder-load case 'two))))

(defun mew-nntp-folder-load (case &optional two)
  (let* ((fld (mew-nntp-folder case))
	 (file (mew-expand-folder fld mew-nntp-folder-alist-file))
	 (groups (mew-lisp-load file))
	 (file2 (mew-expand-folder fld mew-nntp-folder-alist2-file))
	 (groups2 (mew-lisp-load file2)))
    (mew-nntp-folder-alist-set case groups)
    (mew-nntp-folder-alist2-set case groups2)
    (if two groups2 groups)))

(defun mew-nntp-folder-save (case groups groups2)
  (let* ((fld (mew-nntp-folder case))
	 (dir (mew-expand-folder fld))
	 (file (expand-file-name mew-nntp-folder-alist-file dir))
	 (file2 (expand-file-name mew-nntp-folder-alist2-file dir)))
    (mew-check-directory dir)
    (mew-lisp-save file groups 'nobackup 'unlimit)
    (mew-lisp-save file2 groups2 'nobackup 'unlimit)))

(defun mew-nntp-folder-alist-set (case groups)
  (setq mew-nntp-folder-alist
	(cons (cons (or case mew-case-default) groups)
	      (delete (assoc (or case mew-case-default) mew-nntp-folder-alist)
		      mew-nntp-folder-alist))))

(defun mew-nntp-folder-alist2-set (case groups)
  (setq mew-nntp-folder-alist2
	(cons (cons (or case mew-case-default) groups)
	      (delete (assoc (or case mew-case-default) mew-nntp-folder-alist2)
		      mew-nntp-folder-alist2))))

(defun mew-nntp-update ()
  (let ((bnm (mew-summary-folder-name 'ext))
	(case (mew-sinfo-get-case)))
    (if mew-config-cases
	(setq case (mew-input-case case "NNTP")))
    (mew-nntp-retrieve case 'list bnm)))

(provide 'mew-nntp)

;;; Copyright Notice:

;; Copyright (C) 1999-2003 Mew developing team.
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

;;; mew-nntp.el ends here
