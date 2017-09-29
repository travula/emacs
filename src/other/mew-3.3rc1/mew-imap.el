;;; mew-imap.el for reading

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb 21, 2002

;;; Code:

(require 'mew)

(defvar mew-imap-msgid-file ".mew-msgid")
(defvar mew-imap-folder-alist-file ".mew-folder-alist")
(defvar mew-imap-folder-alist nil)
(defvar mew-imap-friend-folder-list-file ".mew-friend-folder-list")
(defvar mew-imap-friend-folder-list nil)

(defvar mew-imap-skip-uidl t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAP info
;;;

(defvar mew-imap-info-list
  '("server" "port" "process" "ssh-process" "ssl-process" "status"
    "directive" "bnm"
    "rtrs" "dels" "refs" "rmvs" "kils" "uidl" "range"
    "rttl" "rcnt" "dttl" "dcnt" "jcnt" "rfl" "hlds"
    "user" "auth" "auth-list" "passwd"
    "size" "truncated" "get-body"
    "flush" "no-msg" "msgdb" "done"
    "delete"
    "case" "mailbox" "msgid" "max" "tag"
    "wrk" "jobs"))

(mew-info-defun "mew-imap-" mew-imap-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-imap-fsm
  '(("greeting"      ("OK" . "capability"))
    ("capability"    ("OK" . "authentication"))
    ("auth-cram-md5" ("OK" . "pwd-cram-md5") ("NO" . "wpwd"))
    ("pwd-cram-md5"  ("OK" . "select") ("NO" . "wpwd"))
    ("auth-login"    ("OK" . "user-login") ("NO" . "wpwd"))
    ("user-login"    ("OK" . "pwd-login") ("NO" . "wpwd"))
    ("pwd-login"     ("OK" . "select") ("NO" . "wpwd"))
    ("login"         ("OK" . "select") ("NO" . "wpwd"))
    ("select"        ("OK" . "uid"))
    ("uid"           ("OK" . "umsg"))
    ;; xxx what if "NO", not "NO [TRYCREATE]"?
    ("copy"          ("OK" . "copy") ("\\[TRYCREATE\\]" . "create"))
    ("create"        ("OK" . "copy") ("NO" . "wmbx"))
    ("dels"	     ("OK" . "dels"))
    ("expunge"       ("OK" . "logout"))
    ;;
    ("fetch"         ("OK" . "post-fetch"))
    ;;
    ("list"          ("OK" . "post-list"))
    ;;
    ("logout"        ("OK" . "noop"))))

(defsubst mew-imap-fsm-by-status (status)
  (assoc status mew-imap-fsm))

(defsubst mew-imap-fsm-next (status code)
  (cdr (mew-assoc-match2 code (nthcdr 1 (mew-imap-fsm-by-status status)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters
;;;

(defun mew-imap-secure-p (pnm)
  (or (mew-imap-get-ssh-process pnm) (mew-imap-get-ssl-process pnm)))
 
(defun mew-imap-command-capability (pro pnm)
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Auth'ing"
		  nil
		  (mew-imap-secure-p pnm))
  (mew-imap-process-send-string pro pnm "CAPABILITY"))

(defun mew-imap-command-authentication (pro pnm)
  (cond
   ((eq (mew-imap-get-auth pnm) t) ;; t means SASL
    (let ((auth-list (mew-imap-auth-list pnm))
	  authl auth func)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "AUTH=\\([^\n\r ]+\\)" nil t)
	  (setq authl (cons (mew-match-string 1) authl))))
      (if (and authl
	       (setq auth (mew-auth-select2 authl auth-list))
	       (setq func (mew-imap-auth-get-func auth))
	       (fboundp func))
	  (progn
	    (mew-imap-set-auth pnm auth)
	    (funcall func pro pnm))
	(mew-imap-debug "<AUTH>" "No preferred IMAP AUTH.\n")
	(mew-imap-set-status pnm "login")
	(mew-imap-command-login pro pnm))))
   (t
    (mew-imap-set-status pnm "login")
    (mew-imap-command-login pro pnm))))

(defun mew-imap-command-login (pro pnm)
  (let ((user (mew-imap-get-user pnm))
	(pass (mew-input-passwd "IMAP password: " (mew-imap-passtag pnm))))
    (setq user (mew-quote-string user ?\\ '(?\\ ?\")))
    (setq pass (mew-quote-string pass ?\\ '(?\\ ?\")))
    (mew-imap-process-send-string pro pnm "LOGIN \"%s\" \"%s\"" user pass)))

(defun mew-imap-command-wpwd (pro pnm)
  (let* ((directive (mew-imap-get-directive pnm))
	 (bnm (mew-imap-get-bnm pnm)))
    (mew-imap-message pnm "IMAP password is wrong!")
    (mew-passwd-set-passwd (mew-imap-passtag pnm) nil)
    (when (eq directive 'exec)
      (mew-summary-visible-buffer bnm)
      (save-excursion
	(set-buffer bnm)
	(mew-sinfo-set-refile
	 (nconc (mew-sinfo-get-refile) (mew-sinfo-get-refile-back)))
	(mew-sinfo-set-refile-back nil)))
    (mew-imap-command-logout2 pro pnm)))

(defun mew-imap-command-select (pro pnm)
  (let ((directive (mew-imap-get-directive pnm))
	(mailbox (mew-imap-get-mailbox pnm)))
    (cond
     ((eq directive 'list)
      (mew-imap-set-status pnm "list")
      (mew-imap-command-list pro pnm))
     (t
      (mew-imap-process-send-string pro pnm "SELECT \"%s\"" mailbox)))))

(defun mew-imap-command-uid (pro pnm)
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Checking"
		  nil
		  (mew-imap-secure-p pnm))
  (let ((directive (mew-imap-get-directive pnm))
	(refs (mew-imap-get-refs pnm)) ;; (uid siz del (+fld msg))
	(rmvs (mew-imap-get-rmvs pnm))
	(bnm (mew-imap-get-bnm pnm))
	(range (mew-imap-get-range pnm))
	max)
    (if (and mew-imap-skip-uidl (or (eq directive 'exec) (eq directive 'get)))
	(mew-imap-command-dispatch pro pnm directive refs rmvs nil)
      (cond
       ((eq directive 'biff) ;; xxx
	(setq max (mew-lisp-load (mew-expand-folder bnm mew-imap-msgid-file))))
       ((eq directive 'scan)
	(if (eq range nil) ;; update
	    (setq max (mew-lisp-load
		       (mew-expand-folder bnm mew-imap-msgid-file))))))
      (mew-imap-set-max pnm max)
      (if max
	  (setq max (int-to-string (1+ (string-to-int max))))
	(setq max "1"))
      (mew-imap-process-send-string
       pro pnm "UID FETCH %s:* (UID RFC822.SIZE)" max))))

(defun mew-imap-command-umsg (pro pnm)
  (let* ((directive (mew-imap-get-directive pnm))
	 (max (mew-imap-get-max pnm))
	 (refs (mew-imap-get-refs pnm)) ;; (uid siz del (+fld msg))
	 (rmvs (mew-imap-get-rmvs pnm))
	 (del-time (mew-imap-get-delete pnm))
	 (range (mew-imap-get-range pnm))
	 (ctime (current-time))
	 rtr rtrs dels uid siz uidl old-uidl uid-time hlds)
    (if (eq directive 'inc)
	(setq old-uidl (mew-net-uidl-db-get (mew-imap-passtag pnm))))
    (setq max (if max (string-to-int max)))
    (goto-char (point-min))
    (while (re-search-forward "^\\* [0-9]+ FETCH (UID \\([0-9]+\\) RFC822.SIZE \\([0-9]+\\))" nil t)
      (setq uid (mew-match-string 1))
      (setq siz (mew-match-string 2))
      (cond
       ((or (eq directive 'get) (eq directive 'exec))
	(cond
	 ((setq rtr (assoc uid refs))
	  (setq rtrs (cons rtr rtrs)))
	 ((member uid rmvs)
	  (setq dels (cons uid dels)))))
       ((eq directive 'biff)
	(when uid
	  (if (or (null max) (> (string-to-int uid) max))
	      (setq rtrs (cons (list uid siz) rtrs)))))
       ((eq directive 'scan)
	(when uid
	  (if (or range ;; all, last:n
		  (null max) (> (string-to-int uid) max)) ;; update
	      (setq rtrs (cons (list uid siz) rtrs)))))
       ((eq directive 'sync)
	(when uid
	  (setq hlds (cons uid hlds))))
       ((eq directive 'inc)
	(if uid (setq uid-time (cdr (assoc uid old-uidl))))
	(cond
	 (uid-time
	  (setq uidl (cons (cons uid uid-time) uidl))
	  (if (mew-expired-p uid-time del-time)
	      (setq dels (cons uid dels))))
	 (t
	  (setq uidl (cons (cons uid ctime) uidl))
	  (if (eq del-time t)
	      (setq dels (cons uid dels)))
	  (setq rtr (list uid siz))
	  (setq rtrs (cons rtr rtrs)))))))
    (mew-imap-set-msgid pnm (nth 0 (car rtrs)))
    ;; last:n
    (when (and (eq directive 'scan) (numberp range))
      (if (> (length rtrs) range)
	  (setcdr (nthcdr (1- range) rtrs) nil)))
    (setq rtrs (nreverse rtrs))
    (setq dels (nreverse dels))
    (setq hlds (nreverse hlds))
    (mew-imap-command-dispatch pro pnm directive rtrs dels hlds)))

(defun mew-imap-command-dispatch (pro pnm directive rtrs dels hlds)
  (cond
   ((eq directive 'exec)
    ;; '((uid1 siz1 t   dst1 dst2)
    ;;   (uid2 siz2 t   dst1)
    ;;   (uid3 siz3 nil dst1 dst2)
    ;;   (uid4 siz4 nil dst3))
    ;; =>
    ;; '((dst1 uid1 uid2)
    ;;   (dst2 uid1 uid2 uid3)
    ;;   (dst3 uid4))
    ;; dels2 = (uid1 uid2)
    ;; =>
    ;; '((dst1 uid1,uid2)
    ;;   (dst2 uid1,uid2)
    ;;   (dst2 uid3)
    ;;   (dst3 uid4))
    (let (uid del fld flds ent ents dels2 fld-uids uids)
      (setq ents rtrs)
      (setq rtrs nil)
      (while ents
	(setq ent  (car ents))
	(setq ents (cdr ents))
	(setq uid  (nth 0 ent))
	(setq del  (nth 2 ent))
	(setq flds (nthcdr 3 ent))
	(if del (setq dels2 (cons uid dels2)))
	(while flds
	  (setq fld  (car flds))
	  (setq flds (cdr flds))
	  (if (setq fld-uids (assoc fld rtrs))
	      (nconc fld-uids (list uid))
	    (setq rtrs (cons (list fld uid) rtrs)))))
      ;;
      (setq ents rtrs)
      (setq rtrs nil)
      (while ents
	(setq ent  (car ents))
	(setq ents (cdr ents))
	(setq fld  (car ent))
	(setq uids (cdr ent))
	(setq uids (mew-net-msg-group uids))
	(setq uids (mapcar (lambda (arg) (list fld arg)) uids))
	(setq rtrs (nconc rtrs uids)))
      (mew-imap-set-rtrs pnm rtrs)
      ;;
      (setq dels (mew-net-msg-group dels))
      (setq dels2 (nreverse dels2))
      (setq dels2 (mew-net-msg-group dels2))
      (mew-imap-set-dels pnm (nconc dels dels2))))
   (t
    (mew-imap-set-rtrs pnm rtrs)
    (setq dels (mew-net-msg-group dels))
    (mew-imap-set-dels pnm dels)))
  ;;
  (mew-imap-set-dttl pnm (length dels))
  (mew-imap-set-rttl pnm (length rtrs))
  (mew-imap-set-hlds pnm hlds)
  (cond
   ((or (eq directive 'sync) (eq directive 'biff))
    (mew-imap-set-status pnm "logout")
    (mew-imap-command-logout pro pnm))
   ((eq directive 'exec)
    (if rtrs
	(mew-imap-command-pre-copy pro pnm)
      (mew-imap-command-pre-dels pro pnm)))
   (t ;; 'get and 'inc
    (mew-imap-command-pre-fetch pro pnm))))

(defun mew-imap-command-pre-copy (pro pnm)
  (let ((rttl (mew-imap-get-rttl pnm)))
    (cond
     ((= rttl 0) ;; should not occur
      (mew-imap-set-status pnm "dels") ;; logout
      (mew-imap-command-dels pro pnm))
     ((= rttl 1)
      (mew-imap-message pnm "Refiling 1 message group in background...")
      (mew-imap-set-status pnm "copy")
      (mew-imap-command-copy pro pnm))
     (t
      (mew-imap-message pnm "Refiling %d message groups in background..." rttl)
      (mew-imap-set-status pnm "copy")
      (mew-imap-command-copy pro pnm)))))

(defun mew-imap-command-copy (pro pnm)
  (let* ((rtrs (mew-imap-get-rtrs pnm))
	 (rtr (car rtrs))
	 (dst (nth 0 rtr))
	 (uid (nth 1 rtr)))
    (if (null rtr)
	(cond
	 ((mew-imap-get-dels pnm)
	  (mew-imap-set-status pnm "dels")
	  (mew-imap-command-dels pro pnm))
	 (t
	  (mew-imap-set-status pnm "logout")
	  (mew-imap-command-logout pro pnm)))
      (mew-imap-set-dcnt pnm (1+ (mew-imap-get-dcnt pnm)))
      (mew-imap-set-rtrs pnm (cdr rtrs))
      (setq dst (mew-bnm-to-mailbox dst))
      (mew-imap-set-rfl pnm rtr)
      (mew-imap-process-send-string pro pnm "UID COPY %s \"%s\"" uid dst))))

(defun mew-imap-command-create (pro pnm)
  (let* ((rtrs (mew-imap-get-rtrs pnm))
	 (rtr (mew-imap-get-rfl pnm))
	 (dst (nth 0 rtr))) ;; 'exec only
    (mew-imap-set-rtrs pnm (cons rtr rtrs))
    (setq dst (mew-bnm-to-mailbox dst))
    (mew-imap-process-send-string pro pnm "CREATE \"%s\"" dst)))

(defun mew-imap-command-wmbx (pro pnm)
  ;; xxx
  (mew-imap-set-status pnm "logout")
  (mew-imap-command-logout pro pnm))

(defun mew-imap-command-pre-dels (pro pnm)
  (let ((dttl (mew-imap-get-dttl pnm)))
    (cond
     ((= dttl 0) ;; should not occur
      (mew-imap-set-status pnm "logout")
      (mew-imap-command-logout pro pnm))
     ((= dttl 1)
      (mew-imap-message pnm "Deleting 1 message group in background...")
      (mew-imap-set-status pnm "dels")
      (mew-imap-command-dels pro pnm))
     (t
      (mew-imap-message pnm "Deleting %d message groups in background..." dttl)
      (mew-imap-set-status pnm "dels")
      (mew-imap-command-dels pro pnm)))))

(defun mew-imap-command-dels (pro pnm)
  (mew-net-status1
   (mew-imap-get-bnm pnm) (mew-imap-get-dttl pnm) (mew-imap-get-dcnt pnm)
   (mew-imap-secure-p pnm))
  (let ((dels (mew-imap-get-dels pnm))
	num)
    (if (null dels)
	(progn
	  ;; xxx refile here
	  (mew-imap-set-status pnm "expunge")
	  (mew-imap-command-expunge pro pnm))
      (mew-imap-set-dcnt pnm (1+ (mew-imap-get-dcnt pnm)))
      (setq num (car dels))
      (mew-imap-set-dels pnm (cdr dels))
      (mew-imap-process-send-string
       pro pnm "UID STORE %s +FLAGS (\\Deleted)" num))))

(defun mew-imap-command-pre-fetch (pro pnm)
  (let* ((rttl (mew-imap-get-rttl pnm)))
    (cond
     ((= rttl 0)
      (mew-imap-message pnm "No new messages")
      (mew-imap-set-status pnm "logout")
      (mew-imap-command-logout pro pnm))
     ((= rttl 1)
      (mew-imap-message pnm "Retrieving 1 message group in background...")
      (mew-imap-set-status pnm "fetch")
      (mew-imap-command-fetch pro pnm))
     (t
      (mew-imap-message pnm "Retrieving %d message groups in background..." rttl)
      (mew-imap-set-status pnm "fetch")
      (mew-imap-command-fetch pro pnm)))))

(defun mew-imap-command-fetch (pro pnm)
  (mew-net-status2 (mew-imap-get-bnm pnm)
		   (mew-imap-get-rttl pnm)
		   (mew-imap-get-rcnt pnm)
		   (nth 1 (car (mew-imap-get-rtrs pnm)))
		   'zero
		   (mew-imap-secure-p pnm))
  (let* ((directive (mew-imap-get-directive pnm))
	 (rtrs (mew-imap-get-rtrs pnm))
	 (dels (mew-imap-get-dels pnm))
	 (rtr (car rtrs))
	 (uid (nth 0 rtr))
	 (siz (nth 1 rtr))
	 (lim (mew-imap-get-size pnm))
	 (get-body (mew-imap-get-get-body pnm)))
    (cond
     ((or (null rtr) (eq directive 'biff))
      (mew-imap-set-truncated pnm nil)
      (if dels
	  (progn
	    (mew-imap-set-status pnm "dels")
	    (mew-imap-command-dels pro pnm))
	(mew-imap-set-status pnm "logout")
	(mew-imap-command-logout pro pnm)))
     ((or (eq directive 'get) (eq directive 'exec)) ;; xxx
      (mew-imap-set-truncated pnm nil)
      (mew-imap-process-send-string pro pnm "UID FETCH %s RFC822" uid))
     ((and (eq directive 'scan) (not get-body))
      (mew-imap-set-truncated pnm t)
      (mew-imap-process-send-string pro pnm "UID FETCH %s RFC822.HEADER" uid))
     ((or (= lim 0) (< (string-to-int siz) lim))
      (mew-imap-set-truncated pnm nil)
      (mew-imap-process-send-string pro pnm "UID FETCH %s RFC822" uid))
     (t
      (mew-imap-set-truncated pnm t)
      (mew-imap-process-send-string
       pro pnm "UID FETCH %s RFC822.HEADER" uid)))))

(defun mew-imap-command-post-fetch (pro pnm)
  (let* ((directive (mew-imap-get-directive pnm))
	 (width (1- (mew-scan-width)))
	 (rtrs (mew-imap-get-rtrs pnm))
	 (rtr (car rtrs))
	 (uid (nth 0 rtr))
	 (siz (nth 1 rtr))
	 (fld-msg (nth 3 rtr)) ;; 'exec, 'scan, ...
	 (truncated (mew-imap-get-truncated pnm))
	 fld msg vec file msg-file lmsg)
    (cond
     ((null fld-msg)
      (setq fld (mew-imap-get-bnm pnm)))
     ((stringp fld-msg)
      (setq fld fld-msg))
     ((listp fld-msg)
      (setq fld (nth 0 fld-msg))
      (setq msg (nth 1 fld-msg))
      (setq lmsg msg)))
    ;;
    (goto-char (point-min))
    (while (looking-at "^\\*")
      (forward-line))
    (delete-region (point-min) (point))
    ;; line delimiters
    (mew-eol-fix-for-read)
    ;; deleting ")"
    (goto-char (point-max))
    ;; skip the last response
    (forward-line -2)
    ;; skip untagged responses if any
    (while (looking-at "^\\*")
      (forward-line -1))
    ;; We cannot count up the octet number of literal in Emacs.
    ;; Assuming that the message ends with CRLF, the cursor now locates
    ;; either
    ;;   *)
    ;; or
    ;;   * ATTRIBUTE VAL ...)
    ;; This also assumes that all attributes are stored in a line.
    (delete-region (point) (point-max))
    ;; line delimiters
    (mew-eol-fix-for-read)
    (setq msg-file (mew-net-get-new-message
		    pnm fld msg 'mew-imap-get-msgdb 'mew-imap-set-msgdb))
    (setq msg (car msg-file) file (cdr msg-file))
    (goto-char (point-min))
    (cond
     (truncated
      (mew-header-insert-xmu uid siz t))
     ((eq directive 'scan)
      (mew-header-insert-xmu uid siz nil))
     ((and (eq directive 'get) (mew-folder-imapp (mew-folder-folder fld)))
      (mew-header-insert-xmu uid siz nil)))
    (catch 'write-error
      (condition-case nil
	  (mew-frwlet 
	   mew-cs-dummy mew-cs-text-for-write
	   (write-region (point-min) (point-max) file nil 'no-msg))
	(error
	 (mew-imap-set-status pnm "logout")
	 (mew-imap-command-logout pro pnm)
	 (throw 'write-error nil)))
      (mew-set-file-modes file)
      (mew-imap-set-rcnt pnm (1+ (mew-imap-get-rcnt pnm)))
      ;; xxx
      (mew-set-buffer-multibyte t)
      (setq vec (mew-imap-scan-header))
      (mew-scan-set-folder vec fld)
      (mew-scan-set-message vec msg)
      (mew-set-buffer-multibyte nil)
      (mew-scan-insert-line fld vec width lmsg nil)
      (mew-imap-set-rtrs pnm (cdr rtrs))
      (mew-imap-set-status pnm "fetch")
      (mew-imap-command-fetch pro pnm))))

(defun mew-imap-command-expunge (pro pnm)
  (mew-imap-process-send-string pro pnm "EXPUNGE"))

(defun mew-imap-command-list (pro pnm)
  (mew-imap-message pnm "Collecting mailbox list...")
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Listing"
		  nil
		  (mew-imap-secure-p pnm))
  (mew-imap-process-send-string pro pnm "LIST \"\" *"))

(defun mew-imap-command-post-list (pro pnm)
  (let* ((case (mew-imap-get-case pnm))
	 (friendp (concat "^" (regexp-quote (mew-imap-friend-folder case))))
	 mbx mailbox mailboxes regex sep subnm friends)
    ;; line delimiters
    (mew-eol-fix-for-read)
    (while (not (eobp))
      (when (looking-at "\\* LIST ([^)\n]*) \"?\\([^\"\n]*\\)\"? \"?\\([^\"\n]*\\)\"?")
	(unless regex
	  (setq sep (mew-match-string 1))
	  (unless (string= sep "NIL")
	    (setq regex (concat "\\([^" sep "]+\\)$"))
	    (setq mailboxes (cons (mew-folder-func mew-imap-inbox-folder sep) mailboxes))))
	(setq mbx (mew-match-string 2))
	(let ((case-fold-search nil))
	  (if (string-match "^INBOX" mbx)
	      (setq mbx (concat "inbox" (substring mbx (match-end 0))))))
	(setq mailbox (concat mew-folder-imap mbx))
	(cond
	 ((null regex)
	  (unless (string= mailbox mew-imap-inbox-folder)
	    (setq mailboxes (cons (mew-folder-func mailbox mbx) mailboxes))))
	 ((string= mailbox mew-imap-inbox-folder)
	  ())
	 ((string-match friendp mailbox)
	  (setq friends (cons mailbox friends))
	  (setq mailboxes (cons (mew-folder-func mailbox) mailboxes)))
	 ((string-match regex mailbox)
	  (setq subnm (match-string 1 mailbox))
	  (setq mailboxes (cons (mew-folder-func mailbox subnm) mailboxes)))))
      (forward-line))
    (unless case (setq case mew-case-default))
    (setq mailboxes (nreverse mailboxes))
    (setq mailboxes (cons (mew-folder-func (mew-imap-queue-folder case)) mailboxes))
    (setq mailboxes (cons (mew-folder-func (mew-imap-trash-folder case)) mailboxes))
    (mew-imap-folder-set case mailboxes friends)
    (mew-imap-set-status pnm "logout")
    (mew-imap-command-logout pro pnm)))

(defun mew-imap-command-logout (pro pnm)
  (let ((wrk (mew-imap-get-wrk pnm)) job jobs data)
    (if wrk
	(progn
	  (mew-queue-backup wrk mew-imapq-info-suffix)
	  (if (mew-imap-get-jcnt pnm)
	      (mew-imap-set-jcnt pnm (1+ (mew-imap-get-jcnt pnm)))
	    (mew-imap-set-jcnt pnm 1))
	  (setq jobs (mew-imap-get-jobs pnm))
	  (setq data (mew-imap-queue-get-next jobs))
	  (setq job (nth 0 data))
	  (setq wrk (nth 1 data))
	  (setq jobs (nth 2 data))
	  (if job
	      (progn
		(mew-imap-set-rcnt pnm 1)
		(mew-imap-set-dcnt pnm 1)
		(mew-imap-set-rttl pnm 0)
		(mew-imap-set-dttl pnm 0)
		(mew-imap-set-mailbox pnm (mew-bnm-to-mailbox (nth 0 job)))
		(mew-imap-set-refs pnm (nth 1 job))
		(mew-imap-set-rmvs pnm (nth 2 job))
		(mew-imap-set-wrk pnm wrk)
		(mew-imap-set-jobs pnm jobs)
		(mew-imap-set-status pnm "select")
		(mew-imap-command-select pro pnm))
	    (mew-imap-set-done pnm t)
	    (mew-imap-process-send-string pro pnm "LOGOUT")))
      (mew-imap-set-done pnm t)
      (mew-imap-process-send-string pro pnm "LOGOUT"))))

(defun mew-imap-command-logout2 (pro pnm)
  (let ((wrk (mew-imap-get-wrk pnm)))
    (if wrk (mew-queue-enqueue2 wrk)))
  (mew-imap-set-status pnm "logout")
  (mew-imap-process-send-string pro pnm "LOGOUT"))

(defun mew-imap-command-noop (pro pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AUTH
;;;

(defvar mew-imap-auth-alist
  '(("CRAM-MD5" mew-imap-command-auth-cram-md5)
    ("LOGIN"    mew-imap-command-auth-login)))

(defsubst mew-imap-auth-get-func (auth)
  (nth 1 (mew-assoc-case-equal auth mew-imap-auth-alist 0)))

(defun mew-imap-command-auth-cram-md5 (pro pnm)
  (mew-imap-process-send-string pro pnm "AUTHENTICATE CRAM-MD5")
  (mew-imap-set-status pnm "auth-cram-md5"))

(defun mew-imap-command-pwd-cram-md5 (pro pnm)
  (let ((user (mew-imap-get-user pnm))
	challenge passwd cram-md5)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward " \\([a-zA-Z0-9+/]+=*\\)" nil t)
	  (setq challenge (mew-match-string 1))))
    (setq passwd (mew-imap-input-passwd "CRAM-MD5 password: " pnm))
    (setq cram-md5 (mew-cram-md5 user passwd challenge))
    (mew-imap-process-send-string2 pro cram-md5)))

(defun mew-imap-command-auth-login (pro pnm)
  (mew-imap-process-send-string pro pnm "AUTHENTICATE LOGIN")
  (mew-imap-set-status pnm "auth-login"))

(defun mew-imap-command-user-login (pro pnm)
  (let* ((user (mew-imap-get-user pnm))
	 (euser (mew-base64-encode-string user)))
    (mew-imap-process-send-string2 pro euser)))

(defun mew-imap-command-pwd-login (pro pnm)
  (let* ((passwd (mew-imap-input-passwd "LOGIN password: " pnm))
	 (epasswd (mew-base64-encode-string passwd)))
    (mew-imap-process-send-string2 pro epasswd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-imap-info-prefix "mew-imap-info-")

(defsubst mew-imap-info-name (case mailbox)
  (let ((server (mew-imap-server case))
	(port (mew-imap-port case))
	(sshsrv (mew-imap-ssh-server case))
	(name mew-imap-info-prefix))
    (setq name (concat name server "/" mailbox))
    (unless (string= port mew-imap-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defsubst mew-imap-buffer-name (pnm)
  (concat mew-buffer-prefix pnm))

(defun mew-imap-debug (label string)
  (when (mew-debug 'net)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-imap-process-send-string (pro pnm &rest args)
  (let ((str (apply 'format args))
	(tag (mew-imap-tag)))
    (mew-imap-debug "=SEND=" str)
    (mew-imap-set-tag pnm tag)
    (process-send-string pro (concat tag " " str mew-cs-eol))))

(defun mew-imap-process-send-string2 (pro &rest args)
  (let ((str (apply 'format args)))
    (process-send-string pro (concat str mew-cs-eol))))

(defun mew-imap-scan-header ()
  (goto-char (point-min))
  (unless (re-search-forward mew-eoh nil t)
    (goto-char (point-max)))
  (mew-scan-header))

(defsubst mew-imap-message (pnm &rest args)
  (or (mew-imap-get-no-msg pnm) (apply 'message args)))

(defun mew-imap-tag ()
  (format "%s%04d" (mew-random-string 4 nil) (% (mew-random) 10000)))

(defun mew-imap-passtag (pnm)
  (let ((server (mew-imap-get-server pnm))
	(port (mew-imap-get-port pnm))
	(user (mew-imap-get-user pnm)))
    (concat user "@" server ":" port)))

(defun mew-imap-passtag2 (case)
  (let ((server (mew-imap-server case))
	(port (mew-imap-port case))
	(user (mew-imap-user case)))
    (concat user "@" server ":" port)))

(defun mew-imap-input-passwd (prompt pnm)
  (let ((directive (mew-imap-get-directive pnm))
	(tag (mew-imap-passtag pnm)))
    (if (eq directive 'biff)
	(or (mew-imap-get-passwd pnm)      ;; mew-imap-biff
	    (mew-input-passwd prompt tag)) ;; mew-imap-check
      (mew-input-passwd prompt tag))))

(defsubst mew-bnm-to-mailbox (bnm)
  (let ((case-folder (mew-folder-case bnm))
	folder)
    (if case-folder
	(setq folder (cdr case-folder))
      (setq folder bnm))
    (mew-folder-string folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening IMAP
;;;

(defun mew-imap-open (pnm server port no-msg)
  (let ((sprt (mew-port-sanity-check port))
	pro tm)
    (condition-case emsg
	(progn
	  (setq tm (mew-timer mew-imap-timeout-time 'mew-imap-timeout))
	  (or no-msg (message "Connecting to the IMAP server..."))
	  (setq pro (open-network-stream pnm nil server sprt))
	  (process-kill-without-query pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (or no-msg (message "Connecting to the IMAP server...done")))
      (quit
       (or no-msg (message "Cannot connect to the IMAP server"))
       (setq pro nil))
      (error
       (or no-msg (message "%s, %s" (nth 1 emsg) (nth 2 emsg)))
       (setq pro nil)))
    (if tm (cancel-timer tm))
    pro))

(defun mew-imap-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-imap-retrieve (case directive bnm &rest args)
  (let* ((server (mew-imap-server case))
	 (port (mew-imap-port case))
	 (sshsrv (mew-imap-ssh-server case))
	 (sslp (mew-imap-ssl case))
	 (sslport (mew-imap-ssl-port case))
	 (mailbox (mew-bnm-to-mailbox bnm))
	 (pnm (mew-imap-info-name case mailbox))
	 (buf (get-buffer-create (mew-imap-buffer-name pnm)))
	 (no-msg (eq directive 'biff))
	 process sshname sshpro sslname sslpro lport)
    (if (mew-imap-get-process pnm)
	(message "Another IMAP process is running. Try later")
      (cond
       (sshsrv
	(setq sshpro (mew-open-ssh-stream server port sshsrv))
	(when sshpro
	  (setq sshname (process-name sshpro))
	  (setq lport (mew-ssh-pnm-to-lport sshname))
	  (when lport
	    (setq process (mew-imap-open pnm "localhost" lport no-msg)))))
       (sslp
 	(setq sslpro (mew-open-ssl-stream server sslport))
 	(when sslpro
 	  (setq sslname (process-name sslpro))
	  (setq lport (mew-ssl-pnm-to-lport sslname))
 	  (when lport
 	    (setq process (mew-imap-open pnm "localhost" lport no-msg)))))
       (t
	(setq process (mew-imap-open pnm server port no-msg))))
      (if (null process)
	  (when (eq directive 'exec)
	    (mew-summary-visible-buffer bnm)
	    (mew-sinfo-set-refile
	     (nconc (mew-sinfo-get-refile) (mew-sinfo-get-refile-back)))
	    (mew-sinfo-set-refile-back nil))
	(mew-summary-lock process "IMAPing" (or sshpro sslpro))
	(mew-sinfo-set-scan-form (mew-summary-scan-form bnm))
	(mew-sinfo-set-scan-id nil)
	(mew-info-clean-up pnm)
	(mew-imap-set-no-msg pnm no-msg) ;; must come here
	(mew-imap-message pnm "Communicating with the IMAP server...")
	(mew-imap-set-process pnm process)
	(mew-imap-set-ssh-process pnm sshpro)
	(mew-imap-set-ssl-process pnm sslpro)
	(mew-imap-set-server pnm server)
	(mew-imap-set-port pnm port)
	(mew-imap-set-user pnm (mew-imap-user case))
	(mew-imap-set-auth pnm (mew-imap-auth case))
	(mew-imap-set-auth-list pnm (mew-imap-auth-list case))
	(mew-imap-set-status pnm "greeting")
	(mew-imap-set-directive pnm directive)
	(mew-imap-set-bnm pnm bnm)
	(mew-imap-set-rcnt pnm 1)
	(mew-imap-set-dcnt pnm 1)
	(mew-imap-set-jcnt pnm nil)
	(mew-imap-set-rttl pnm 0)
	(mew-imap-set-dttl pnm 0)
	(mew-imap-set-size pnm (mew-imap-size case))
	(mew-imap-set-mailbox pnm mailbox)
	(mew-imap-set-case pnm case)
	(cond
	 ((eq directive 'biff)
	  (mew-imap-set-passwd pnm (nth 0 args))) ;; password
	 ((eq directive 'inc)
	  (mew-imap-set-flush pnm (nth 0 args)) ;; no-flush
	  (mew-imap-set-delete pnm (mew-imap-delete case)))
	 ((eq directive 'get)
	  (mew-imap-set-refs pnm (nth 0 args)))
	 ((eq directive 'scan)
	  (mew-imap-set-range pnm (nth 0 args))
	  (mew-imap-set-get-body pnm (nth 1 args))
	  (if (mew-imap-get-range pnm)
	      (mew-net-folder-clean)
	    (mew-net-invalid-cache-clean)))
	 ((eq directive 'sync)
	  )
	 ((eq directive 'exec)
	  ;; (uid siz del dst1 dst2 ...)
	  (mew-imap-set-refs pnm (nth 0 args))
	  (mew-imap-set-rmvs pnm (nth 1 args))
	  (mew-imap-set-kils pnm (nth 2 args))
	  (setq mailbox (nth 3 args))
	  (when mailbox
	    (setq mailbox (mew-bnm-to-mailbox mailbox))
	    (mew-imap-set-mailbox pnm mailbox))
	  (mew-imap-set-wrk pnm (nth 4 args))
	  (mew-imap-set-jobs pnm (nth 5 args))))
	(mew-sinfo-set-start-point (point)) ;; after erase-buffer
	;;
	(set-process-sentinel process 'mew-imap-sentinel)
	(set-process-filter process 'mew-imap-filter)
	(set-process-buffer process buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-imap-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-imap-get-status pnm))
	 (tag (mew-imap-get-tag pnm))
	 (eos (format "^%s \\(OK\\|NO\\|BAD\\)\\(.*\\)" tag))
	 stay next func code resp)
    (mew-imap-debug (upcase status) string)
    (mew-filter
     ;; Process's buffer
     (goto-char (point-max))
     (mew-set-buffer-multibyte nil)
     (insert string)
     (when (string= status "fetch")
       (mew-net-status2 (mew-imap-get-bnm pnm)
			(mew-imap-get-rttl pnm)
			(mew-imap-get-rcnt pnm)
			(nth 1 (car (mew-imap-get-rtrs pnm)))
			nil
			(mew-imap-secure-p pnm)))
     (cond
      ((and (goto-char (point-max)) (forward-line -1) (looking-at  "\\*"))
       ;; untagged message
       (if (string= status "greeting")
	   (setq next (mew-imap-fsm-next "greeting" "OK"))
	 (setq stay t)))
      ((and (goto-char (point-min)) (looking-at "\\+"))
       (setq next (mew-imap-fsm-next status "OK")))
      ((and (goto-char (point-max)) (forward-line -1) (looking-at eos))
       (mew-imap-set-tag pnm nil)
       (setq code (mew-match-string 1))
       (setq resp (mew-match-string 2))
       (setq next (mew-imap-fsm-next status code))
       (unless next (setq next (mew-imap-fsm-next status resp))))
      (t
       (setq stay t)))
     (unless stay
       (unless next (setq next "logout"))
       (mew-imap-set-status pnm next)
       (setq func (intern-soft (concat "mew-imap-command-" next)))
       (goto-char (point-min))
       (if (fboundp func)
	   (and func (funcall func process pnm))
	 (error "No function called %s" (symbol-name func)))
       (mew-erase-buffer)))))

(defun mew-imap-sentinel (process event)
  (let* ((pnm (process-name process))
	 (directive (mew-imap-get-directive pnm))
	 (sshpro (mew-imap-get-ssh-process pnm))
	 (sslpro (mew-imap-get-ssl-process pnm))
	 (rttl (mew-imap-get-rttl pnm))
	 (dttl (mew-imap-get-dttl pnm))
	 (jcnt (mew-imap-get-jcnt pnm))
	 (bnm (mew-imap-get-bnm pnm))
	 (flush (mew-imap-get-flush pnm))
	 (kils (mew-imap-get-kils pnm))
	 (hlds (mew-imap-get-hlds pnm))
	 (msgid (mew-imap-get-msgid pnm))
	 (done (mew-imap-get-done pnm))
	 (file (mew-expand-folder bnm mew-imap-msgid-file)))
    (mew-imap-debug "IMAP SENTINEL" event)
    (mew-filter
     (set-buffer bnm)
     (when done
       (cond
	((eq directive 'list)
	 (mew-imap-message pnm "Collecting mailbox list...done"))
	((eq directive 'biff)
	 (funcall mew-biff-function rttl))
	((eq directive 'sync)
	 (mew-imap-message pnm "Synchronizing messages...")
	 (mew-net-folder-sync bnm hlds)
	 (mew-imap-message pnm "Synchronizing messages...done")
	 (mew-summary-folder-cache-save))
	((eq directive 'inc)
	 (mew-biff-clear)
	 (mew-net-uidl-db-set (mew-imap-passtag pnm) (mew-imap-get-uidl pnm))
	 (cond
	  ((= rttl 0)
	   (mew-imap-message pnm "No new messages"))
	  ((= rttl 1)
	   (mew-imap-message pnm "1 message retrieved")
	   (mew-summary-folder-cache-save))
	  (t
	   (mew-imap-message pnm "%s messages retrieved" rttl)
	   (mew-summary-folder-cache-save))))
	((eq directive 'get)
	 (cond
	  ((= rttl 0)
	   (mew-imap-message pnm "No new messages"))
	  ((= rttl 1)
	   (mew-imap-message pnm "1 message retrieved")
	   (mew-summary-folder-cache-save))
	  (t
	   (mew-imap-message pnm "%s messages retrieved" rttl)
	   (mew-summary-folder-cache-save))))
	((eq directive 'scan)
	 (mew-biff-clear)
	 (cond
	  ((= rttl 0)
	   (mew-imap-message pnm "No new messages"))
	  ((= rttl 1)
	   (mew-imap-message pnm "1 message retrieved")
	   (mew-lisp-save file msgid)
	   (mew-summary-folder-cache-save))
	  (t
	   (mew-imap-message pnm "%s messages retrieved" rttl)
	   (mew-lisp-save file msgid)
	   (mew-summary-folder-cache-save))))
	((eq directive 'exec)
	 (when kils
	   (mew-mark-exec-unlink bnm kils)
	   (mew-mark-kill-invisible)
	   (mew-summary-folder-cache-save))
	 (cond
	  (jcnt
	   (if (= jcnt 1)
	       (mew-imap-message pnm "1 job processed")
	     (mew-imap-message pnm "%d jobs processed" jcnt)))
	  ((= rttl 1)
	   (mew-imap-message pnm "1 message group refiled"))
	  ((> rttl 1)
	   (mew-imap-message pnm "%d message groups refiled" rttl))
	  ((= dttl 1)
	   (mew-imap-message pnm "1 message group deleted"))
	  (t
	   (mew-imap-message pnm "%d message groups deleted" dttl))))))
     ;;
     (mew-net-status-clear (mew-imap-get-bnm pnm))
     (mew-info-clean-up pnm)
     (set-buffer-modified-p nil)
     (mew-summary-unlock)
     (if (and (processp sshpro) (not mew-ssh-keep-connection))
	 (process-send-string sshpro "exit\n"))
     (if (and (processp sslpro) (not mew-ssl-keep-connection))
 	 (delete-process sslpro))
     (unless (eq directive 'biff)
       (run-hooks 'mew-imap-sentinel-non-biff-hook))
     (run-hooks 'mew-imap-sentinel-hook)
     (when (and mew-auto-flush-queue flush)
       (mew-smtp-flush-queue mew-case-output)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biff
;;;

(defun mew-imap-biff ()
  (let* ((case mew-case-input)
	 (inbox (mew-proto-inbox-folder nil case))
	 (tag (mew-imap-passtag2 case))
	 passwd)
    (save-excursion
      (set-buffer inbox)
      (when (and (mew-summary-exclusive-p 'no-msg)
		 (and mew-use-cached-passwd
		      (setq passwd (mew-passwd-get-passwd tag))))
	(mew-imap-retrieve case 'biff inbox passwd)))))

(defun mew-imap-check ()
  (interactive)
  (let* ((case mew-case-input)
	 (inbox (mew-proto-inbox-folder nil case)))
    (save-excursion
      (set-buffer inbox)
      (when (mew-summary-exclusive-p)
	(mew-imap-retrieve case 'biff inbox)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mailbox alist
;;;

(defun mew-imap-folder-clean-up ()
  (setq mew-imap-folder-alist nil)
  (setq mew-imap-friend-folder-list nil))

(defun mew-imap-folder-alist (&optional case no-load)
  (unless case (setq case mew-case-default))
  (let* ((ent (assoc case mew-imap-folder-alist))
	 (t2 (nth 1 ent)) ;; may be nil
	 (alist (nth 2 ent))
	 (fld (mew-imap-folder case))
	 (file (mew-expand-folder fld mew-imap-folder-alist-file))
	 (t1 (mew-file-get-time file)))
    (if (or no-load (and t1 (not (mew-compare-times t1 t2))))
	alist
      (mew-imap-folder-load case)
      (setq ent (assoc case mew-imap-folder-alist))
      (setcar (nthcdr 1 ent) t1)
      (setq alist (nth 2 ent))
      (if alist
	  alist
	(list
	 (mew-folder-func mew-imap-inbox-folder)
	 (mew-folder-func (mew-imap-trash-folder case))
	 (mew-folder-func (mew-imap-queue-folder case)))))))

(defun mew-imap-friend-folder-list (&optional case no-load)
  (unless case (setq case mew-case-default))
  (unless no-load (mew-imap-folder-alist case)) ;; do update
  (cdr (assoc case mew-imap-friend-folder-list)))

(defun mew-imap-folder-load (case)
  (let* ((fld (mew-imap-folder case))
	 (file (mew-expand-folder fld mew-imap-folder-alist-file))
	 (mailboxes (mew-lisp-load file))
	 (file2 (mew-expand-folder fld mew-imap-friend-folder-list-file))
	 (friends (mew-lisp-load file2)))
    (mew-imap-folder-set case mailboxes friends 'no-save)))

(defun mew-imap-folder-save (case)
  (let* ((fld (mew-imap-folder case))
	 (dir (mew-expand-folder fld))
	 (file (expand-file-name mew-imap-folder-alist-file dir))
	 (file2 (expand-file-name mew-imap-friend-folder-list-file dir))
	 (mailboxes (mew-imap-folder-alist case 'no-load))
	 (friends (mew-imap-friend-folder-list case 'no-load)))
    (mew-check-directory dir)
    (mew-lisp-save file mailboxes 'nobackup 'unlimit)
    (mew-lisp-save file2 friends 'nobackup 'unlimit)
    (mew-file-get-time file)))

(defun mew-imap-folder-set (case mailboxes friends &optional no-save)
  (unless case (setq case mew-case-default))
  (setq mew-imap-folder-alist
	(cons (list case nil mailboxes)
	      (delete (assoc case mew-imap-folder-alist)
		      mew-imap-folder-alist)))
  (setq mew-imap-friend-folder-list
	(cons (cons case friends)
	      (delete (assoc case mew-imap-friend-folder-list)
		      mew-imap-friend-folder-list)))
  (unless no-save
    (let ((t1 (mew-imap-folder-save case))
	  (ent (assoc case mew-imap-folder-alist)))
      (setcar (nthcdr 1 ent) t1))))

(defun mew-imap-update ()
  (let ((bnm (mew-summary-folder-name 'ext))
	(case (mew-sinfo-get-case)))
    (if mew-config-cases
	(setq case (mew-input-case case "IMAP")))
    (mew-imap-retrieve case 'list bnm)))

(defun mew-imap-folder-insert (case folder)
  (let* ((sep (mew-imap-separator case))
	 (regex (concat "\\([^" sep mew-folder-imap "]+\\)$"))
	 (mailboxes (mew-imap-folder-alist case))
	 (friends (mew-imap-friend-folder-list case))
	 (friendp (concat "^" (regexp-quote (mew-imap-friend-folder case))))
	 sub)
    (when (string-match regex folder)
      (setq sub (match-string 1 folder))
      (setq mailboxes (cons (mew-folder-func folder sub) mailboxes))
      (when (string-match friendp folder)
	(setq friends (cons folder friends)))
      (mew-imap-folder-set case mailboxes friends)
      t)))

(defun mew-imap-separator (case)
  (or (cdr (assoc mew-imap-inbox-folder (mew-imap-folder-alist case)))
      mew-path-separator)) ;; xxx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flush the job queue
;;;

(defun mew-imap-queue-get-next (msgs)
  (let* (file job work info info-work)
    (while (and msgs (not job))
      (setq file (car msgs))
      (setq msgs (cdr msgs))
      (setq work (concat file mew-queue-work-suffix))
      (setq info (concat file mew-imapq-info-suffix))
      (when (and (file-readable-p file) (file-readable-p info))
	(rename-file file work 'override)
	(setq job (mew-lisp-load info))
	(setq info-work (concat info mew-queue-work-suffix))
	(rename-file info info-work 'override)))
    (list job work msgs)))

(defun mew-imap-flush-queue ()
  (let ((case (mew-sinfo-get-case))
	(qfld (mew-summary-folder-name 'ext))
	data wrk jobs job)
    (mew-summary-folder-cache-clean qfld)
    (setq jobs (directory-files (mew-expand-folder qfld)
				'full mew-regex-message-files))
    (setq data (mew-imap-queue-get-next jobs))
    (setq job (nth 0 data) wrk (nth 1 data) jobs (nth 2 data))
    (if (null job)
	(message "No IMAP jobs")
      (mew-imap-retrieve
       case 'exec qfld
       (nth 1 job) ;; refs
       (nth 2 job) ;; rmvs
       nil         ;; kils
       (nth 0 job) ;; mailbox
       wrk jobs))))

(provide 'mew-imap)

;;; Copyright Notice:

;; Copyright (C) 2002-2003 Mew developing team.
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

;;; mew-imap.el ends here
