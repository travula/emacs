;;; mew-net.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb 27, 2002

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Port DB
;;;

(defvar mew-port-db
  '(("smtp"  .  "25")
    ;; This is officially assigned to another service (urd) by IANA.
    ;; IESG will never assign a port number to SMTP over SSL...
    ("smtps" . "465")
    ("pop3"  . "110")
    ("pop3s" . "995")
    ("nntp"  . "119")
    ("nntps" . "563")
    ("imap"  . "143")
    ("imaps" . "993")))

(defun mew-serv-to-port (serv)
  (if (string-match "^[0-9]+$" serv)
      serv
    (or (cdr (assoc serv mew-port-db)) "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Net folders
;;;

(defvar mew-pop-folder  "+.pop/%s@%s%%%s")
(defvar mew-imap-folder "+.imap/%s@%s%%%s")
(defvar mew-nntp-folder "+.nntp/%s@%s%%%s")

(defun mew-pop-folder (&optional case)
  (format mew-pop-folder
	  (mew-pop-user case) (mew-pop-server case) (mew-pop-port case)))

(defun mew-imap-folder (&optional case)
  (format mew-imap-folder
	  (mew-imap-user case) (mew-imap-server case) (mew-imap-port case)))

(defun mew-nntp-folder (&optional case)
  (format mew-nntp-folder
	  (or (mew-nntp-user case) "") ;; a user may be nil
	  (mew-nntp-server case) (mew-nntp-port case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Net status
;;;

(defun mew-summary-lock (key status &optional secure)
  (setq mew-summary-buffer-process key)
  (setq mew-summary-buffer-process-status (format " %s" status))
  (setq mew-summary-buffer-secure-process secure))
  

(defun mew-summary-unlock ()
  (setq mew-summary-buffer-process nil)
  (setq mew-summary-buffer-process-status nil)
  (setq mew-summary-buffer-secure-process nil))

(defun mew-net-status-clear (buf)
  (when mew-use-net-status
    (save-excursion
      (set-buffer buf)
      (setq mew-summary-buffer-process-status nil)
      (setq mew-summary-buffer-secure-process nil))))

(defun mew-net-status (buf status &optional substatus secure)
  (when mew-use-net-status
    (save-excursion
      (set-buffer buf)
      (if substatus
	  (setq mew-summary-buffer-process-status
		(format " %s:%s" status substatus))
	(setq mew-summary-buffer-process-status (format " %s" status)))
      (setq mew-summary-buffer-secure-process secure))))

(defun mew-net-status1 (buf dttl dcnt &optional secure)
  (when mew-use-net-status
    (when (<= dcnt dttl)
      (let ((substatus (format "%d/%d" dcnt dttl)))
	(mew-net-status buf "Deleting" substatus secure)))))

(defvar mew-net-status-percent-size 51200)

(defun mew-net-status2 (buf rttl rcnt siz &optional zero secure)
  (when mew-use-net-status
    (when (<= rcnt rttl)
      (let ((msiz (string-to-int siz))
	    (bsiz (if zero 0 (buffer-size)))
	    percent substatus)
	(if (and (numberp mew-net-status-percent-size)
		 (< msiz mew-net-status-percent-size))
	    (progn
	      (setq substatus (format "%d/%d" rcnt rttl))
	      (mew-net-status buf "Retrieving" substatus secure))
	  (if (= msiz 0) (setq msiz 1))
	  (if (< 10000 msiz)
	      (setq percent (/ bsiz (/ msiz 100)))
	    (setq percent (/ (* bsiz 100) msiz)))
	  (setq substatus (format "%d/%d:%02d%%" rcnt rttl percent))
	  (mew-net-status buf "Retrieving" substatus secure))))))

(defvar mew-local-status-unit 10)
(defvar mew-local-status-threshold 100)

(defun mew-net-status3 (buf rttl rcnt)
  (when mew-use-net-status
    (if rttl
	(if (or (not (numberp mew-local-status-unit))
		(not (numberp mew-local-status-threshold))
		(< (- rttl rcnt) mew-local-status-threshold)
		(= 0 (% rcnt mew-local-status-unit)))
	    (mew-net-status buf "Scanning" (format "%d/%d" rcnt rttl)))
      (mew-net-status buf "Scanning" (format "%d" rcnt)))))

(defun mew-summary-visible-buffer (buf)
  (save-excursion
    (set-buffer buf)
    (mew-elet (put-text-property (point-min) (point-max) 'invisible nil))
    (setq mew-summary-buffer-raw t)
    (set-buffer-modified-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biff
;;;

(defvar mew-biff-string nil)
(defvar mew-biff-timer-id nil)

(defun mew-biff-bark (n)
  (if (= n 0)
      (setq mew-biff-string nil)
    (if (and mew-use-biff-bell (eq mew-biff-string nil))
	(beep))
    (setq mew-biff-string (format "Mail(%d)" n))))

(defun mew-biff-clear ()
  (setq mew-biff-string nil))

(defun mew-biff-setup ()
  (let ((inbox (mew-proto-inbox-folder nil mew-case-input))
	func)
    (if (not mew-use-biff)
	(mew-biff-clean-up)
      (if mew-biff-timer-id (cancel-timer mew-biff-timer-id))
      (cond
       ((mew-folder-localp inbox)
	(setq func 'mew-pop-biff))
       ((mew-folder-popp inbox)
	(setq func 'mew-pop-biff))
       ((mew-folder-imapp inbox)
	(setq func 'mew-imap-biff)))
      (if func
	  (setq mew-biff-timer-id (mew-timer (* 60 mew-biff-interval) func)))))
  (let ((ent '(mew-biff-string mew-biff-string)))
    (unless (member ent global-mode-string)
      (if global-mode-string
	  (setq global-mode-string
		(append global-mode-string (list " " ent)))
	(setq global-mode-string (list ent))))))

(defun mew-biff-clean-up ()
  (if mew-biff-timer-id (cancel-timer mew-biff-timer-id))
  (setq mew-biff-timer-id nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keep time
;;;

(defsubst mew-time-diff (t1 t2)
  (/ (+ (* (- (nth 0 t2) (nth 0 t1)) 65536)
	(- (nth 1 t2) (nth 1 t1)))
     86400.0)) ;; one day (* 60 60 24)

(defsubst mew-expired-p (time keep)
  (cond
   ((and (consp keep)
	 (stringp (nth 0 keep)) (file-exists-p (nth 0 keep))
	 (numberp (nth 1 keep)))
    (if (>= (mew-time-diff time (mew-file-get-time (nth 0 keep))) (nth 1 keep))
	t))
   ((numberp keep)
    (if (>= (mew-time-diff time (current-time)) keep) t))
   ;; ((eq keep t) t)
   ;; This case MUST not be included because messages marked with 'T'
   ;; will be deleted.
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UIDL
;;;

(defvar mew-net-uidl-file ".mew-uidl")
(defvar mew-net-uidl-db nil)

(defsubst mew-net-uidl-db-get (tag)
  (cdr (assoc tag mew-net-uidl-db)))

(defsubst mew-net-uidl-db-set (tag uidl)
  (let* ((ent (assoc tag mew-net-uidl-db)))
    (if ent
	(setcdr ent uidl)
      (setq mew-net-uidl-db (cons (cons tag uidl) mew-net-uidl-db)))
    (mew-lisp-save mew-net-uidl-file mew-net-uidl-db)))

(defun mew-net-setup ()
  (setq mew-net-uidl-db (mew-lisp-load mew-net-uidl-file))
  (add-hook 'kill-emacs-hook 'mew-net-clean-up))

(defun mew-net-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-net-clean-up)
  (mew-lisp-save mew-net-uidl-file mew-net-uidl-db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; New message
;;;

(defun mew-net-get-new-message (pnm bnm msg get set)
  (if msg
      (cons msg (mew-expand-folder bnm msg)) ;; 'get
    (let ((msg (funcall get pnm))
	  file nxt)
      (cond
       (msg
	(setq nxt (int-to-string (1+ (string-to-int msg))))
	(funcall set pnm nxt)
	(setq file (mew-expand-folder bnm msg)))
       (t
	(setq file (mew-folder-new-message bnm))
	(setq msg (file-name-nondirectory file))
	(setq nxt (int-to-string (1+ (string-to-int msg))))
	(funcall set pnm nxt)))
      (cons msg file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queue
;;;

(defun mew-queue-enqueue (work qfld) ;; 1.wrk or draft/1
  (let (orig info iwrk)
    (if (not (string-match (concat mew-queue-work-suffix "$") work))
	;; draft/1
	(setq orig (mew-folder-new-message qfld))
      ;; 1.wrk
      (setq orig (file-name-sans-extension work)) ;; 1
      (setq iwrk (concat orig mew-queue-info-suffix mew-queue-work-suffix))
      (mew-delete-file iwrk) ;; 1.mqi.wrk
      (if (file-exists-p orig) (setq orig (mew-folder-new-message qfld))))
    (rename-file work orig 'override)
    (unless mew-use-nfs-hack
      ;; This cause an error when using NFS.
      (mew-set-file-modes orig))
    (setq info (concat orig mew-queue-info-suffix))
    (list orig info)))

(defun mew-queue-enqueue2 (work) ;; 1.wrk
  (let* ((dir (file-name-directory work))
	 (orig (file-name-sans-extension work)) ;; 1
	 (info (concat orig mew-imapq-info-suffix)) ;; 1.iqi
	 (iwrk (concat info mew-queue-work-suffix))) ;; 1.iqi.wrk
    (when (file-exists-p orig)
      (setq orig (mew-folder-new-message dir))
      (setq info (concat orig mew-imapq-info-suffix)))
    (rename-file work orig 'override)
    (rename-file iwrk info 'override)))

(defun mew-queue-insert-file (pnm n file) ;; 1
  (let* ((work (concat file mew-queue-work-suffix)) ;; 1.wrk
	 (info (concat file mew-queue-info-suffix)) ;; 1.mqi
	 (iwrk (concat info mew-queue-work-suffix)) ;; 1.mqi.wrk
	 (i 0) buf data)
    (when (and (file-readable-p file) (file-readable-p info))
      (rename-file file work 'override)
      ;; If an old buffer exists by accident, we MUST remove the buffer.
      (if (setq buf (get-file-buffer work)) (mew-remove-buffer buf))
      (mew-frwlet
       mew-cs-text-for-read mew-cs-dummy
       (set-buffer (find-file-noselect work)))
      (setq data (mew-lisp-load info))
      (while (< i n)
	(aset (mew-info pnm) i (aref data i))
	(setq i (1+ i)))
      (rename-file info iwrk 'override)
      t)))

(defun mew-queue-get-next (pnm msgs n func)
  (let* (msg flushp)
    (while (and msgs (not flushp))
      (setq msg (car msgs))
      (setq msgs (cdr msgs))
      (if (mew-queue-insert-file pnm n msg) (setq flushp t)))
    (if (fboundp func) (funcall func pnm msgs)) ;; set-messages
    flushp))

(defun mew-queue-backup (work info-suffix) ;; 1.wrk
  (let* ((orig (file-name-sans-extension work)) ;; 1
	 (back (mew-prepend-prefix orig mew-backup-prefix)) ;; #1
	 (info (concat orig info-suffix)) ;; 1.sfx
	 (iwrk (concat info mew-queue-work-suffix)) ;; 1.sfx.wrk
	 (ibck (mew-prepend-prefix info mew-backup-prefix))) ;; #1.sfx
    (if (file-exists-p work) (rename-file work back 'override))
    (if (file-exists-p iwrk) (rename-file iwrk ibck 'override))
    back))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fcc:
;;;

(defun mew-net-fcc-message (flds file)
  (let (fcc-file link fld)
    (catch 'loop
      (while flds
	(setq fld (car flds))
	(mew-local-folder-check fld)
	(setq flds (cdr flds))
	(setq fcc-file (mew-folder-new-message fld))
	(when fcc-file
	  (copy-file file fcc-file)
	  (mew-set-file-modes fcc-file)
	  (mew-touch-folder fld)
	  (throw 'loop nil))))
    (while flds
      (setq fld (car flds))
      (mew-local-folder-check fld)
      (setq flds (cdr flds))
      (setq link (mew-folder-new-message fld))
      (when link
	(mew-link fcc-file link)
	(mew-touch-folder fld)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syncing
;;;

(defun mew-net-folder-sync (bnm hlds)
  (let (uid msg ulks new-hlds)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq uid (mew-summary-message-uid))
	(if (setq new-hlds (member uid hlds))
	    (progn
	      (setq hlds (cdr new-hlds))
	      (forward-line))
	  (setq msg (mew-summary-message-number))
	  (setq ulks (cons msg ulks))
	  (mew-mark-kill-line))))
    (when ulks
      (setq ulks (nreverse ulks))
      (mew-mark-exec-delete bnm ulks)
      (mew-summary-folder-cache-save))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleaning
;;;

;; See also mew-summary-folder-cache-clean.
(defun mew-net-folder-clean ()
  (let* ((bnm (mew-summary-folder-name 'ext))
	 (dir (mew-expand-folder bnm))
	 (msgs (mew-dir-messages dir mew-regex-message-files3)))
    (mew-summary-unlink-msgs bnm msgs)
    (mew-erase-buffer)
    (mew-summary-folder-cache-save)))

(defun mew-net-invalid-cache-clean ()
  (let* ((bnm (mew-summary-folder-name 'ext))
	 (dir (mew-expand-folder bnm))
	 (msgs (mew-dir-messages dir mew-regex-message-files4))
	 beg)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward mew-regex-msg-invalid nil t)
	(beginning-of-line)
	(setq beg (point))
	(goto-char (point-max))
	(mew-elet
	 (delete-region beg (point)))
	(mew-summary-unlink-msgs bnm msgs)
	(mew-summary-folder-cache-save)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieving messages
;;;

(defun mew-summary-retrieve-gap (folder)
  (when (mew-summary-folder-dir-newp)
    (goto-char (point-max))
    (let ((range (mew-range-update folder)))
      (mew-local-retrieve 'scan folder range)
      (mew-rendezvous mew-summary-buffer-process))))

(defun mew-summary-retrieve (&optional no-flush)
  "Retrieve messages to +inbox asynchronously according to
'mew-mailbox-type'. 

If 'mew-auto-flush-queue' is non-nil, +queue is flushed.
If called with '\\[universal-argument]', +queue is not flushed."
  (interactive "P")
  (let* ((case mew-case-input)
	 (inbox (mew-inbox-folder case))
	 (dir (mew-expand-folder inbox))
	 (mailbox (mew-mailbox-type case)))
    (unless (file-directory-p dir)
      (mew-make-directory dir))
    ;; for mew-summary-exchange-point.
    (set-marker mew-summary-inbox-position (point) (current-buffer))
    (mew-summary-switch-to-folder inbox)
    ;; in the inbox
    (when (mew-summary-exclusive-p)
      (mew-window-configure 'summary)
      ;;
      (mew-current-set nil nil nil)
      (mew-decode-syntax-delete)
      (mew-unhighlight-cursor-line)
      ;;
      (mew-summary-retrieve-gap inbox)
      ;;
      (goto-char (point-max))
      (mew-sinfo-set-start-point (point))
      (mew-sinfo-set-direction 'down)
      (cond
       ((eq mailbox 'pop)
	(mew-pop-retrieve case 'inc inbox (not no-flush)))
       ((eq mailbox 'imap)
	(mew-imap-retrieve case 'inc inbox (not no-flush)))
       ((eq mailbox 'mbox)
	(mew-mbox-retrieve case 'inc inbox (not no-flush)))))))

(defun mew-summary-scan-boot (proto case)
  ;; called by mew()
  (let* ((inbox (mew-proto-inbox-folder proto case))
	 (case:inbox (mew-folder-case-folder case inbox))
	 (dir (mew-expand-folder case:inbox))
	 mailbox)
    (unless (file-directory-p dir)
      (mew-make-directory dir))
    (mew-summary-switch-to-folder case:inbox)
    ;; in the inbox
    ;; for mew-summary-exchange-point.
    (set-marker mew-summary-inbox-position (point) (current-buffer))
    (when (mew-summary-exclusive-p)
      (mew-window-configure 'summary)
      ;;
      (mew-current-set nil nil nil)
      (mew-decode-syntax-delete)
      (mew-unhighlight-cursor-line)
      ;;
      (if (mew-folder-localp inbox) (mew-summary-retrieve-gap case:inbox))
      ;;
      (goto-char (point-max))
      (mew-sinfo-set-start-point (point))
      (mew-sinfo-set-direction 'down)
      (cond
       ((mew-folder-localp inbox)
	(setq mailbox (mew-mailbox-type case))
	(cond
	 ((eq mailbox 'pop)
	  (mew-pop-retrieve case 'inc inbox))
	 ((eq mailbox 'imap)
	  (mew-imap-retrieve case 'inc inbox))
	 ((eq mailbox 'mbox)
	  (mew-mbox-retrieve case 'inc inbox))))
       ((mew-folder-popp inbox)
	(mew-pop-retrieve case 'scan case:inbox nil 'get-body))
       ((mew-folder-imapp inbox)
	(mew-imap-retrieve case 'scan case:inbox nil 'get-body))
       ((mew-folder-nntpp inbox)
	(mew-nntp-retrieve case 'scan case:inbox nil 'get-body))))))

(defun mew-mbox-retrieve (case directive inbox &optional flush)
  ;; directive is 'inc
  (let ((mbox-command (mew-mbox-command case))
	(mbox-command-arg (mew-mbox-command-arg case))
	opts)
    (if (not (mew-which-exec mbox-command))
	(message "'%s' not found!" mbox-command)
      (if mbox-command-arg
	  (setq opts (list "-e" mbox-command "-m" mbox-command-arg))
	(setq opts (list "-e" mbox-command)))
      (mew-local-retrieve 'inc inbox flush opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Retrieving truncated messages
;;;

(defun mew-summary-retrieve-message (&optional rev-del)
  "Retrieve the rest of a truncated('T') message.

In a LOCAL folder: a method to retrieve the message is determined by
'mew-input-case' and 'mew-mailbox-type'. If
'mew-pop-delete'/'mew-imap-delete' is non-nil, delete the message from
the mailbox. When executed with '\\[universal-argument]',
'mew-pop-delete'/'mew-imap-delete' is considered reversed.

In a REMOTE folder: case and protocol are determined by the folder.
The message in the server side is always retained."
  (interactive "P")
  (mew-summary-msg
   (mew-summary-only
    (unless (mew-summary-msg-invalidp)
      (when (mew-summary-exclusive-p)
	(let* ((bnm (mew-summary-folder-name 'ext))
	       (folder (mew-sinfo-get-folder))
	       (case (mew-sinfo-get-case))
	       msg uid siz del rtr rtrs mailbox)
	  (when (mew-folder-localp folder)
	    (setq case mew-case-input)
	    (setq mailbox (mew-mailbox-type case))
	    (cond
	     ((eq mailbox 'pop)
	      (setq del (eq (mew-pop-delete case) t)));; delete may be number
	     ((eq mailbox 'imap)
	      (setq del (eq (mew-imap-delete case) t))));; delete may be number
	    (if rev-del (setq del (not del))))
	  (setq uid (mew-summary-message-uid))
	  (when uid
	    (setq msg (mew-summary-message-number))
	    (setq siz (mew-summary-message-size))
	    (setq rtr (list uid siz del (list bnm msg)))
	    (setq rtrs (list rtr)))
	  (if (not rtrs)
	      (message "No message to be retrieved")
	    (cond
	     ((mew-folder-localp folder)
	      (cond
	       ((eq mailbox 'pop)
		(mew-pop-retrieve case 'get bnm rtrs))
	       ((eq mailbox 'imap)
		(mew-imap-retrieve case 'get bnm rtrs))))
	     ((mew-folder-popp folder)
	      (mew-pop-retrieve case 'get bnm rtrs))
	     ((mew-folder-imapp folder)
	      (mew-imap-retrieve case 'get bnm rtrs))
	     ((mew-folder-nntpp folder)
	      (mew-nntp-retrieve case 'get bnm rtrs))))))))))

(defun mew-summary-mark-retrieve-message (&optional rev-del)
  "Retrieve the rest of truncated('T') messages marked with '*'.

In a LOCAL folder: a method to retrieve the messages is determined by
'mew-input-case' and 'mew-mailbox-type'.  If
'mew-pop-delete'/'mew-imap-delete' is non-nil, delete the messages
from the mailbox.  When executed with '\\[universal-argument]',
'mew-pop-delete'/'mew-imap-delete' is considered reversed.

In a REMOTE folder: case and protocol are determined by the folder.
The messages in the server side is always retained."
  (interactive "P")
  (mew-summary-only
   (let* ((bnm (mew-summary-folder-name 'ext))
	  (folder (mew-sinfo-get-folder))
	  (case (mew-sinfo-get-case))
	  msg uid siz del rtr rtrs mailbox)
     (when (mew-folder-localp folder)
       (setq case mew-case-input)
       (setq mailbox (mew-mailbox-type case))
       (cond
	((eq mailbox 'pop)
	 (setq del (eq (mew-pop-delete case) t))) ;; delete may be number
	((eq mailbox 'imap)
	 (setq del (eq (mew-imap-delete case) t)))) ;; delete may be number
       (if rev-del (setq del (not del))))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward mew-regex-msg-valid-review nil t)
	 (setq uid (mew-summary-message-uid))
	 (when uid
	   (setq msg (mew-summary-message-number))
	   (setq siz (mew-summary-message-size))
	   (setq rtr (list uid siz del (list bnm msg)))
	   (setq rtrs (cons rtr rtrs)))
	 (forward-line)))
     (if (not rtrs)
	 (message "No message to be retrieved")
       (setq rtrs (nreverse rtrs))
       (cond
	((mew-folder-localp folder)
	 (cond
	  ((eq mailbox 'pop)
	   (mew-pop-retrieve case 'get bnm rtrs))
	  ((eq mailbox 'imap)
	   (mew-imap-retrieve case 'get bnm rtrs))))
	((mew-folder-popp folder)
	 (mew-pop-retrieve case 'get bnm rtrs))
	((mew-folder-imapp folder)
	 (mew-imap-retrieve case 'get bnm rtrs))
	((mew-folder-nntpp folder)
	 (mew-nntp-retrieve case 'get bnm rtrs)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder clean up
;;;

(defun mew-folder-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-folder-clean-up)
  (mew-local-folder-clean-up)
  (mew-imap-folder-clean-up)
  (mew-nntp-folder-clean-up))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message packing
;;;

(defun mew-net-msg-pack (msgs)
  ;; (mew-net-msg-pack '("1" "3" "4" "5" "7" "8" "10"))
  ;; => ("1" "3:5" "7:8" "10")
  (let (pack a b s)
    (when (setq a (car msgs))
      (setq a (string-to-int a))
      (setq msgs (cdr msgs))
      (while (setq b (car msgs))
	(setq b (string-to-int b))
	(setq msgs (cdr msgs))
	(if (= (1+ a) b)
	    (if (null s)
		(setq s a))
	  (if (null s)
	      (setq pack (cons (format "%d" a) pack))
	    (setq pack (cons (format "%d:%d" s a) pack))
	    (setq s nil)))
	(setq a b))
      (if s
	  (setq pack (cons (format "%d:%d" s a) pack))
	(setq pack (cons (format "%d" a) pack)))
      (nreverse pack))))

(defvar mew-imap-message-cat-size 10)

(defun mew-net-msg-cat (lst)
  (let ((N (1- mew-imap-message-cat-size))
	(crn lst)
	prv nxt ret)
    (while (setq prv (nthcdr N crn))
      (setq nxt (cdr prv))
      (setcdr prv nil)
      (setq ret (cons (mew-join "," crn) ret))
      (setq crn nxt))
    (if crn (setq ret (cons (mew-join "," crn) ret)))
    (nreverse ret)))

(defun mew-net-msg-group (lst)
  (mew-net-msg-cat (mew-net-msg-pack lst)))

(provide 'mew-net)

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

;;; mew-net.el ends here
