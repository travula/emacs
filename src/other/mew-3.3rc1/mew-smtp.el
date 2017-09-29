;;; mew-smtp.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec  3, 1999

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SMTP info
;;;

(defvar mew-smtp-info-list
  '(;; parameters to be saved
    "raw-header" "recipients" "orig-recipients"
    "bcc" "dcc" "fcc" "msgid" "logtime"
    "case" ;; save for re-edit, not for sending
    ;; parameters used internally
    "server" "port" "ssh-server"
    "user" "auth-list"
    "helo-domain" 
    "status" "ssh-process" "ssl-process" "messages"
    ;; parameters used internally and should be initialized
    "string" "error" "auth-selected" "timer" "cont" "from" "sender"
    "done"))

(mew-info-defun "mew-smtp-" mew-smtp-info-list)

(defvar mew-smtp-info-list-save-length 9)
(defvar mew-smtp-info-list-clean-length 19)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-smtp-fsm
  '(("greeting"      ("220" . "ehlo"))
    ("ehlo"          ("250" . "auth") (t . "helo"))
    ;;
    ("auth"          ("250" . "mail-from"))
    ("auth-cram-md5" ("334" . "pwd-cram-md5") (t . "wpwd"))
    ("pwd-cram-md5"  ("235" . "mail-from") (t . "wpwd"))
    ("auth-login"    ("334" . "pwd-login") (t . "wpwd"))
    ("pwd-login"     ("235" . "mail-from") (t . "wpwd"))
    ("auth-plain"    ("235" . "mail-from") (t . "wpwd"))
    ;;
    ("helo"          ("250" . "mail-from"))
    ("mail-from"     ("250" . "rcpt-to"))
    ("rcpt-to"       ("250" . "data"))
    ("data"          ("354" . "content"))
    ("content"       ("250" . "done"))
    ("quit"          (t     . "noop"))))

(defsubst mew-smtp-fsm-by-status (status)
  (assoc status mew-smtp-fsm))

(defsubst mew-smtp-fsm-next (status code)
  (cdr (mew-assoc-match2 code (cdr (mew-smtp-fsm-by-status status)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters
;;;

(defun mew-smtp-command-ehlo (pro pnm)
  (let ((helo-domain (mew-smtp-get-helo-domain pnm)))
    (mew-smtp-process-send-string pro "EHLO %s" helo-domain)))

(defun mew-smtp-command-helo (pro pnm)
  (let ((helo-domain (mew-smtp-get-helo-domain pnm)))
    (mew-smtp-process-send-string pro "HELO %s" helo-domain)))

(defun mew-smtp-command-auth (pro pnm)
  (cond
   ((mew-smtp-get-user pnm)
    (let ((auth-list (mew-smtp-get-auth-list pnm))
	  (str (mew-smtp-get-string pnm)) auth func)
      (if (and (string-match "AUTH \\([^\n\r]+\\)\r?\n" str)
	       (setq auth (mew-auth-select (match-string 1 str) auth-list))
	       (setq func (mew-smtp-auth-get-func auth))
	       (fboundp func))
	  (progn
	    (mew-smtp-set-auth-selected pnm auth)
	    (funcall func pro pnm))
	(mew-smtp-debug "<AUTH>" "No preferred SMTP AUTH.\n")
	;; xxx how should we act?
	(mew-smtp-set-status pnm "mail-from")
	(mew-smtp-command-mail-from pro pnm))))
   (t
    (mew-smtp-set-status pnm "mail-from")
    (mew-smtp-command-mail-from pro pnm))))

(defun mew-smtp-command-wpwd (pro pnm)
  (let ((auth (mew-smtp-get-auth-selected pnm)))
    (mew-passwd-set-passwd (mew-smtp-passtag pnm auth) nil)
    (mew-smtp-recover pro pnm (format "Wrong password for %s" auth))))

(defun mew-smtp-command-mail-from (pro pnm)
  (widen)
  (clear-visited-file-modtime)
  ;;
  (let* ((case (mew-smtp-get-case pnm))
	 (resentp (mew-header-existp mew-resent-from:))
	 (sender: (if resentp mew-resent-sender: mew-sender:))
	 (from: (if resentp mew-resent-from: mew-from:))
	 (from (mew-header-get-value from:))
	 (froms (mew-addrstr-parse-address-list from))
	 (nfrom (length froms))
	 (mail-from (mew-smtp-mail-from case)))
    (mew-smtp-set-from pnm from) ;; for Bcc:
    (unless mail-from
      ;; Which address is suitable for MAIL FROM if multiple?
      (setq mail-from (car froms)))
    (unless (mew-header-existp sender:)
      (if (= nfrom 1)
	  (if (and mew-use-sender (not (string= mail-from (car froms))))
	      (mew-smtp-set-sender pnm (cons sender: mail-from)))
	(mew-smtp-set-sender pnm (cons sender: mail-from))))
    ;;
    (mew-smtp-process-send-string pro "MAIL FROM:<%s>" mail-from)))

(defun mew-smtp-command-rcpt-to (pro pnm)
  (let* ((recipients (mew-smtp-get-recipients pnm))
	 (recipient (car recipients)))
    (setq recipients (cdr recipients))
    (mew-smtp-set-recipients pnm recipients)
    (if recipients (mew-smtp-set-status pnm "mail-from"))
    (mew-smtp-process-send-string pro "RCPT TO:<%s>" recipient)))

(defun mew-smtp-command-data (pro pnm)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (mew-dot-insert)
  (mew-eol-fix-for-write)
  (set-buffer-modified-p nil)
  (mew-smtp-set-cont pnm (point-min))
  (mew-smtp-set-timer pnm nil)
  (mew-smtp-process-send-string pro "DATA"))

(defun mew-smtp-command-content (pro pnm)
  (save-excursion
    (let ((cont (mew-smtp-get-cont pnm))
	  (sender (mew-smtp-get-sender pnm))
          (inc 1000) (i 0) (N 10))
      (set-buffer (process-buffer pro))
      ;; Sender:
      (when sender
	(mew-smtp-process-send-string pro "%s %s" (car sender) (cdr sender))
	(mew-smtp-set-sender pnm nil))
      ;;
      (while (and (< cont (point-max)) (not (input-pending-p)) (< i N))
        (let ((next (min (point-max) (+ cont inc))))
          (process-send-region pro cont next) ;; xxx
          (setq cont next)
          (setq i (1+ i))))
      (mew-smtp-set-cont pnm cont)
      (if (< cont (point-max))
          (let ((timer
                 (if (input-pending-p)
                     (run-with-idle-timer
		      0.01 nil 'mew-smtp-command-content pro pnm)
                   (run-at-time 0.05 nil 'mew-smtp-command-content pro pnm))))
            (mew-smtp-set-timer pnm timer))
        (mew-smtp-set-cont pnm nil)
        (mew-smtp-set-timer pnm nil)
        (mew-smtp-process-send-string pro ".")))))

(defun mew-smtp-command-done (pro pnm)
  (let ((fcc (mew-smtp-get-fcc pnm))
	(back (mew-queue-backup (buffer-file-name) mew-queue-info-suffix))
	(buf (process-buffer pro))
	msgs)
    ;; mew-folder-new-message may be slow if the folder contains
    ;; a lot of messages. So, let's Fcc in background.
    (mew-net-fcc-message fcc back)
    (mew-smtp-log pnm)
    (if (mew-smtp-get-bcc pnm)
	(mew-smtp-bcc pro pnm back)
      ;; killing buffer
      (set-process-buffer pro nil)
      (mew-remove-buffer buf)
      (setq msgs (mew-smtp-get-messages pnm))
      (if (and msgs
	       (mew-queue-get-next
		pnm msgs
		mew-smtp-info-list-save-length
		'mew-smtp-set-messages))
	  ;; A file inserted. Now in msg's buffer
	  (progn
	    (mew-info-clean-up pnm mew-smtp-info-list-clean-length)
	    (set-process-buffer pro (current-buffer))
	    (mew-smtp-set-status pnm "mail-from")
	    (mew-smtp-command-mail-from pro pnm))
	(mew-smtp-set-status pnm "quit")
	(mew-smtp-command-quit pro pnm)))))

(defun mew-smtp-command-quit (pro pnm)
  (mew-smtp-set-done pnm t)
  (mew-smtp-process-send-string pro "QUIT"))

(defun mew-smtp-command-noop (pro pnm)
  ())

(defun mew-smtp-recover (pro pnm err)
  (let ((case (mew-smtp-get-case pnm)))
    (mew-smtp-log pnm err)
    (mew-smtp-queue case err)
    (when (and (processp pro) (eq (process-status pro) 'open))
      (mew-smtp-set-status pnm "quit")
      (mew-smtp-command-quit pro pnm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AUTH
;;;

(defvar mew-smtp-auth-alist
  '(("CRAM-MD5" mew-smtp-command-auth-cram-md5)
    ("PLAIN"    mew-smtp-command-auth-plain)
    ("LOGIN"    mew-smtp-command-auth-login)))

(defsubst mew-smtp-auth-get-func (auth)
  (nth 1 (mew-assoc-case-equal auth mew-smtp-auth-alist 0)))

;;

(defun mew-smtp-command-auth-cram-md5 (pro pnm)
  (mew-smtp-process-send-string pro "AUTH CRAM-MD5")
  (mew-smtp-set-status pnm "auth-cram-md5"))

(defun mew-smtp-command-pwd-cram-md5 (pro pnm)
  (let ((str (mew-smtp-get-string pnm))
	(user (mew-smtp-get-user pnm))
	challenge passwd cram-md5)
    (if (string-match " \\([a-zA-Z0-9+/]+=*\\)" str) ;; xxx
	(setq challenge (match-string 1 str)))
    (setq passwd (mew-smtp-input-passwd "CRAM-MD5 password: " pnm "cram-md5"))
    (setq cram-md5 (mew-cram-md5 user passwd challenge))
    (mew-smtp-process-send-string pro cram-md5)))

(defun mew-smtp-command-auth-login (pro pnm)
  (let* ((user (mew-smtp-get-user pnm))
	 (euser (mew-base64-encode-string user)))
    (mew-smtp-process-send-string pro "AUTH LOGIN %s" euser)
    (mew-smtp-set-status pnm "auth-login")))

(defun mew-smtp-command-pwd-login (pro pnm)
  (let* ((passwd (mew-smtp-input-passwd "LOGIN password: " pnm "login"))
	 (epasswd (mew-base64-encode-string passwd)))
    (mew-smtp-process-send-string pro epasswd)))

(defun mew-smtp-command-auth-plain (pro pnm)
  (let* ((passwd (mew-smtp-input-passwd "PLAIN password: " pnm "plain"))
	 (user (mew-smtp-get-user pnm))
	 (plain (mew-base64-encode-string (format "\0%s\0%s" user passwd))))
    (mew-smtp-process-send-string pro "AUTH PLAIN %s" plain)
    (mew-smtp-set-status pnm "auth-plain")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-smtp-info-prefix "mew-smtp-info-")

(defsubst mew-smtp-info-name (case)
  (let ((server (mew-smtp-server case))
	(port (mew-smtp-port case))
	(user (mew-smtp-user case))
	(sshsrv (mew-smtp-ssh-server case))
	(name mew-smtp-info-prefix))
    (if user
	(setq name (concat name user "@" server))
      (setq name (concat name server)))
    (unless (string= port mew-smtp-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defun mew-smtp-process-send-string (pro &rest args)
  (let ((str (apply 'format args)))
    (mew-smtp-debug "=SEND=" str)
    (process-send-string pro (concat str mew-cs-eol))))

(defun mew-smtp-passtag (pnm auth)
  (concat (downcase auth)
	  "/" (mew-smtp-get-user pnm)
	  "@" (mew-smtp-get-server pnm)
	  ":" (mew-smtp-get-port pnm)))

(defun mew-smtp-input-passwd (prompt pnm auth)
  (let ((tag (mew-smtp-passtag pnm auth)))
    (mew-input-passwd prompt tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening SMTP
;;;

(defun mew-smtp-open (pnm server port)
  (let ((sprt (mew-port-sanity-check port))
	pro tm errmsg)
    (condition-case emsg
	(progn
	  (setq tm (mew-timer mew-smtp-timeout-time 'mew-smtp-timeout))
	  (message "Connecting to the SMTP server...")
	  (setq pro (open-network-stream pnm nil server sprt))
	  (process-kill-without-query pro)
	  (mew-set-process-cs pro mew-cs-text-for-net mew-cs-text-for-net)
	  (message "Connecting to the SMTP server...done"))
      (quit
       (setq errmsg "Cannot connect to the SMTP server")
       (mew-smtp-set-error pnm errmsg)
       (setq pro nil)
       (message errmsg))
      (error
       (setq errmsg (format "%s, %s" (nth 1 emsg) (nth 2 emsg)))
       (mew-smtp-set-error pnm errmsg)
       (setq pro nil)
       (message errmsg)))
    (if tm (cancel-timer tm))
    pro))

(defun mew-smtp-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-smtp-send-message (case)
  (let* ((server (mew-smtp-server case))
	 (port (mew-smtp-port case))
	 (pnm (mew-smtp-info-name case))
	 (sshsrv (mew-smtp-ssh-server case))
	 (sslp (mew-smtp-ssl case))
	 (sslport (mew-smtp-ssl-port case))
	 process sshname sshpro sslname sslpro lport errmsg)
    (cond
     ((null (mew-smtp-get-recipients pnm))
      (message "No recipient!"))
     ((get-process pnm)
      (setq errmsg "SMTP connection is locked")
      (mew-smtp-set-error pnm errmsg)
      (mew-smtp-queue case errmsg))
     (t
      (cond
       (sshsrv
	(setq sshpro (mew-open-ssh-stream server port sshsrv))
	(when sshpro
	  (setq sshname (process-name sshpro))
	  (setq lport (mew-ssh-pnm-to-lport sshname))
	  (when lport
	    (setq process (mew-smtp-open pnm "localhost" lport)))))
       (sslp
	(setq sslpro (mew-open-ssl-stream server sslport))
	(when sslpro
	  (setq sslname (process-name sslpro))
	  (setq lport (mew-ssl-pnm-to-lport sslname))
	  (when lport
	    (setq process (mew-smtp-open pnm "localhost" lport)))))
       (t
	(setq process (mew-smtp-open pnm server port))))
      (if (null process)
	  (progn
	    (cond
	     ((and sshsrv (null sshpro))
	      (setq errmsg "Cannot create to the SSH connection"))
	     ((and sslp (null sslpro))
	      (setq errmsg "Cannot create to the SSL connection"))
	     (t
	      (setq errmsg "Cannot connect to the SMTP server")))
	    (mew-smtp-set-error pnm errmsg)
	    (mew-smtp-queue case errmsg))
	(mew-info-clean-up pnm mew-smtp-info-list-clean-length)
	(mew-smtp-set-case pnm case)
	(mew-smtp-set-server pnm server)
	(mew-smtp-set-port pnm port)
	(mew-smtp-set-ssh-server pnm sshsrv)
	(mew-smtp-set-ssh-process pnm sshpro)
	(mew-smtp-set-ssl-process pnm sslpro)
	(mew-smtp-set-helo-domain pnm (mew-smtp-helo-domain case))
	(mew-smtp-set-user pnm (mew-smtp-user case))
	(mew-smtp-set-auth-list pnm (mew-smtp-auth-list case))
	(mew-smtp-set-status pnm "greeting")
	(set-process-buffer process (current-buffer))
	(set-process-sentinel process 'mew-smtp-sentinel)
	(set-process-filter process 'mew-smtp-filter)
	(message "Sending in background..."))))))

(defun mew-smtp-flush-queue (case &optional qfld)
  (let* ((pnm (mew-smtp-info-name case))
	 msgs)
    (unless qfld
      (setq qfld (mew-queue-folder case)))
    (if (get-process pnm)
	(message "SMTP connection for %s is locked" qfld)
      (mew-summary-folder-cache-clean qfld)
      (setq msgs (directory-files (mew-expand-folder qfld)
				  'full mew-regex-message-files))
      (when (and msgs
		 (mew-queue-get-next
		  pnm msgs
		  mew-smtp-info-list-save-length
		  'mew-smtp-set-messages))
	;; A file inserted. Now in msg's buffer
	(run-hooks 'mew-smtp-flush-hook)
	(message "Flushing %s..." qfld)
	(mew-smtp-send-message case)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-smtp-debug (label string)
  (when (mew-debug 'net)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-smtp-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-smtp-get-status pnm))
	 (str (concat (mew-smtp-get-string pnm) string))
	 next func code)
    (mew-smtp-debug (upcase status) string)
    (mew-filter
     ;; SMTP server's strings should be short enough.
     (mew-smtp-set-string pnm str)
     (cond
      ((and (string-match "\n$" str)
	    (string-match "^\\([1-5][0-7][0-9]\\) " str))
       (setq code (match-string 1 str))
       (setq next (mew-smtp-fsm-next status code))
       (cond
	(next
	 (mew-smtp-set-status pnm next)
	 (setq func (intern-soft (concat "mew-smtp-command-" next)))
	 (and func (funcall func process pnm))
	 (mew-smtp-set-string pnm nil))
	(t
	 (if (string-match "^pwd-" status)
	     (message "Password is wrong"))
	 (mew-smtp-recover process pnm str))))
      (t ()))))) ;; stay

(defun mew-smtp-sentinel (process event)
  (let* ((pnm (process-name process))
	 (done (mew-smtp-get-done pnm))
	 (sshpro (mew-smtp-get-ssh-process pnm))
	 (sslpro (mew-smtp-get-ssl-process pnm)))
    (mew-smtp-debug "SMTP SENTINEL" event)
    (mew-info-clean-up pnm)
    (if (and (processp sshpro) (not mew-ssh-keep-connection))
	(process-send-string sshpro "exit\n"))
    (if (and (processp sslpro) (not mew-ssl-keep-connection))
	(delete-process sslpro))
    (if done (message "Sending in background...done"))
    (run-hooks 'mew-smtp-sentinel-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queuing
;;;

(defun mew-smtp-queue (case err)
  ;; Must be in a buffer where a message is contained.
  (let* ((pnm (mew-smtp-info-name case))
	 (qfld (mew-queue-folder case))
	 (oname (buffer-name))
	 (work (buffer-file-name))
	 file-info file info nname)
    (mew-local-folder-check qfld)
    (setq file-info (mew-queue-enqueue work qfld))
    (setq file (nth 0 file-info) info (nth 1 file-info))
    (setq nname (mew-concat-folder qfld (file-name-nondirectory file)))
    (if (mew-draft-p)
	(mew-smtp-set-case pnm (mew-tinfo-get-case)))
    ;;
    (mew-smtp-set-recipients pnm (mew-smtp-get-orig-recipients pnm))
    (let* ((n mew-smtp-info-list-save-length)
	   (data (make-vector n nil))
	   (i 0))
      (while (< i n)
	(aset data i (aref (mew-info pnm) i))
	(setq i (1+ i)))
      (mew-lisp-save info data))
    ;;
    (mew-remove-buffer (current-buffer))
    (message "%s has been queued to %s (%s)"
	     oname nname (or (mew-smtp-get-error pnm) err))
    (mew-touch-folder qfld)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bcc:
;;;

(defun mew-smtp-bcc (pro pnm back)
  (let* ((dir (file-name-directory back))
	 (msg (file-name-nondirectory back))
	 (case (mew-smtp-get-case pnm)))
    (mew-elet
     (mew-erase-buffer)
     (mew-set-buffer-multibyte t)
     (mew-smtp-set-recipients pnm (mew-smtp-get-bcc pnm))
     (mew-smtp-set-orig-recipients pnm (mew-smtp-get-bcc pnm))
     (mew-smtp-set-bcc pnm nil)
     ;;
     (mew-encode-id-date pnm (mew-smtp-message-id case)) ;; save-excursion
     (goto-char (point-max))
     (mew-draft-header-insert mew-to: "Bcc-Receiver:;")
     (mew-draft-header-insert mew-subj: mew-bcc-subject)
     (mew-draft-header-insert mew-from: (mew-smtp-get-from pnm))
     (mew-draft-header-insert mew-organization: (mew-organization case))
     (mew-draft-header-insert-alist (mew-header-alist case))
     ;; X-Mailer: must be the last
     (mew-draft-header-insert mew-x-mailer: mew-x-mailer)
     (mew-header-set (concat mew-header-separator "\n"))
     (insert mew-bcc-body)
     (goto-char (mew-header-end))
     (forward-line)
     (setq mew-encode-syntax (mew-encode-syntax-initial dir))
     (setq mew-encode-syntax
	   (mew-syntax-insert-entry
	    mew-encode-syntax
	    '(2)
	    (mew-encode-syntax-single msg mew-type-msg nil nil nil)))
     (mew-encode-multipart mew-encode-syntax dir 0 'buffered)
     (mew-encode-make-header)
     (mew-encode-save-draft)
     (mew-overlay-delete-buffer)
     (mew-info-clean-up pnm mew-smtp-info-list-clean-length)
     (set-process-buffer pro (current-buffer))
     (mew-smtp-set-status pnm "mail-from")
     (mew-smtp-command-mail-from pro pnm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Logging
;;;

(defun mew-smtp-log (pnm &optional err)
  (let ((logtime (mew-smtp-get-logtime pnm))
	(msgid (mew-smtp-get-msgid pnm))
	(recipients (mew-smtp-get-orig-recipients pnm))
	(server (mew-smtp-get-server pnm))
	(sshsrv (mew-smtp-get-ssh-server pnm))
	(sslp (mew-smtp-get-ssl-process pnm)))
    (with-temp-buffer
      (and logtime (insert logtime))
      (and msgid (insert " id=" msgid))
      (and server (insert " server=" server))
      (and sshsrv (insert " sshsrv=" sshsrv))
      (and sslp (insert " SSL"))
      (and recipients
	   (setq recipients (mapconcat 'identity recipients ",")))
      (and recipients (insert " recipients=" recipients))
      (if err
	  (insert " status=" "("
                  (substring err 0 (string-match "\n+$" err))
                  ")")
	(insert " status=sent"))
      (insert "\n")
      (write-region (point-min) (point-max)
		    (expand-file-name mew-smtp-log-file mew-conf-path)
		    'append 'no-msg))))

(provide 'mew-smtp)

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

;;; mew-smtp.el ends here
