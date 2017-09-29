;;; mew-ssh.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 14, 1999

;;; Code:

(require 'mew)

(defvar mew-ssh-ver nil)		;; 0-SSH1, 1-SSH2, 2-OpenSSH

(defsubst mew-ssh-get (list-or-vec)
  (elt list-or-vec mew-ssh-ver))

(defconst mew-ssh-msg-passwd 
  '("password:\\|Enter passphrase"
    "password:\\|Passphrase for\\|Enter passphrase"
    "password:\\|Enter passphrase"))

(defconst mew-ssh-msg-connected
  '("Entering interactive session"
    "Authentication successful\\|client_authenticated"
    "Entering interactive session"))

(defconst mew-ssh-msg-denied
  '("Permission denied\\." "Permission denied\\." "Permission denied\\."))

(defconst mew-ssh-msg-refused
  '("Secure connection .* refused\\."
    "FATAL: Connecting .* failed:"
    "Secure connection .* refused\\."))

(defconst mew-ssh-process-exec-cnt 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSH info
;;;

(defvar mew-ssh-info-list '("status" "try"))

(mew-info-defun "mew-ssh-" mew-ssh-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process name
;;;

(defconst mew-ssh-info-prefix "mew-ssh-info-")

(defsubst mew-ssh-info-name (sshserver server remoteport localport)
  (format "%s:%s:%s:%s:%d"
	  mew-ssh-info-prefix sshserver server remoteport localport))

(defsubst mew-ssh-info-name-regex (sshserver server remoteport)
  (format "^%s:%s:%s:%s" mew-ssh-info-prefix sshserver server remoteport))

(defsubst mew-ssh-pnm-to-lport (pnm)
  (if (string-match ":\\([0-9]+\\)$" pnm) (match-string 1 pnm)))

;; user@host. port should be used here.
;; However, SSH does not symbolic port name "ssh" for the "-p" option
;; while OpenSSH does. Sigh...
(defsubst mew-ssh-passtag (name)
  (if (string-match (format "^%s:[^:]+:" mew-ssh-info-prefix) name)
      (match-string 0 name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing SSH
;;;

(defun mew-open-ssh-stream (server serv sshserver)
  "Open SSH stream for SERVER's SERV via SSHSERVER.
This function returns a process when an SSH connection is created
successfully. A local port number can be obtained the process name
after ':'. "
  (cond
   ((not (and (stringp server) (stringp serv) (stringp sshserver)))
    ())
   ((not (mew-which-exec mew-prog-ssh))
    ())
   (t
    (let* ((remoteport (mew-serv-to-port serv))
	   (localport (+ 8000 (% (mew-random) 4000)))
	   (process-connection-type mew-connection-type2)
	   (i 0) (N mew-ssh-process-exec-cnt)
	   (pros (process-list))
	   (regex (mew-ssh-info-name-regex sshserver server remoteport))
	   buf name pnm pro status)
      (catch 'find
	(while pros
	  (when (string-match regex (process-name (car pros)))
	    (if (memq (process-status (car pros)) '(run))
		(setq pro (car pros))
	      (delete-process (car pros)))
	    (throw 'find nil))
	  (setq pros (cdr pros))))
      (if pro
	  pro
	(message "Connecting to the SSH server...")
	(setq pro nil)
	(setq buf (generate-new-buffer (concat mew-buffer-prefix "ssh")))
	(catch 'loop
	  (while (< i N)
	    (setq name (mew-ssh-info-name sshserver server remoteport localport))
	    (setq pro
		  (apply 'start-process
			 name buf
			 mew-prog-ssh "-x" "-v"
			 "-o" (concat 
			       (if (= mew-ssh-ver 2)
				   "NumberOfPasswordPrompts " ;; OpenSSH style
				 "NumberOfPasswordPrompts=")  ;; SSH style
			       (int-to-string mew-ssh-number-of-password-prompts))
			 (format "-L%d:%s:%s" localport server remoteport)
			 sshserver
			 mew-prog-ssh-arg))
	    ;; An error would occur. So, let's exit in the case.
	    (mew-timing)
	    (cond
	     ((not (processp pro))
	      (message "Connecting to the SSH server...FAILED")
	      (throw 'loop nil))
	     ((not (memq (process-status pro) '(run)))
	      (delete-process pro)
	      (message "Connecting to the SSH server...FAILED")
	      (throw 'loop nil)))
	    ;; ssh is now running.
	    (process-kill-without-query pro)
	    (setq pnm (process-name pro))
	    (mew-info-clean-up pnm)
	    (mew-ssh-set-try pnm 0)
	    (mew-set-process-cs pro mew-cs-text-for-read mew-cs-text-for-write)
	    (set-process-filter pro 'mew-ssh-filter)
	    (set-process-sentinel pro 'mew-ssh-sentinel)
	    (mew-rendezvous (null (mew-ssh-get-status pnm)))
	    (setq status (mew-ssh-get-status pnm))
	    (cond
	     ((eq status t)
	      (throw 'loop pro)) ;; return value
	     ((not (eq status 'bound))
	      (message "Connecting to the SSH server...FAILED")
	      (throw 'loop nil))) ;; return value
	    ;; 'bound
	    (setq localport (1+ localport))
	    (setq i (1+ i))
	    (message "Connecting to the SSH server...FAILED")
	    nil))))))) ;; return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-ssh-debug (label string)
  (when (mew-debug 'net)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-ssh-filter (process string)
  (mew-ssh-debug "SSH: " string)
  (let ((pnm (process-name process)) pass)
    (cond
     ((string-match (mew-ssh-get mew-ssh-msg-passwd) string)
      (if (= (mew-ssh-get-try pnm) 0)
	  (setq pass (mew-input-passwd "SSH password: " (mew-ssh-passtag pnm)))
	(mew-passwd-set-passwd (mew-ssh-passtag pnm) nil)
	(setq pass (mew-input-passwd "SSH password again: "
				     (mew-ssh-passtag pnm))))
      (mew-ssh-set-try pnm (1+ (mew-ssh-get-try pnm)))
      (if (and (processp process)
	       (memq (process-status process) '(run)))
	  (progn
	    (message "Sending password to the SSH server...")
	    (process-send-string process (concat pass "\n")))
	(mew-passwd-set-passwd (mew-ssh-passtag pnm) nil)
	(message "Cannot find SSH process")))
     ((or (string-match (mew-ssh-get mew-ssh-msg-denied) string)
	  (string-match (mew-ssh-get mew-ssh-msg-refused) string))
      (message "Cannot connect to the SSH server")
      (mew-passwd-set-passwd (mew-ssh-passtag pnm) nil)
      (mew-ssh-set-status pnm 'denied))
     ((string-match (mew-ssh-get mew-ssh-msg-connected) string)
      (mew-ssh-set-status pnm t)
      (message "Connecting to the SSH server...done"))
     ((string-match "Local: bind: Address already in use" string)
      (mew-ssh-set-status pnm 'bound)
      (message "Connecting to the SSH server...done")))))

(defun mew-ssh-sentinel (process event)
  (let ((pnm (process-name process)))
    (unless (mew-ssh-get-status pnm)
      (mew-ssh-set-status pnm 'exit))
    (mew-remove-buffer (process-buffer process))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSH version check 
;;;

(defun mew-ssh-setup ()
  (if (not (mew-which-exec mew-prog-ssh))
      (setq mew-ssh-ver nil)
    (with-temp-buffer
      (call-process mew-prog-ssh nil t nil "-V")
      (goto-char (point-min))
      (cond
       ((looking-at "SSH Version 1")
	(setq mew-ssh-ver 0))
       ((looking-at (concat mew-prog-ssh ": SSH Version 2"))
	(setq mew-ssh-ver 1))
       ((looking-at (concat mew-prog-ssh ": SSH Secure Shell [23]"))
	(setq mew-ssh-ver 1))
       ((looking-at "\\(SSH Version \\)?\\(OpenSSH\\|NetBSD\\|Sun_SSH\\)")
	(setq mew-ssh-ver 2))
       (t
	(setq mew-ssh-ver nil))))))

(provide 'mew-ssh)

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

;;; mew-ssh.el ends here
