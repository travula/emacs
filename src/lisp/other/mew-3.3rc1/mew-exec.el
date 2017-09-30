;;; mew-exec.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  2, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exec-func
;;;

(defun mew-mark-kill-refile (src msg)
  (not (member src (mew-refile-get msg))))

(defun mew-mark-exec-refile (src msgs)
  "Refile MSGs from the SRC folder."
  (let (dsts tmp msg msg-dsts dsts-msgses)
    (while msgs
      (setq msg (car msgs))
      (setq msgs (cdr msgs))
      (setq msg-dsts (mew-refile-get msg))
      (setq dsts (cdr msg-dsts))
      (if dsts ;; sanity check
	  (if (setq tmp (assoc dsts dsts-msgses))
	      (nconc tmp (list msg))	      
	    (setq dsts-msgses (cons (list dsts msg) dsts-msgses))))
      (mew-refile-reset msg))
    ;; refile at once
    (while dsts-msgses
      (mew-summary-mv-msgs src (car dsts-msgses))
      (setq dsts-msgses (cdr dsts-msgses)))))

(defun mew-mark-exec-unlink (src dels)
  "Unlink DELS from the SRC folder.
DELS represents the messages to be deleted."
  (let ((mew-msg-rm-policy 'always))
    (mew-mark-exec-delete src dels)))

(defun mew-mark-exec-delete (src dels)
  "Delete messages from the SRC folder according to mew-msg-rm-policy.
DELS represents the messages to be deleted."
  (when dels
    (let ((unlink-it nil))
      (cond
       ((eq mew-msg-rm-policy 'always)
	(setq unlink-it t))
       ((eq mew-msg-rm-policy 'trashonly)
	(if (string= src mew-trash-folder)
	    (setq unlink-it t)))
       ((eq mew-msg-rm-policy 'uselist)
	(if (mew-member-match src mew-msg-rm-folder-list)
	    (setq unlink-it t))))
      ;; t = 'totrash
      (if unlink-it
	  (mew-summary-unlink-msgs src dels)
	(if (string= src mew-trash-folder)
	    (progn
	      (goto-char (point-min))
	      (while (re-search-forward mew-regex-msg-delete nil t)
		(mew-summary-undo-one)))
	  (mew-summary-totrash-msgs src dels))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sanity-func
;;;

(defun mew-mark-sanity-refile (msgs)
  "Check the destination folder for MSGS."
  (let (msg dst dsts uniq-dsts udst err-dsts dir)
    (while msgs
      (setq msg (car msgs))
      (setq msgs (cdr msgs))
      (setq dsts (cdr (mew-refile-get msg)))
      (while dsts
	(setq dst (car dsts))
	(setq dsts (cdr dsts))
	(mew-addq uniq-dsts dst)))
    (mew-addq uniq-dsts mew-trash-folder)
    (while uniq-dsts
      (setq udst (mew-folder-case-folder mew-inherit-exec-case (car uniq-dsts)))
      (setq uniq-dsts (cdr uniq-dsts))
      (setq dir (mew-expand-folder udst))
      (if (file-exists-p dir)
	  (if (file-directory-p dir)
	      (unless (file-writable-p dir)
		(set-file-modes dir mew-folder-mode))
	    (setq err-dsts (cons udst err-dsts))) ;; NG
	(mew-make-directory dir)))
    err-dsts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unlinking, moving, to-trash
;;;

(defun mew-folder-suffix (folder)
  (if (or (mew-folder-queuep folder) (mew-folder-postqp folder))
      mew-queue-info-suffix
    (if (mew-folder-draftp folder)
	mew-draft-info-suffix)))

(defun mew-summary-unlink-msgs (src dels)
  (let ((suffix (mew-folder-suffix src))
	file infofile)
    (while dels
      (setq file (mew-expand-folder src (car dels)))
      (setq dels (cdr dels))
      ;; if file does not exist, the marked line will be deleted anyway.
      (mew-delete-file file)
      (when suffix
	(setq infofile (concat file suffix))
	(mew-delete-file infofile)))))

(defun mew-i2s (num cache)
  (if cache
      (concat "0" (int-to-string num)) ;; "0" is the mark for invalid messages
    (int-to-string num)))

(defun mew-summary-mv-msgs (src dsts-msgs)
  (let* ((dsts (car dsts-msgs)) ;; (+foo +bar)
	 (msgs (cdr dsts-msgs)) ;; (1 2 3)
	 (srcx (mew-folder-folder src))
	 (myselfp (member srcx dsts))
	 (cache mew-inherit-offline)
	 (suffix (mew-folder-suffix srcx))
	 msgs- msg srcfile dstfile dst num strnum infofile)
    (if myselfp
	;; msg stays in the src folder with the same number
	(progn
	  (setq dsts (delete srcx dsts))
	  (while msgs
	    (setq msg (car msgs))
	    (setq msgs (cdr msgs))
	    (if (file-regular-p (mew-expand-folder src msg))
		(setq msgs- (cons msg msgs-))))
	  (setq msgs- (nreverse msgs-))
	  (setq msgs msgs-))
      (setq dst (mew-folder-case-folder mew-inherit-exec-case (car dsts)))
      (setq dsts (cdr dsts))
      (setq num (string-to-int (mew-folder-new-message dst 'num-only cache)))
      (while msgs
	(setq srcfile (mew-expand-folder src (car msgs)))
	(setq msgs (cdr msgs))
	(when (and (file-exists-p srcfile) (file-writable-p srcfile))
	  (setq strnum (mew-i2s num cache))
	  (setq msgs- (cons strnum msgs-))
	  (setq dstfile (mew-expand-folder dst strnum))
	  (setq num (1+ num))
	  (rename-file srcfile dstfile 'override)
	  (when suffix
	    (setq infofile (concat srcfile suffix))
	    (mew-delete-file infofile))))
      (mew-touch-folder dst)
      (setq msgs- (nreverse msgs-))
      (setq src dst)) ;; myselfp
    (while dsts
      (setq dst (mew-folder-case-folder mew-inherit-exec-case (car dsts)))
      (setq dsts (cdr dsts))
      (setq num (string-to-int (mew-folder-new-message dst 'num-only cache)))
      (setq msgs msgs-)
      (while msgs
	(setq srcfile (mew-expand-folder src (car msgs)))
	(setq msgs (cdr msgs))
	(setq strnum (mew-i2s num cache))
	(setq dstfile (mew-expand-folder dst strnum))
	(setq num (1+ num))
	(mew-link srcfile dstfile))
      (mew-touch-folder dst))))

(defun mew-summary-totrash-msgs (src dels)
  (let* ((trash (mew-folder-case-folder mew-inherit-exec-case mew-trash-folder))
	 (trashdir (mew-expand-folder trash))
	 (cache mew-inherit-offline)
	 (suffix (mew-folder-suffix src))
	 num srcfile dstfile infofile)
    (unless (file-directory-p trashdir) (make-directory trashdir))
    (setq num (string-to-int (mew-folder-new-message trash 'num-only cache)))
    ;; must be here after ensuring that +trash exists.


    (while dels
      (setq srcfile (mew-expand-folder src (car dels))) ;; xxx
      (setq dels (cdr dels))
      (setq dstfile (mew-expand-folder trash (mew-i2s num cache)))
      (setq num (1+ num))
      (if (file-regular-p srcfile)
	  ;; if not, the marked line will be deleted anyway.
	  (rename-file srcfile dstfile 'override))
      (when suffix
	(setq infofile (concat srcfile suffix))
	(mew-delete-file infofile)))
    (mew-touch-folder trash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands for mark processing
;;;

(defun mew-summary-exec (&optional arg)
  "\\<mew-summary-mode-map> Process marked messages. When called with
'\\[universal-argument]', process marked messages in the specified
region. To cancel the '*' mark, use '\\[mew-summary-undo]' or
'\\[mew-summary-undo-all]'."
  (interactive "P")
  (let (beg end region)
    (cond
     (arg
      (setq region (mew-summary-get-region))
      (setq beg (car region))
      (setq end (cdr region)))
     (t
      (setq beg (point-min))
      (setq end (point-max))))
    (mew-summary-exec-region beg end)))

(defun mew-summary-exec-one (&optional arg)
  "Process the current marked message.
When called with '\\[universal-argument]', process 
all marked messages before the current message."
  (interactive "P")
  (mew-summary-goto-message)
  (mew-decode-syntax-delete)
  (save-window-excursion
    (let (beg end)
      (save-excursion
	(beginning-of-line)
	(setq beg (if arg (point-min) (point)))
	(end-of-line)
	(setq end (1+ (point))))
      (if (> end (point-max))
	  (message "No message")
	(mew-summary-exec-region beg end)))))

(defun mew-summary-exec-delete ()
  "Process messages marked with 'D'."
  (interactive)
  (let* ((ent (assoc mew-mark-delete mew-mark-spec))
	 (mew-mark-spec (list ent)))
    (mew-summary-exec-region (point-min) (point-max))))

(defun mew-summary-exec-unlink ()
  "Process messages marked with 'X'."
  (interactive)
  (let* ((ent (assoc mew-mark-unlink mew-mark-spec))
	 (mew-mark-spec (list ent)))
    (mew-summary-exec-region (point-min) (point-max))))

(defun mew-summary-exec-refile ()
  "Process messages marked with 'o'."
  (interactive)
  (let* ((ent (assoc mew-mark-refile mew-mark-spec))
	 (mew-mark-spec (list ent)))
    (mew-summary-exec-region (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exec region
;;;

(defun mew-summary-exec-region (beg end)
  (let ((folder (mew-sinfo-get-folder)))
    (cond
     ((mew-folder-nntpp folder)
      (message "This command cannot be used in this folder"))
     ((mew-folder-remotep folder)
      (message "Refiling and deleting...")
      (sit-for 0)
      (mew-summary-exec-remote beg end))
     ((mew-virtual-p)
      (if (not (mew-thread-p))
	  (message "Move to a physical folder first")
	(let* ((bnm (mew-summary-folder-name 'ext))
	       (fld (mew-thread-to-folder bnm))
	       (msg (mew-summary-message-number)))
	  (if (not (and fld (get-buffer fld)))
	      (message "No physical folder")
	    (mew-summary-visit-folder fld nil 'no-ls)
	    (if msg
		(mew-summary-jump-message msg)
	      (goto-char (point-max)))
	    (message "Now in %s. Type '%s' again"
		     fld
		     (substitute-command-keys
		      "\\<mew-summary-mode-map>\\[mew-summary-exec]"))))))
     (t
      (message "Refiling and deleting...")
      (sit-for 0)
      (mew-summary-exec-local beg end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks for local
;;;

(defsubst mew-mark-kill-line ()
  (let (start)
    (beginning-of-line)
    (setq start (point))
    (forward-line)
    (mew-elet (delete-region start (point)))))

(defun mew-summary-exec-local (beg end)
  (when (mew-summary-exclusive-p)
    (save-excursion
      (condition-case nil
	  (let ((marks (mew-mark-get-all-marks))
		(src (mew-summary-folder-name 'ext))
		(m (make-marker))
		(cnt 0)
		(case-fold-search nil)
		msg msgs err-folders mark func-exec func-sanity killp regex)
	    (set-marker m end)
	    (while marks
	      (setq mark (car marks))
	      (setq marks (cdr marks))
	      (setq func-exec (mew-markdb-func-exec mark))
	      (when func-exec
		(setq killp (mew-markdb-killp mark))
		(setq regex (mew-mark-regex mark))
		(setq msgs nil)
		(goto-char beg)
		(while (re-search-forward regex m t)
		  (setq msg (mew-summary-message-number))
		  (setq msgs (cons msg msgs))
		  (cond
		   ((fboundp killp)
		    (if (funcall killp src msg)
			(mew-mark-kill-line)
		      (mew-mark-delete-here t)))
		   ((eq killp t)
		    (mew-mark-kill-line))))
		(setq msgs (nreverse msgs))
		(when msgs
		  (when (= cnt 0)
		    ;; opening...
		    (mew-summary-lock t "Executing")
		    (mew-window-configure 'summary)
		    (mew-current-set nil nil nil)
		    (mew-decode-syntax-delete))
		  (setq cnt (1+ cnt))
		  ;; refiling and deleting...
		  (setq func-sanity (mew-markdb-func-sanity mark))
		  (when (and func-sanity
			     (setq err-folders (funcall func-sanity msgs)))
		    (mew-summary-unlock)
		    (mew-warn
		     "Nothing proceeded. Folder(s) MUST be a file!: %s"
		     (mew-join "," err-folders)))
		  (funcall func-exec src msgs))))
	    (if (= cnt 0)
		(message "No marks")
	      ;; ending...
	      (mew-cache-clean-up)
	      (mew-summary-reset-mode-line)
	      (mew-summary-folder-cache-save)
	      (set-buffer-modified-p nil)
	      (mew-summary-unlock)
	      (when (and (eobp) (not (pos-visible-in-window-p)))
		(forward-line -1)
		;; This ensures that some messages are displayed in Summary.
		(recenter))
	      (run-hooks 'mew-summary-exec-hook)
	      (message "Refiling and deleting...done")))
	(quit
	 (set-buffer-modified-p nil)
	 (mew-summary-unlock))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks for remote
;;;

(defsubst mew-mark-invisible-line ()
  (let (start)
    (beginning-of-line)
    (setq start (point))
    (forward-line)
    (mew-elet (put-text-property start (point) 'invisible t))))

(defun mew-summary-exec-remote-get-info (beg end modify)
  (let* ((case (mew-sinfo-get-case))
	 (folder (mew-sinfo-get-folder))
	 (trash (mew-imap-trash-folder case))
	 (m (make-marker))
	 siz msg uid ref refs rmvs flds del regex kils
	 refinfo refile-back)
    (if (and trash (string= trash folder)) (setq trash nil))
    (save-excursion
      (set-marker m end)
      (mew-decode-syntax-delete)
      (setq regex (mew-mark-regex mew-mark-refile))
      (goto-char beg)
      (while (re-search-forward regex m t)
	(setq msg (mew-summary-message-number))
	(setq uid (mew-summary-message-uid))
	(setq siz (mew-summary-message-size))
	(setq refinfo (mew-refile-get msg))
	(setq refile-back (cons refinfo refile-back))
	(setq flds (cdr refinfo))
	(if modify (mew-refile-reset msg)) ;; reset before delete just in case
	(setq del nil)
	(if (member folder flds)
	    (setq flds (delete folder flds)) ;; destructive
	  (setq del t)
	  (if modify (setq kils (cons msg kils))))
	(when flds
	  (setq ref (cons uid (cons siz (cons del flds))))
	  (setq refs (cons ref refs)))
	(if modify
	    (if del
		(mew-mark-invisible-line)
	      (mew-mark-delete-here t))))
      (setq refs (nreverse refs))
      (if modify (mew-sinfo-set-refile-back refile-back))
      ;;
      (setq regex (mew-mark-list-regex (list mew-mark-delete mew-mark-unlink)))
      (goto-char beg)
      (while (re-search-forward regex m t)
	(setq msg (mew-summary-message-number))
	(setq uid (mew-summary-message-uid))
	(setq siz (mew-summary-message-size))
	(if trash
	    (setq rmvs (cons (list uid siz t trash) rmvs))
	  (setq rmvs (cons uid rmvs)))
	(when modify
	  (setq kils (cons msg kils))
	  (mew-mark-invisible-line)))
      (setq rmvs (nreverse rmvs))
      (when trash
	(setq refs (nconc rmvs refs))
	(setq rmvs nil))
      (set-buffer-modified-p nil)
      (if modify
	  (progn
	    (sit-for 0)
	    (list refs rmvs kils))
	(list refs rmvs)))))

(defun mew-summary-exec-remote (beg end)
  (if (mew-folder-nntpp (mew-sinfo-get-folder))
      (message "This command cannot be used in this folder")
    (when (mew-summary-exclusive-p)
      (let* ((case (mew-sinfo-get-case))
	     (folder (mew-sinfo-get-folder))
	     (bnm (mew-summary-folder-name 'ext))
	     (info (mew-summary-exec-remote-get-info beg end 'modify))
	     (refs (nth 0 info))
	     (rmvs (nth 1 info))
	     (kils (nth 2 info)))
	(if (or refs rmvs)
	    (cond
	     ((mew-folder-popp folder)
	      (mew-pop-retrieve case 'exec bnm refs rmvs kils))
	     ((mew-folder-imapp folder)
	      (mew-imap-retrieve case 'exec bnm refs rmvs kils)))
	  (message "No messages to be processed"))))))

(defun mew-mark-kill-invisible ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (get-text-property (point) 'invisible)
	  (mew-mark-kill-line)
	(forward-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Processing marks for offline
;;;

(defun mew-summary-exec-offline (&optional arg)
  "\\<mew-summary-mode-map>Process marked messages *offline*. Messages to be refiled in 
a remote folder are moved to the corresponding folder but they are
marked invalid. A number of an invalid message starts with '0'.
Invalid messages will be overridden when the remote folder
is scanned online. 
A job to delete or refile messages, which is created by this command, 
is queued in a queue folder (%queue for IMAP). To flush jobs in 
the queue, type '\\[mew-summary-send-message]' in the queue online."
  (interactive "P")
  (if (not (mew-folder-imapp (mew-sinfo-get-folder)))
      (message "This command cannot be used in this folder")
    (let (beg end region)
      (cond
       (arg
	(setq region (mew-summary-get-region))
	(setq beg (car region))
	(setq end (cdr region)))
       (t
	(setq beg (point-min))
	(setq end (point-max))))
      (message "Refiling and deleting...")
      (sit-for 0)
      (mew-summary-exec-offline-region beg end))))

(defun mew-summary-exec-offline-region (beg end)
  "Process the current marked message offline."
  (interactive)
  (let* ((info (mew-summary-exec-remote-get-info beg end nil))
	 (refs (nth 0 info))
	 (rmvs (nth 1 info)))
    (if (or refs rmvs)
	(let* ((fld (mew-sinfo-get-folder))
	       (case (mew-sinfo-get-case))
	       (imapq (mew-folder-case-folder
		       case (mew-imap-queue-folder case)))
	       (imapq-dir (mew-expand-folder imapq))
	       file file-info buf ref)
	  (unless (file-directory-p imapq-dir) (mew-make-directory imapq-dir))
	  (setq file (mew-folder-new-message imapq))
	  (setq file-info (concat file mew-imapq-info-suffix))
	  (mew-lisp-save file-info (cons fld info) 'nobackup 'unlimit)
	  (setq buf (find-file-noselect file))
	  (save-excursion
	    (set-buffer buf)
	    (mew-header-insert mew-subj: (format "IMAP jobs for %s" fld))
	    (mew-header-insert mew-date: (mew-time-ctz-to-rfc (current-time)))
	    (mew-header-insert mew-from: "Mew IMAP manager")
	    (insert "\n")
	    (insert "Messages to be refiled:\n")
	    (while refs
	      (setq ref (car refs))
	      (setq refs (cdr refs))
	      (if (nth 2 ref)
		  (insert (format "\t%s will be copied to %s\n"
				  (nth 0 ref)
				  (mew-join "," (nthcdr 3 ref))))
		(insert (format "\t%s will be moved to %s\n"
				(nth 0 ref)
				(mew-join "," (nthcdr 3 ref))))))
	    (insert "\n")
	    (when rmvs
	      (insert "Messages to be deleted:\n")
	      (insert "\t" (mew-join "," rmvs) "\n"))
	    (set-buffer-modified-p nil)
	    (mew-frwlet
	     mew-cs-dummy nil
	     (write-region (point-min) (point-max) (buffer-file-name) nil 'no-msg)))
	  (mew-touch-folder imapq)
	  (mew-remove-buffer buf)
	  (if (mew-imap-trash-folder case)
	      (let* ((mew-inherit-exec-case case)
		     (mew-msg-rm-policy 'trashonly)
		     (mew-trash-folder (mew-imap-trash-folder case))
		     (mew-inherit-offline t))
		(mew-summary-exec-local beg end))
	    (let ((mew-inherit-exec-case case)
		  (mew-msg-rm-policy 'always)
		  (mew-inherit-offline t))
	      (mew-summary-exec-local beg end))))
      (message "No messages to be processed"))))

(defun mew-summary-exec-offline-one (&optional arg)
  (interactive "P")
  (if (not (mew-folder-imapp (mew-sinfo-get-folder)))
      (message "This command cannot be used in this folder")
    (mew-summary-goto-message)
    (mew-decode-syntax-delete)
    (save-window-excursion
      (let (beg end)
        (save-excursion
          (beginning-of-line)
          (setq beg (if arg (point-min) (point)))
          (end-of-line)
          (setq end (1+ (point))))
        (if (> end (point-max))
            (message "No message")
          (sit-for 0)
          (mew-summary-exec-offline-region beg end))))))

(provide 'mew-exec)

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

;;; mew-exec.el ends here
