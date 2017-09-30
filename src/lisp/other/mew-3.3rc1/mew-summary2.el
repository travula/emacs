;;; mew-summary2.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of
;;;

(defun mew-message-set-end-of ()
  (save-restriction
    (widen)
    (save-excursion
      (mew-elet
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (if (and mew-xemacs-p (extent-at (point) nil nil nil 'at))
	   ;; to skip graphics
	   (insert "\n"))
       (mew-message-clear-end-of)
       (when (or mew-end-of-message-string mew-end-of-part-string)
	 (move-overlay (mew-minfo-get-overlay) (point-max) (point-max))
	 (if (mew-decode-syntax-p)
	     (if (mew-summary-end-of-message-p)
		 (mew-message-set-end-of-message)
	       (mew-message-set-end-of-part))
	   (mew-message-set-end-of-message)))))))

(defsubst mew-message-clear-end-of ()
  (unless (overlayp (mew-minfo-get-overlay))
    (mew-minfo-set-overlay (mew-overlay-make (point-max) (point-max))))
  (mew-message-set-end-of-nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Prefetch
;;;

(defun mew-summary-cache-prefetch ()
  (let ((mew-inherit-prefetching t)
	fld next)
    (save-excursion
      (mew-redraw) ;; need to display
      (mew-summary-goto-message)
      (cond
       ((eq (mew-sinfo-get-direction) 'up)
	(when (re-search-backward mew-regex-msg-or-part nil t)
	  (setq fld (mew-summary-folder-name))
	  (setq next (mew-summary-message-number))))
       ((eq (mew-sinfo-get-direction) 'down)
	(if (mew-decode-syntax-end)
	    (goto-char (mew-decode-syntax-end))
	  (forward-line))
	(when (re-search-forward mew-regex-msg-or-part nil t)
	  (setq fld (mew-summary-folder-name))
	  (setq next (mew-summary-message-number))))))
    ;; should get the cursor back for display
    (save-excursion
      (if (and fld next
	       (not (mew-cache-hit fld next))
	       (not (string= fld mew-draft-folder))
	       (not (mew-summary-message-toobig fld next)))
	  (mew-cache-message fld next nil 'no-err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Caching
;;;

(defun mew-summary-cache-message (fld msg sumbuf &optional unlimit nodisplay)
  "Create a cache for the message specified with FLD and MSG.
If UNLIMIT is non-nil, decodes it without limitations.
If nodisplay is non-nil, displays the cached message."
  ;; message buffer
  (mew-elet
   (mew-summary-display-preamble)
   (let ((cache (mew-cache-message fld msg unlimit)))
     (mew-decode-syntax-copy cache)
     (unless nodisplay
       (mew-mime-message/rfc822 cache mew-decode-syntax))
     ;; Must print the syntax before mew-summary-display-postscript
     ;; to tell the end of message.
     (mew-decode-syntax-print sumbuf
			      mew-decode-syntax
			      (mew-xinfo-get-multi-form)
			      (mew-xinfo-get-icon-spec))
     (unless nodisplay
       (mew-summary-display-postscript))
     cache)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; After step
;;;

(defsubst mew-summary-display-after (direction)
  (cond 
   ((eq direction 'down)
    (mew-summary-display-down))
   ((eq direction 'up)
    (mew-summary-display-up))
   ((eq direction 'next)
    (mew-summary-display-next))
   (t ()))) ;; 'stop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Preamble and postscript
;;;

(defun mew-summary-recenter ()
  "Make the current line to the center of Summary mode."
  (interactive)
  (if (or mew-summary-recenter-p (interactive-p))
      (recenter (/ (- (window-height) 2) 2))))

(defsubst mew-summary-display-preamble ()
  ;; message buffer
  (mew-erase-buffer)
  (mew-message-clear-end-of)
  (mew-overlay-delete-buffer) ;; xxx also delete extents?
  ;; kill mark for cite
  (and (mark-marker) (set-marker (mark-marker) nil)))

(defvar mew-message-last-buffer nil)

(defsubst mew-summary-display-postscript (&optional no-hook)
  ;; message buffer
  (unless no-hook (run-hooks 'mew-message-hook))
  (setq mew-message-last-buffer (current-buffer))
  (mew-message-set-end-of)
  (set-buffer-modified-p nil))

(defun mew-summary-cursor-postscript ()
  (mew-summary-mode-line)
  (mew-summary-recenter)
  (mew-highlight-cursor-line)
  (set-buffer-modified-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying
;;;

(defsubst mew-summary-message-toobig (fld msg)
  (let ((file (mew-expand-folder fld msg)))
    (and (mew-folder-localp fld)
	 (file-readable-p file)
	 (> (mew-file-get-size file) mew-file-max-size))))

(defun mew-summary-display (&optional redisplay)
  " (1) If called interactively, this command lets you read through
messages.  That is, display a message, scroll it, and
move-then-display another message or part.

See 'mew-summary-show-direction' to set the direction that the cursor
moves. You can select a value out of 'up, 'down, 'next(current
direction) or 'stop.  Default is 'down. 'mew-summary-show-direction'
is valid in this case only because the cursor stays in the other
cases.

 (2) If called interactively with '\\[universal-argument]' (i.e.
REDISPLAY is 't'), this command displays the current message or part
again. This is a convenient way to get back to the beginning of the
current message or part.

 (3) If called internally, this function displays the current message
or part. If it is already displayed, nothing changes.

 (4) If called internally and REDISPLAY is 't', this function displays
the current message or part. Even if it is already displayed, this
function displays it again getting gack to the beginning."
  (interactive "P")
  (when (or redisplay (mew-sinfo-get-disp-msg) (interactive-p))
    (mew-summary-msg-or-part
     (let* ((fld (mew-summary-folder-name))
	    (vfld (mew-summary-folder-name 'ext))
	    (msg (mew-summary-message-number))
	    (part (mew-syntax-nums))
	    (ofld (mew-current-get-fld (mew-frame-id)))
	    (omsg (mew-current-get-msg (mew-frame-id)))
	    (opart (mew-current-get-part (mew-frame-id)))
	    (cache (mew-cache-hit fld (or msg omsg)))
	    (win (selected-window))
	    (read-through (interactive-p))
	    (sumbuf (current-buffer))
	    next prefetch)
       (unwind-protect
	   (progn
	     (mew-summary-toggle-disp-msg 'on)
	     (mew-window-configure 'message)
	     ;; messge buffer
	     (mew-current-set fld (or msg omsg) part)
	     (mew-minfo-set-summary vfld)
	     (cond
	      ((null cache)
	       (mew-decode-syntax-delete)
	       (cond
		((string= fld mew-draft-folder)
		 (if (and (string= fld ofld) (string= msg omsg)
			  (not redisplay))
		     (if read-through
			 (if (mew-message-next-page)
			     (setq next t)))
		   (mew-decode-syntax-clear)
		   (mew-summary-display-draft fld msg)
		   (setq prefetch t)))
		((mew-summary-message-toobig fld msg)
		 (if (and (string= fld ofld) (string= msg omsg)
			  (not redisplay))
		     (if read-through
			 (if (mew-message-next-page)
			     (setq next t)))
		   (mew-decode-syntax-clear)
		   (mew-summary-display-raw fld msg mew-file-max-size)
		   (setq prefetch t)
		   (message
		    (substitute-command-keys
		     "Too large, truncated. To see the entire message, type '\\<mew-summary-mode-map>\\[mew-summary-analyze-again]'"))))
		(t
		 (mew-decode-syntax-clear)
		 (mew-summary-cache-message fld msg sumbuf)
		 (setq prefetch t))))
	      (msg
	       (cond
		((or (null ofld)
		     (not (and (string= fld ofld) (string= msg omsg)))
		     opart redisplay)
		 (mew-decode-syntax-clear)
		 (mew-decode-syntax-delete)
		 (mew-summary-display-message cache sumbuf)
		 (setq prefetch t))
		(read-through
		 (if (mew-message-next-page)
		     (setq next t)))))
	      (part
	       (cond
		((or (null opart)
		     (not (equal opart part))
		     redisplay)
		 (mew-summary-display-part cache part))
		;; If called internally, never match below
		(read-through
		 (if (mew-message-next-page)
		     (setq next t)))))))
	 (if (mew-xinfo-get-decode-err)
	     (message "MIME decoding error: %s" (mew-xinfo-get-decode-err)))
	 (if (mew-xinfo-get-action)
	     (message (mew-xinfo-get-action)))
	 (select-window win)
	 ;; summary buffer
	 (mew-summary-cursor-postscript)
	 (if prefetch (mew-summary-cache-prefetch))
	 (if next (mew-summary-display-after mew-summary-show-direction)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying message or part
;;;

(defun mew-summary-display-message (cache sumbuf)
  "Display a message from the CACHE."
  ;; message buffer
  (mew-elet
   (mew-summary-display-preamble)
   (mew-decode-syntax-copy cache)
   (mew-mime-message/rfc822 cache mew-decode-syntax)
   ;; Must print the syntax before mew-summary-display-postscript
   ;; to tell the end of message.
   (mew-decode-syntax-print sumbuf
			    mew-decode-syntax
			    (mew-xinfo-get-multi-form)
			    (mew-xinfo-get-icon-spec))
   (mew-summary-display-postscript)))

(defun mew-summary-display-part (cache nums)
  "Display a part from the CACHE."
  ;; message buffer
  (mew-elet
   (mew-summary-display-preamble)
   (mew-decode-syntax-copy cache)
   (mew-mime-part cache mew-decode-syntax nums)
   (mew-summary-display-postscript)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying draft
;;;

(defun mew-summary-display-draft (fld msg)
  "Display the message in +draft specified by FLD and MSG.
The message is not in the MIME form. So, it cannot be decoded as MIME.
Just display it as is. This function does not create a cache."
  ;; message buffer
  (mew-elet
   (mew-summary-display-preamble)
   (mew-insert-message fld msg mew-cs-m17n nil)
   (mew-header-goto-end)
   (mew-highlight-header-region (point-min) (point))
   (mew-summary-display-postscript)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Displaying draft
;;;

(defun mew-summary-display-raw (fld msg size)
  "Display the message specified by FLD and MSG as is.
If SIZE is specified, truncates it with the size.
This function does not create a cache."
  ;; message buffer
  (mew-elet
   (mew-summary-display-preamble)
   (mew-insert-message fld msg mew-cs-binary size)
   (goto-char (point-min))
   (when (re-search-forward mew-eoh nil t)
     (let ((beg (point)))
       (goto-char (point-max))
       (mew-cs-decode-region beg (point) mew-cs-autoconv)))
   (goto-char (point-min))
   (condition-case nil
       (progn
	 (mew-decode-rfc822-header) ;; xxx limit..
	 (mew-decode-syntax-arrange-warning)
	 (mew-header-goto-end)
	 (mew-header-arrange (point-min) (point))
	 (setq mew-decode-syntax (mew-decode-syntax-rfc822)))
     (error ()))
   (mew-summary-display-postscript 'no-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Analyze again
;;;

(defun mew-find-longest-match (nums)
  (save-excursion
    (forward-line)
    (let ((i 1) (max 0) (lim (length nums)) j part lines)
      (while (and (mew-decode-syntax-end) (< (point) (mew-decode-syntax-end)))
	(setq part (mew-syntax-nums))
	(setq j 0)
	(catch 'loop
	  (while (< j lim)
	    (unless (eq (nth j nums) (nth j part))
	      (throw 'loop nil))
	    (setq j (1+ j))))
	(when (> j max)
	  (setq max j)
	  (setq lines i))
	(setq i (1+ i))
	(forward-line))
      lines)))

(defun mew-summary-analyze-again-alternative (&optional arg)
  "This command analyzes the message again with 
'mew-use-alternative' and 'mew-use-text-body' reversed."
  (interactive "P")
  (let ((mew-use-alternative (not mew-use-alternative))
	(mew-use-text-body   (not mew-use-text-body)))
    (mew-summary-analyze-again arg)))

(defun mew-summary-analyze-again (&optional arg)
  "This command removes the cache of this message or part and
analyzes the message, then displays this message or part again.

If the size of the current message exceeds 'mew-file-max-size', MIME
analysis is skipped then the beginning of the raw message is
displayed. In this situation, this command analyzes the current
message without the limitation then displays it.

If the length of a header exceeds 'mew-header-max-length', a broken
message is displayed. In this situation, this command analyzes the
current message without the limitation then displays it.

If called with '\\[universal-argument]', analyze the message
with 'mew-decode-broken' reversed."
  (interactive "P")
  (mew-summary-msg-or-part
   (if (mew-folder-draftp (mew-summary-folder-name))
       (mew-summary-display 'redisplay)
     (let ((mew-decode-broken
	    (if arg (not mew-decode-broken) mew-decode-broken))
	   (fld (mew-summary-folder-name))
	   (vfld (mew-summary-folder-name 'ext))
	   (msg (mew-summary-message-number2))
	   (part (mew-syntax-nums))
	   (win (selected-window))
	   (sumbuf (current-buffer))
	   cache lines)
       (mew-cache-delete2 fld msg)
       (mew-summary-goto-message)
       (unwind-protect
	   (progn
	     (mew-summary-toggle-disp-msg 'on)
	     (mew-window-configure 'message)
	     ;; message buffer
	     (mew-current-set fld msg part)
	     (mew-minfo-set-summary vfld)
	     (mew-decode-syntax-clear)
	     (mew-decode-syntax-delete)
	     (if (null part)
		 (mew-summary-cache-message fld msg sumbuf 'unlimit)
	       (setq cache (mew-summary-cache-message
			    fld msg sumbuf 'unlimit 'nodisplay))
	       ;; Decode syntax is displayed.
	       ;; Let's move the cursor for end-of-*.
	       ;; Since its window is not selected, the displayed cursor
	       ;; does not move. Hence call (forward-line lines) twice.
	       (save-excursion
		 (set-buffer sumbuf)
		 (setq lines (mew-find-longest-match part))
		 (if lines (forward-line lines))
		 (setq part (mew-syntax-nums)))
	       (mew-summary-display-part cache part)))
	 (if (mew-xinfo-get-decode-err)
	     (message "MIME decoding error: %s" (mew-xinfo-get-decode-err)))
	 (select-window win)
	 ;; summary buffer
	 (if lines (forward-line lines))
	 (mew-summary-cursor-postscript))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display ASIS
;;;

(defun mew-summary-display-asis ()
  "Insert this message or part into Message mode in the raw format."
  (interactive)
  (mew-summary-msg-or-part
   (let* ((fld (mew-summary-folder-name))
	  (vfld (mew-summary-folder-name 'ext))
	  (msg (mew-summary-message-number2))
	  (nums (mew-syntax-nums))
	  (win (selected-window))
	  (buf (generate-new-buffer mew-buffer-prefix))
	  (cbuf (mew-cache-hit fld msg))
	  (alt (if cbuf
		   (mew-cache-dinfo-get-use-alt cbuf)
		 mew-use-alternative))
	  syntax ct begin end)
     (save-excursion
       (set-buffer buf)
       (mew-erase-buffer)
       (mew-insert-message fld msg mew-cs-text-for-read nil)
       (mew-dinfo-set nil t nil alt)
       (mew-decode-for-edit)
       (setq syntax (mew-syntax-get-entry mew-decode-syntax nums)))
     (setq ct (mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap))
     (setq begin (mew-syntax-get-begin syntax))
     (setq end (mew-syntax-get-end syntax))
     (unwind-protect
	 (progn
	   (mew-summary-toggle-disp-msg 'on)
	   (mew-window-configure 'message)
	   ;; message buffer
	   (mew-current-set fld msg nums)
	   (mew-minfo-set-summary vfld)
	   (mew-elet
	    (mew-summary-display-preamble)
	    (mew-decode-syntax-copy buf)
	    (cond
	     ((string= ct mew-ct-msg)
	      (if nums
		  (setq end (mew-syntax-get-end (mew-syntax-get-part syntax)))
		(setq end nil))
	      ;; We need to keep properties of a header.
	      ;; This must be "insert-buffer-substring".
	      (insert-buffer-substring buf begin end)
	      (goto-char (point-min))
	      (mew-header-goto-end)
	      (mew-header-arrange (point-min) (point))
	      (setq mew-decode-syntax (mew-decode-syntax-rfc822)))
	     (t
	      ;; We need to keep composite properties of charset.
	      ;; This must be "insert-buffer-substring".
	      (insert-buffer-substring buf begin end)
	      (goto-char (point-min)))))
	   (mew-summary-display-postscript 'no-hook))
       (mew-remove-buffer buf)
       (select-window win)))))

(defun mew-summary-find-file (&optional arg)
  "Open this message and makes it read-only.
If called with '\\[universal-argument]', it stays writable."
  (interactive "P")
  (mew-summary-msg-or-part
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number2))
	  (file (mew-expand-folder fld msg)))
     (when (mew-sinfo-get-disp-msg)
       (mew-summary-toggle-disp-msg))
     (mew-frwlet
      (if (mew-folder-draftp fld) mew-cs-m17n mew-cs-autoconv) mew-cs-dummy
      (if arg
	  (find-file file)
	(view-file file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Direction
;;;

(defun mew-summary-next ()
  (if (eq (mew-sinfo-get-direction) 'up)
      (mew-summary-up)
    (mew-summary-down)))

(defvar mew-summary-down-function 'forward-line
  "This function is called by mew-summary-down(). This is a very
ad-hoc solution for putting the review mark on a thread.")

(defun mew-summary-down ()
  (funcall mew-summary-down-function)
  (cond 
   ((re-search-forward mew-regex-msg-or-part nil t)
    (mew-summary-thread-move-cursor)
    (mew-sinfo-set-direction 'down)
    t)
   (t
    (mew-decode-syntax-delete)
    (forward-line -1)
    (unless (pos-visible-in-window-p)
      ;; This ensures that some messages are displayed in Summary.
      (recenter))
    (mew-summary-thread-move-cursor)
    (mew-window-configure 'summary)
    (mew-current-set nil nil nil)
    (message "No more messages")
    nil)))

(defun mew-summary-up ()
  (beginning-of-line)
  (cond 
   ((re-search-backward mew-regex-msg-or-part nil t)
    (mew-summary-thread-move-cursor)
    (mew-sinfo-set-direction 'up)
    t)
   (t 
    (mew-summary-thread-move-cursor)
    (mew-decode-syntax-delete)
    (mew-window-configure 'summary)
    (mew-current-set nil nil nil)
    (message "No more messages")
    nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Direction 2
;;;

(defun mew-summary-display-next ()
  (if (mew-summary-next) (mew-summary-display nil)))

(defun mew-summary-display-up (&optional arg)
  "Move to above then display. Targets includes parts, messages 
marked with '*', and non-marked messages. When called with '\\[universal-argument]',
parts are skipped."
  (interactive "P")
  (beginning-of-line)
  (when arg
    (mew-summary-goto-message)
    (mew-decode-syntax-delete))
  (if (mew-summary-up) (mew-summary-display nil)))

(defun mew-summary-display-down (&optional arg)
  "Move to below then display. Targets includes parts, messages 
marked with '*', and non-marked messages. When called with '\\[universal-argument]',
parts are skipped."
  (interactive "P")
  (when arg
    (mew-summary-goto-message)
    (mew-decode-syntax-delete))
  (if (mew-summary-down) (mew-summary-display nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Paging
;;;

(defun mew-summary-prev-page ()
  "\\<mew-summary-mode-map>
Back-scroll this message. Unnecessary header fields are hidden
over the window. Type '\\[mew-summary-prev-page]' to see them when a message is displayed."
  (interactive)
  (mew-summary-scroll-down 'fullpage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scrolling
;;;

(defun mew-summary-scroll-up ()
  "Make this message scroll up with one line."
  (interactive)
  (mew-summary-msg-or-part
   (let ((win (selected-window))
	 (msg (mew-summary-message-number))
	 (part (mew-syntax-nums))
	 (omsg (mew-current-get-msg (mew-frame-id)))
	 (opart (mew-current-get-part (mew-frame-id))))
     ;; xxx how about folder check?
     (if (or (and msg (string= msg omsg) (null part) (null opart))
	     (and part (equal part opart))) ;; MUST be equal
	 (unwind-protect
	     (progn
	       (mew-window-configure 'message)
	       ;; message buffer
	       (mew-message-next-page 1))
	   (select-window win))
       (call-interactively 'mew-summary-display)))))

(defun mew-summary-scroll-down (&optional fullpage)
  "Make this message scroll down with one line."
  (interactive)
  (mew-summary-msg-or-part
   (let ((win (selected-window))
	 (msg (mew-summary-message-number))
	 (part (mew-syntax-nums))
	 (omsg (mew-current-get-msg (mew-frame-id)))
	 (opart (mew-current-get-part (mew-frame-id))))
     ;; xxx how about folder check?
     (if (or (and msg (string= msg omsg) (null part) (null opart))
	     (and part (equal part opart))) ;; MUST be equal
	 (unwind-protect
	     (progn
	       (mew-window-configure 'message)
	       ;; message buffer
	       (mew-message-prev-page (if fullpage nil 1)))
	   (select-window win))
       (call-interactively 'mew-summary-display)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mouse
;;;

(defun mew-summary-mouse-show (e)
  "Mouse version of 'mew-summary-display'."
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (call-interactively 'mew-summary-display))

(provide 'mew-summary2)

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

;;; mew-summary2.el ends here
