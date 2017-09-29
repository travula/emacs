;;; mew-mark.el --- Marking for Mew Summary and Virtual mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  2, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; undo-func
;;;

(defun mew-mark-unrefile (fld msg)
  "Delete refile state and delete the mark."
  (when (get-buffer fld)
    (save-excursion
      (set-buffer fld)
      (mew-refile-reset msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo
;;;

(defsubst mew-mark-unmark ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward mew-regex-msg nil t)
	(mew-mark-delete-here))))

(defun mew-summary-undo (&optional count)
  "Cancel the mark in COUNT times."
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-undo-one count 'stayp))

(defun mew-summary-undo-one (&optional no-msg)
  "Cancel the mark on this message."
  (if (eobp)
      (or no-msg (message "No message"))
    (let (mark func fld msg)
      (save-excursion
	(mew-summary-goto-message)
	(setq mark (mew-summary-get-mark))
	(if (null mark)
	    (or no-msg (message "No mark"))
	  (setq func (mew-markdb-func-undo mark))
	  (or (fboundp func) (setq func nil))
	  (setq fld (mew-summary-folder-name))
	  (setq msg (mew-summary-message-number))
	  (cond
	   ((mew-virtual-p)
	    (mew-mark-unmark)
	    (mew-summary-unmark-in-physical fld msg func))
	   (t ;; Summary mode
	    (mew-mark-unmark)
	    (if func (funcall func fld msg)))))))))

;;

(defun mew-summary-undo-all ()
  "Cancel all marks according to what you input."
  (interactive)
  (let ((char (mew-input-mark)))
    (if (and char (not (char-equal char ?\r)))
	(mew-mark-undo-mark char))))

(defun mew-mark-undo-mark (mark &optional no-msg)
  "Undo MARK on the entire buffer.
If optional argument NO-MSG is non-nil, no message is displayed."
  (or no-msg (message "Unmarking..."))
  (mew-decode-syntax-delete)
  (let* ((regex (mew-mark-regex mark))
	 (func (mew-markdb-func-undo mark))
	 (case-fold-search nil)
	 alist fld msg)
    (or (fboundp func) (setq func nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(setq fld (mew-summary-folder-name))
	(setq msg (mew-summary-message-number))
	(mew-mark-delete-here t)
	(if (mew-virtual-p)
	    (mew-mark-alist-set alist fld msg)
	  (if func (funcall func fld msg))) ;; in physical only
	(forward-line))  ;; make search faster
      (set-buffer-modified-p nil)
      (mew-summary-unmark-in-physical-alist alist func)))
  (or no-msg (message "Unmarking...done")))

;;

(defun mew-summary-mark-undo-all ()
  "Unmark all message marked with 'o' or 'D' or 'X'."
  (interactive)
  (let ((marks mew-summary-mark-undo-marks))
    (message "Unmarking...")
    (while marks
      (mew-mark-undo-mark (car marks) 'no-msg)
      (setq marks (cdr marks)))
    (message "Unmarking...done")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic functions and macros for mark
;;;

(defun mew-summary-get-mark ()
  "Get a mark on the current message."
  (save-excursion
    (beginning-of-line)
    (if (looking-at mew-regex-msg-mark)
	(string-to-char (mew-match-string 2))
      nil)))

(defalias 'mew-summary-marked-p 'mew-summary-get-mark)

(defsubst mew-summary-mark-as (mark &optional force)
  "Mark this message with MARK if possible."
   (when (or force (not (mew-summary-marked-p)))
     (save-excursion
       (beginning-of-line)
       (when (re-search-forward mew-regex-msg nil t)
	 (mew-mark-put-here mark)))))

(defsubst mew-mark-put-here (mark &optional after-mark)
  (mew-elet
   (if after-mark (forward-char -1))
   (delete-char 1)
   (insert-and-inherit (char-to-string mark)) ;; inherit highlight
   (mew-highlight-mark-line mark)))

(defsubst mew-mark-delete-here (&optional after-mark)
  (mew-elet
   (if after-mark (forward-char -1))
   (delete-char 1)
   (insert-and-inherit " ") ;; inherit highlight
   (mew-highlight-unmark-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entire buffer
;;;

(defun mew-summary-mark-exist-p (mark-list)
  "See if this Summary mode has one or more marked messages."
  (let ((regex (mew-mark-list-regex mark-list)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regex nil t))))

(defun mew-summary-mark-collect (mark &optional begin end)
  "This function returns a list of message number."
  (save-excursion
    (let ((regex (mew-mark-regex mark))
	  (msglist nil)
	  (case-fold-search nil))
      (goto-char (if begin begin (point-min)))
      (while (re-search-forward regex end t)
	(setq msglist (cons (mew-summary-message-number) msglist)))
      (nreverse msglist))))

(defun mew-summary-mark-collect2 (mark)
  "For virtual mode, this function returns a list of
cons pairs of folder name and message number."
  (save-excursion
    (let ((regex (mew-mark-regex mark))
          (msglist nil)
	  (case-fold-search nil))
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (setq msglist (cons 
                       (cons
                        (mew-summary-folder-name)
                        (mew-summary-message-number))
                       msglist)))
      (nreverse msglist))))

(defun mew-summary-mark-collect4 ()
  (save-excursion
    (let (ret mrk msg)
      (goto-char (point-min))
      (while (re-search-forward mew-regex-msg-mark nil t)
	(setq msg (mew-match-string 1))
	(setq mrk (string-to-char (mew-match-string 2)))
	(setq ret (cons (cons msg mrk) ret))
	(forward-line))
      (nreverse ret))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base function
;;;

(defun mew-mark-afterstep (mark case)
  "Move the cursor after marking according to MARK's CASE.
See also mew-mark-afterstep-spec."
  (let ((action (mew-markas-nth mark case)))
    (cond
     ((eq action 0)
      ()) ;; stay
     ((eq action 1)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      (cond
       ((eq mew-summary-mark-direction 'up)
	(forward-line -1))
       ((eq mew-summary-mark-direction 'down)
	(forward-line))
       ((eq mew-summary-mark-direction 'next)
	(if (eq (mew-sinfo-get-direction) 'up)
	    (forward-line -1)
	  (forward-line)))))
     ((eq action 2)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      ;; for C-x C-x
      (beginning-of-line)
      (let ((zmacs-regions nil))
	(push-mark (point) t t))
      (mew-summary-display-after mew-summary-mark-direction)))))

(defun mew-mark-put-mark (newmark &optional no-msg valid-only)
  "Put the NEWMARK on the current line if possible.
If NO-MSG is non-nil, no message is displayed.
NO-MSG also means that this function is being called in loop."
  (mew-summary-msg-or-part
   (let (oldmark oldlevel oldname newlevel newname case zmacs-regions
	 msg fld marked invalidp)
     (save-excursion
       (mew-summary-goto-message)
       (setq invalidp (and valid-only (mew-summary-msg-invalidp)))
       (if invalidp
	   (unless no-msg (message "Cannot mark this invalid message with '%c'" newmark))
	 (when (mew-virtual-p)
	   (setq msg (mew-summary-message-number))
	   (setq fld (mew-summary-folder-name)))
	 (setq oldmark (mew-summary-get-mark))
	 (setq oldlevel (mew-markdb-level oldmark))
	 (setq oldname (mew-markdb-name oldmark))
	 (setq newlevel (mew-markdb-level newmark))
	 (setq newname (mew-markdb-name newmark))
	 (cond
	  ((null oldmark);; no mark
	   (setq case 1)
	   (mew-summary-mark-as newmark)
	   (setq marked t))
	  ((eq oldmark newmark)
	   (setq case 2)
	   (or no-msg
	       (mew-markdb-statefullp oldmark)
	       (message "Already marked as '%s'" oldname)))
	  ((< oldlevel newlevel)
	   (setq case 3)
	   (mew-summary-undo-one no-msg)
	   (mew-summary-mark-as newmark)
	   (setq marked t))
	  ((= oldlevel newlevel)
	   (cond
	    ((mew-markdb-statefullp oldmark)
	     (if (or no-msg
		     (y-or-n-p (format "Already marked as '%s'. %s it? "
				       oldname (mew-capitalize newname))))
		 (progn
		   (setq case 4)
		   (mew-summary-undo-one no-msg)
		   (mew-summary-mark-as newmark)
		   (setq marked t))
	       (setq case 5)))
	    (t
	     (setq case 6)
	     (mew-summary-undo-one no-msg)
	     (mew-summary-mark-as newmark)
	     (setq marked t))))
	  (t ;; > oldlevel newlevel
	   (setq case 7)
	   (message "Cannot mark here because '%s' is stronger than '%s'"
		    oldname newname)))))
     (unless invalidp
       (if (and msg fld marked)
	   (mew-summary-mark-in-physical fld msg newmark))
       (or no-msg (mew-mark-afterstep newmark case))
       (set-buffer-modified-p nil)))))

(defun mew-mark-put-mark-loop (func count stayp)
  "Unless COUNT is numeric, just call FUNC once. 
The cursor moves forward. STAYP has no effect.
If COUNT is positive, call FUNC in COUNT times moving the cursor forward.
If COUNT is negative, call FUNC in COUNT times moving the cursor backward.
If COUNT is numeric and STAYP is non-nil, the cursor stays in the
original position."
  (when (and func (fboundp func))
    (mew-summary-msg-or-part
     (if (numberp count)
	 (let ((start (point)))
	   (mew-decode-syntax-delete)
	   ;; positive loop
	   (while (and (> count 0) (not (eobp)))
	     (setq count (1- count))
	     (funcall func 'no-msg)
	     (forward-line))
	   ;; negative loop
	   (while (< count 0)
	     (if (bobp)
		 ;; need to call the func
		 (setq count 0)
	       (setq count (1+ count)))
	     (funcall func 'no-msg)
	     (forward-line -1))
	   (and stayp (goto-char start)))
       ;; just one
       (funcall func))
     (set-buffer-modified-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Physical
;;;

(defun mew-summary-mark-in-physical (fld msg new-mark)
  (save-excursion
    (set-buffer fld)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (mew-regex-jmp-msg msg) nil t)
	(mew-mark-put-here new-mark t)
	(set-buffer-modified-p nil)))))

(defun mew-summary-unmark-in-physical (fld msg &optional func)
  (save-excursion
    (set-buffer fld)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (mew-regex-jmp-msg msg) nil t)
	(mew-mark-delete-here t)
	(if func (funcall func fld msg))
	(set-buffer-modified-p nil)))))

(defmacro mew-mark-alist-set (alist fld msg)
  `(let ((imsg (string-to-int ,msg))
	 (fld-msgs (assoc ,fld ,alist)))
     (if fld-msgs
	 (nconc fld-msgs (list imsg))
       (setq ,alist (cons (list ,fld imsg) ,alist)))))

(defun mew-summary-mark-in-physical-alist (alist mark)
  (let (ent fld msg msgs)
    (while alist
      (setq ent (car alist))
      (setq alist (cdr alist))
      (setq fld (car ent))
      (setq msgs (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
      (when (get-buffer fld)
	(set-buffer fld)
	(save-excursion
	  (goto-char (point-min))
	  (while msgs
	    (setq msg (int-to-string (car msgs)))
	    (setq msgs (cdr msgs))
	    (when (re-search-forward (mew-regex-jmp-msg msg) nil t)
	      (mew-mark-put-here mark t)
	      (forward-line)))
	  (set-buffer-modified-p nil))))))

(defun mew-summary-unmark-in-physical-alist (alist func)
  (let (ent fld msg msgs)
    (while alist
      (setq ent (car alist))
      (setq alist (cdr alist))
      (setq fld (car ent))
      (setq msgs (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
      (when (get-buffer fld)
	(set-buffer fld)
	(save-excursion
	  (goto-char (point-min))
	  (while msgs
	    (setq msg (int-to-string (car msgs)))
	    (setq msgs (cdr msgs))
	    (when (re-search-forward (mew-regex-jmp-msg msg) nil t)
	      (mew-mark-delete-here t)
	      (if func (funcall func fld msg))
	      (forward-line)))
	  (set-buffer-modified-p nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Review: "*" in Summary mode
;;;

(defun mew-summary-review (&optional count)
  "\\<mew-summary-mode-map>
Put the review mark (default is '*') in COUNT times.
Use '\\[mew-summary-display-review-down]' or '\\[mew-summary-display-review-up]' to jump to a message marked with '*'.
See also '\\[mew-summary-mark-refile]', '\\[mew-summary-mark-delete]', '\\[mew-summary-mark-regexp]', and '\\[mew-summary-mark-all]'."
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-review-one count nil))

(defun mew-summary-review-one (&optional no-msg)
  "Put the review mark (default is '*') on this message."
  (mew-mark-put-mark mew-mark-review no-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multi: "@" in Summary mode
;;;

(defun mew-summary-multi (&optional count)
  "\\<mew-summary-mode-map>
Put the multi mark (default is '@') in COUNT times
for '\\[mew-summary-multi-forward]', '\\[mew-summary-unshar]', '\\[mew-summary-uudecode]', '\\[mew-summary-burst-multi]'. "
  (interactive "P")
  (mew-mark-put-mark-loop 'mew-summary-multi-one count nil))

(defun mew-summary-multi-one (&optional no-msg)
  "Put the multi mark (default is '@') on this message."
  (mew-mark-put-mark mew-mark-multi no-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete: "D" in Summary mode
;;;

(defun mew-summary-delete (&optional count)
  "Put the delete mark (default is 'D') in COUNT times."
  (interactive "P")
  (mew-summary-not-in-nntp
   (mew-mark-put-mark-loop 'mew-summary-delete-one count nil)))

(defun mew-summary-delete-one (&optional no-msg)
  "Put the delete mark (default is 'D') on this message."
  (mew-mark-put-mark mew-mark-delete no-msg 'valid-only))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unlink: "X" in Summary mode
;;;

(defun mew-summary-unlink (&optional count)
  "Put the unlink mark (default is 'X') in COUNT times."
  (interactive "P")
  (mew-summary-not-in-nntp
   (mew-mark-put-mark-loop 'mew-summary-unlink-one count nil)))

(defun mew-summary-unlink-one (&optional no-msg)
  "Put the unlink mark (default is 'X') on this message."
  (mew-mark-put-mark mew-mark-unlink no-msg 'valid-only))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; All messages
;;;

(defun mew-summary-mark-all (&optional arg)
  "Put the '*' mark onto all messages which are not marked."
  (interactive "P")
  (mew-decode-syntax-delete)
  (if arg
      (let ((begend (mew-summary-get-region)))
	(mew-summary-mark-all-region (car begend) (cdr begend)))
    (mew-summary-mark-all-region (point-min) (point-max))))

(defun mew-summary-mark-all-region (beg end)
  "Put the '*' mark onto all messages which are not marked between
BEG and END."
  (interactive "r")
  (let ((regex (mew-mark-regex ? )) ;; not marked
	(mark mew-mark-review) ;; someday ...
	fld msg alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
	(mew-summary-mark-as mark)
	(when (mew-virtual-p)
	  (setq fld (mew-summary-folder-name))
	  (setq msg (mew-summary-message-number))
	  (mew-mark-alist-set alist fld msg))
	(forward-line)) ;; make search faster
      (set-buffer-modified-p nil))
    (mew-summary-mark-in-physical-alist alist mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regex
;;;

(defun mew-summary-mark-regexp ()
  "Put the '*' mark onto all messages matched to a regular expression."
  (interactive)
  (mew-decode-syntax-delete)
  (let ((regex (read-string "Regexp: "))
	(mark mew-mark-review) ;; someday ...
        (n 0)
	fld msg alist)
    (while (string= regex "")
      (setq regex (read-string "Regexp: ")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
	(when (looking-at "[^\n]*\r") ;; before thread info
	  (mew-summary-mark-as mark)
          (setq n (1+ n))
	  (when (mew-virtual-p)
	    (setq fld (mew-summary-folder-name))
	    (setq msg (mew-summary-message-number))
	    (mew-mark-alist-set alist fld msg)))
	(forward-line)) ;; make search faster
      (set-buffer-modified-p nil))
    (mew-summary-mark-in-physical-alist alist mark)
    (cond
     ((= n 1)
      (message "1 message marked"))
     ((> n 1)
      (message "%d messages marked" n))
     (t
      (message "No message to be marked")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exchange
;;;

(defun mew-summary-exchange-mark (oldmark newmark &optional valid-only)
  (let ((regex (if valid-only 
		   (mew-mark-valid-regex oldmark)
		 (mew-mark-regex oldmark)))
	(case-fold-search nil)
	fld msg alist)
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward regex nil t))
	  (message "No marked messages")
	(beginning-of-line)
	(while (re-search-forward regex nil t)
	  (mew-mark-put-here newmark t)
	  (when (mew-virtual-p)
	    (setq fld (mew-summary-folder-name))
	    (setq msg (mew-summary-message-number))
	    (mew-mark-alist-set alist fld msg))
	  (forward-line)) ;; make search faster
	(set-buffer-modified-p nil)
	(mew-summary-mark-in-physical-alist alist newmark)))))
   
(defun mew-summary-mark-delete ()	;; * -> D
  "Put the delete mark onto all messages marked with '*'."
  (interactive)
  (mew-summary-not-in-nntp
   (mew-summary-exchange-mark mew-mark-review mew-mark-delete 'valid-only)))

(defun mew-summary-mark-unlink ()	;; * -> X
  "Put the delete mark onto all messages marked with '*'."
  (interactive)
  (mew-summary-exchange-mark mew-mark-review mew-mark-unlink 'valid-only))

(defun mew-summary-mark-multi ()	;; * -> @
  "Change the '*' mark into the '@' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-review mew-mark-multi))

(defun mew-summary-mark-review ()	;; @ -> *
  "Change the '@' mark into the '*' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-multi mew-mark-review))

(defun mew-summary-mark-swap ()		;; @ <-> *
  "Swap the '@' mark and the '*' mark."
  (interactive)
  (mew-summary-exchange-mark mew-mark-multi mew-mark-tmp)
  (mew-summary-exchange-mark mew-mark-review mew-mark-multi)
  (mew-summary-exchange-mark mew-mark-tmp mew-mark-review))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marking duplicated messages
;;;

(defun mew-summary-mark-duplicated (&optional arg)
  "Put the mark specified by 'mew-mark-duplicated' on duplicated
messages. If called with '\\[universal-argument]', process in the
region."
  (interactive "P")
  (mew-summary-not-in-queue
   (mew-summary-not-in-draft
    (if (and (or (eq mew-mark-duplicated mew-mark-delete)
		 (eq mew-mark-duplicated mew-mark-unlink))
	     (eq mew-summary-mark-duplicated-skip nil))
	(message "Cannot mark because messages may lost by this setting.")
      (let ((reversep (eq mew-summary-mark-duplicated-skip 'last))
	    (count 0) my-id dup-id size ids beg end region)
	(cond
	 (arg
	  (setq region (mew-summary-get-region))
	  (setq beg (car region))
	  (setq end (cdr region)))
	 (t
	  (setq beg (point-min))
	  (setq end (point-max))))
	(message "Marking duplications...")
	(save-excursion
	  ;; from mew-summary-thread-region in mew-threaed.el
	  (setq size (mew-count-lines beg end))
	  (cond
	   ((<= size 211)
	    (setq size 211))
	   ((<= size 1511)
	    (setq size 1511))
	   ((<= size 7211)
	    (setq size 7211))
	   (t
	    (setq size 18211)))
	  (setq ids (make-vector size 0)) ;; hash
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char (if reversep (point-max) (point-min)))
	    (catch 'loop
	      (while t
		(when (and (mew-summary-message-number)
			   (not (mew-summary-marked-p)))
		  (setq my-id (mew-summary-my-id))
		  (when (> (length my-id) 0)
		    (setq dup-id (intern-soft my-id ids))
		    (if (null dup-id)
			;; first time (no duplication)
			(set (intern my-id ids) t)
		      (when (symbol-value dup-id)
			;; second time (first duplication)
			(unless mew-summary-mark-duplicated-skip
			  (save-excursion
			    (when (search-backward
				   (format "\r <%s> " my-id) nil t)
			      (beginning-of-line)
			      (mew-mark-put-mark mew-mark-duplicated 'no-msg)
			      (setq count (1+ count))))
			  (set (intern my-id ids) nil)))
		      (mew-mark-put-mark mew-mark-duplicated 'no-msg)
		      (setq count (1+ count)))))
		(cond
		 (reversep
		  (beginning-of-line)
		  (if (bobp)
		      (throw 'loop t)
		    (forward-line -1)))
		 (t
		  (forward-line 1)
		  (if (eobp)
		      (throw 'loop t))))))))
	(cond
	 ((= count 0)
	  (message "Marking duplications...done  (no duplication)"))
	 ((= count 1)
	  (message "Marking duplications...done  (1 msg is marked)" count))
	 (t
	  (message "Marking duplications...done  (%d msgs are marked)" count))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reviewing
;;;

(defun mew-summary-set-walk-mark ()
  (let ((char (mew-input-mark)))
    (cond
     ((char-equal char ?\r)
      (setq mew-mark-walk mew-mark-default-walk)
      (message "Target mark was set to '%s'" (char-to-string mew-mark-walk)))
     (char (setq mew-mark-walk char)))))

(defun mew-summary-down-mark (mark)
  (let ((case-fold-search nil))
    (forward-line)
    (cond 
     ((re-search-forward (mew-mark-regex mark) nil t)
      (beginning-of-line)
      t)
     (t 
      (forward-line -1)
      (message "No more marked messages")
      nil))))

(defun mew-summary-display-review-down (&optional arg)
  "Jump to the message marked with '*' below.
If called with '\\[universal-argument]', you can change the target mark.
After that, this command jumps to the message marked with 
the specified mark."
  (interactive "P")
  (if arg
      (mew-summary-set-walk-mark)
    (if (mew-summary-down-mark mew-mark-walk)
	(mew-summary-display nil))))

(defun mew-summary-up-mark (mark)
  (let ((case-fold-search nil))
    (cond
     ((re-search-backward (mew-mark-regex mark) nil t)
      t)
     (t 
      (message "No more marked messages")
      nil))))

(defun mew-summary-display-review-up (&optional arg)
  "Jump to the message marked with '*' above.
If called with '\\[universal-argument]', you can change the target mark.
After that, this command jumps to the message marked with 
the specified mark."
  (interactive "P")
  (if arg
      (mew-summary-set-walk-mark)
    (if (mew-summary-up-mark mew-mark-walk)
	(mew-summary-display nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clean up marks!
;;;

(defun mew-mark-clean (askp &optional sum)
  "Process marked messages for this folder."
  (if sum
      (set-buffer sum)
    (setq sum (mew-summary-folder-name 'ext)))
  (if (and (mew-summary-p)
           (mew-summary-mark-exist-p mew-mark-clean)
	   (or (not askp)
	       (y-or-n-p (format "Marks exist in %s. Process them? " sum))))
      (mew-summary-exec)))

(defun mew-mark-init ()
  (add-hook 'kill-emacs-hook 'mew-mark-clean-up))

(defun mew-mark-clean-up ()
  "Process marked messages for all Summary modes.
Typically called by kill-emacs-hook."
  (remove-hook 'kill-emacs-hook 'mew-mark-clean-up)
  (mew-decode-syntax-delete)
  (let ((bufs mew-buffers) buf)
    (save-excursion
      (while bufs
	(setq buf (car bufs))
	(setq bufs (cdr bufs))
	(when (and (bufferp (get-buffer buf))
		   (mew-folder-localp buf)) ;; xxx
	  (mew-mark-clean mew-ask-mark-process buf))))))

;; Saving marks is a really bad idea.
;; First because there is no way to fill the gap if the folder is newer
;; than the cache at quitting.
;; Even if the cache is newer, saving marks faces dilemma if 
;; multiple Emacses run.

(provide 'mew-mark)

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

;;; mew-mark.el ends here
