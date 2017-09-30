;;; mew-thread.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;

(defvar mew-use-sorted-thread t)

(defcustom mew-use-complete-thread t
  "If non-nil, threads are made using two passes.

First pass -  Repeat the following procedure in numerical order:
	(1.0) Pick one message from the message list.
	(1.1) Register the current message-id: to DB.
	(1.2) Find its parent message-id: in DB.
	(1.3) If found, register the current message as a child of
	      the parent.
	(1.4) Otherwise, register the current message to the top
	      node list.

Here we have pretty good threads.  However, if the messages are not
sorted by Date:, it is possible that some top nodes can be
connected to other threads.  If 'mew-use-complete-thread' is non-nil,
the second pass is carried out.

Second pass - Repeat the following procedure for top nodes linearly:
	(2.0) Pick one message from the top node list.
	(2.1) Find its parent message-id: in DB.
	(2.2) If found, register the current message as a child of
	      the parent.
	(2.3) Otherwise, register the current message to the new top
	      node list.

If you have bogus messages and the second pass is carried out, thread
structure MAY loop. This results in an infinite loop of visualizing 
threads (not making threads). 

Mew does not provide any loop detection/avoidance mechanism. So, you
should understand this risk."
  :group 'mew-summary
  :type 'boolean)

(defcustom mew-thread-indent-string "  "
  "*A string to be inserted for indentation of thread."
  :group 'mew-summary
  :type 'string)

(defvar mew-thread-indent-array nil)
(defvar mew-thread-indent-array-size 33) ;; divide by 32

(defcustom mew-use-fancy-thread nil
  "*If non-nil, use fancy style thread visualization.
See also mew-fancy-thread-indent-strings."
  :group 'mew-summary
  :type 'boolean)


(defcustom mew-fancy-thread-indent-strings [" +" " +" " |" "  "]
  "*Vector of strings to be used for indentation of fancy style thread.
Valid only if mew-use-fancy-thread is non-nil.

This consists of four members; 1st member for prefixing to a child
message that is not the last one, 2nd member is for prefixing to the 
last child, 3rd and 4th members are for prefixing to grand-child thread trees, 
4th member is for the child tree of the last child message.
For example, [ \" +\" \" +\" \" |\" \"  \" ] makes thread view below.

    Message 1
     +Message 2
     | +Message 3
     +Message 4
       +Message 5

All members must have the same length."
  :group 'mew-summary
  :type 'sexp)

(defcustom mew-use-thread-cursor nil
  "*If non-nil, move cursor after the indentation of thread."
  :group 'mew-summary
  :type 'boolean)


(defvar mew-thread-indent-length nil)
(defvar mew-thread-indent-list nil)

(defvar mew-use-thread-separator nil
  "*If non-nil, the specified string is inserted between threads.")
(defvar mew-thread-separator "--")

(defsubst mew-thread-insert-separator ()
  (if (and mew-use-thread-separator
	   (/= (save-excursion (beginning-of-line) (point)) 1))
      (insert mew-thread-separator "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread info macro
;;;

(defun mew-thread-make-entry ()
  (make-vector 5 nil))

(defsubst mew-thread-get-myid (entry)
  (aref entry 0))

(defsubst mew-thread-get-prntid (entry)
  (aref entry 1))

(defsubst mew-thread-get-child (entry)
  (aref entry 2))

(defsubst mew-thread-get-msg (entry)
  (aref entry 3))

(defsubst mew-thread-get-line (entry)
  (aref entry 4))

(defsubst mew-thread-set-myid (entry myid)
  (aset entry 0 myid))

(defsubst mew-thread-set-prntid (entry prntid)
  (aset entry 1 prntid))

(defsubst mew-thread-set-child (entry child)
  (aset entry 2 child))

(defsubst mew-thread-set-msg (entry msg)
  (aset entry 3 msg))

(defsubst mew-thread-set-line (entry line)
  (aset entry 4 line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;

(defun mew-thread-setup ()
  (cond
   (mew-use-fancy-thread
    (let ((idt1 (aref mew-fancy-thread-indent-strings 0))
	  (idt2 (aref mew-fancy-thread-indent-strings 1))
	  (idt3 (aref mew-fancy-thread-indent-strings 2))
	  (idt4 (aref mew-fancy-thread-indent-strings 3)))
      (unless (and (= (length idt1) (length idt2))
		   (= (length idt2) (length idt3))
		   (= (length idt3) (length idt4)))
	(error
	 "All members of mew-fancy-thread-indent-strings must have the same length."))
      (setq mew-thread-indent-length (length idt1))))
   (t
    (let* ((i 0)
	   (size mew-thread-indent-array-size)
	   (array (make-vector size nil))
	   (indent ""))
      (while (< i size)
	(aset array i indent)
	(setq i (1+ i))
	(setq indent (concat indent mew-thread-indent-string)))
      (setq mew-thread-indent-array array))
    (setq mew-thread-indent-length (length mew-thread-indent-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Making thread
;;;

(defun mew-summary-mark-make-thread ()
  "Make threads for messages marked with '*'."
  (interactive)
  (mew-summary-thread-region (point-min) (point-max) 'mark))

(defsubst mew-thread-cache-valid-p (vfolder)
  (let ((ofld (mew-thread-to-folder vfolder)))
    (when (get-buffer ofld)
      (save-excursion
	(equal (progn (set-buffer ofld) (mew-sinfo-get-cache-time))
	       (progn (set-buffer vfolder) (mew-sinfo-get-cache-time)))))))

(defun mew-summary-make-thread (&optional arg)
  "If called in Summary mode, this command makes threads for the
Summary mode as Virtual mode, then the cursor jump onto the current
message in the Virtual mode. If a corresponding Virtual mode exists,
this command just visits the Virtual mode.

If called with '\\[universal-argument]' in Summary mode, make
threads for messages in the region.

If called in Virtual mode, switch back to the corresponding Summary
mode and move to the current message."
  (interactive "P")
  (when (mew-syntax-number)
    (mew-summary-goto-message))
  (mew-decode-syntax-delete)
  (let* ((msg (mew-summary-message-number))
	 (disp (mew-sinfo-get-disp-msg))
	 (folder (mew-summary-folder-name 'ext)) ;; xxx
	 fld vfolder)
    (cond
     ((mew-virtual-p)
      (if (not (mew-thread-p))
	  (message "No physical folder")
	(setq fld (mew-thread-to-folder folder))
	(if (not (and fld (get-buffer fld)))
	    (message "No physical folder")
	  (mew-summary-visit-folder fld nil 'no-ls)
	  (mew-summary-toggle-disp-msg (if disp 'on 'off))
	  (if (not msg)
	      (goto-char (point-max))
	    (mew-summary-jump-message msg)
	    (mew-summary-display nil)))))
     ((and (setq vfolder (mew-folder-to-thread folder))
	   (get-buffer vfolder)
	   (mew-virtual-thread-p vfolder)
	   (mew-thread-cache-valid-p vfolder))
      (mew-summary-visit-folder vfolder)
      (mew-summary-toggle-disp-msg (if disp 'on 'off))
      (when msg
	(mew-summary-jump-message msg)
	(mew-summary-thread-move-cursor)
	(mew-summary-display nil)))
     (arg
      (let ((begend (mew-summary-get-region)))
	(mew-summary-thread-region (car begend) (cdr begend))))
     (t
      (mew-summary-thread-region (point-min) (point-max) nil msg)))))

(defun mew-summary-thread-region (beg end &optional mark disp-msg)
  "Make threads for messages in a region.  If you want to know how
threads are created, see 'mew-use-complete-thread'."
  (interactive "r")
  (mew-summary-only
   (when (mew-summary-exclusive-p)
     (let* ((folder (mew-summary-folder-name 'ext))
	    (vfolder (mew-folder-to-thread folder))
	    (disp (mew-sinfo-get-disp-msg))
	    (ctime (mew-sinfo-get-cache-time))
	    (case (mew-sinfo-get-case))
	    (column (mew-summary-scan-form folder 'column))
	    (numlen (mew-summary-scan-form-num folder))
 	    overnum spacer indentlst
	    db size top start me prnt prnt-cld my-id prnt-id func msg line
	    tm1 tm2 tm3 tm4 tm5 tm6)
       (message "Making thread (first pass)...")
       (save-restriction
	 (narrow-to-region beg end)
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
	 (setq db (make-vector size 0)) ;; hash
	 (save-excursion
	   ;; Calculate message number width
 	   (goto-char (point-max))
 	   (mew-summary-goto-message)
 	   (when (looking-at mew-regex-msg)
 	     (setq overnum (length (mew-match-string 1)))
 	     (if (<= overnum numlen)
		 (setq overnum nil)
	       (setq column (+ column (- overnum numlen)))
	       (setq spacer (make-vector overnum nil))
	       (setq size 0)
	       (while (< size overnum)
		 (aset spacer size (make-string size ? ))
		 (setq size (1+ size)))))
	   (goto-char (point-min))
	   (if mark
	       (setq func (lambda ()
			    (re-search-forward mew-regex-msg-review nil t)))
	     (setq func (lambda () (not (eobp)))))
	   (setq tm1 (current-time))
	   (while (funcall func)
	     (beginning-of-line)
	     (setq start (point))
	     (if (not (looking-at mew-regex-summary))
		 (forward-line)
	       (setq my-id   (mew-match-string 1))
	       (setq prnt-id (mew-match-string 2))
	       (setq msg     (mew-summary-message-number))
	       (forward-line)
	       ;; Throw away properties here and give properties later.
	       ;; This is faster than inheriting properties.
	       (setq line (mew-buffer-substring start (point)))
	       (setq me (mew-thread-make-entry))
	       (mew-thread-set-msg me msg)
	       (if (and overnum (> overnum (length msg)))
		   (mew-thread-set-line
		    me
		    (concat (aref spacer (- overnum (max (length msg) numlen)))
			    line))
 		 (mew-thread-set-line me line))
	       (if (string= my-id "")
		   (setq top (cons me top))
		 (mew-thread-set-myid me my-id)
		 (set (intern my-id db) me)
		 (if (string= prnt-id "")
		     (setq top (cons me top))
		   (mew-thread-set-prntid me prnt-id)
		   (setq prnt (symbol-value (intern-soft prnt-id db)))
		   (if (null prnt)
		       (setq top (cons me top))
		     (setq prnt-cld (mew-thread-get-child prnt))
		     (if prnt-cld
			 (nconc prnt-cld (list me))
		       (mew-thread-set-child prnt (list me))))))))))
       ;;
       (if (null me)
 	   (message "No target messages")
	 (setq tm2 (current-time))
	 (mew-summary-switch-to-folder vfolder 'thread)
	 (mew-erase-buffer)
	 (mew-hscroll)
	 (mew-summary-toggle-disp-msg (if disp 'on 'off))
	 (mew-sinfo-set-cache-time ctime)
	 (mew-sinfo-set-case case)
	 (setq mew-summary-buffer-raw t)
	 (mew-vinfo-set-top nil)
	 (mew-vinfo-set-db db)
 	 (mew-vinfo-set-column column)
	 (mew-vinfo-set-flds (list folder))
	 ;;
	 (message "Making thread (second pass)...")
	 (setq tm3 (current-time))
	 (if (null mew-use-complete-thread)
	     (mew-vinfo-set-top (nreverse top))
	   ;; This may create looped thread.
	   ;; See mew-use-complete-thread for more information.
	   (while top
	     (setq me (car top))
	     (if (not (and (mew-thread-get-myid me)
			   (setq prnt-id (mew-thread-get-prntid me))))
		 (mew-vinfo-set-top (cons me (mew-vinfo-get-top)))
	       (setq prnt (symbol-value (intern-soft prnt-id db)))
	       (if (null prnt)
		   (mew-vinfo-set-top (cons me (mew-vinfo-get-top)))
		 (setq prnt-cld (mew-thread-get-child prnt))
		 (if prnt-cld
		     (nconc prnt-cld (list me))
		   (mew-thread-set-child prnt (list me)))))
	     (setq top (cdr top))))
	 ;;
	 (setq tm4 (current-time))
	 (message "Displaying thread...")
	 (setq tm5 (current-time))
 	 (setq mew-thread-indent-list nil)
	 (cond
	  (mew-use-fancy-thread
	   (mew-summary-fancy-thread-print-top
 	    (mew-vinfo-get-top) folder column))
	  (t
	   (mew-summary-thread-print
	    (mew-vinfo-get-top) folder column)))
	 ;; Indent property insert
 	 (mew-elet
 	  (while (setq indentlst (car mew-thread-indent-list))
 	    (put-text-property (nth 0 indentlst) (nth 1 indentlst)
 			       'mew-thread-indent (nth 2 indentlst))
 	    (setq mew-thread-indent-list (cdr mew-thread-indent-list))))
	 ;;
	 ;; Unmarking in both Summary and Thread
	 (when mark (mew-mark-undo-mark mew-mark-review))
	 (setq tm6 (current-time))
	 (when mew-gemacs-p
	   (jit-lock-register 'mew-summary-cook-region))
	 (set-buffer-modified-p nil)
	 (if disp-msg
	     (mew-summary-jump-message disp-msg)
	   (goto-char (point-max)))
	 (mew-summary-display nil)
	 (mew-summary-thread-move-cursor)
	 (message "Displaying thread...done")
	 (run-hooks 'mew-thread-display-hook)
	 (when (mew-debug 'thread)
	   (let* ((t1 (mew-time-calc tm2 tm1))
		  (t2 (mew-time-calc tm4 tm3))
		  (t3 (mew-time-calc tm6 tm5)))
	     (message "pass1 %f, pass2 %f, visual %f" t1 t2 t3))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Visualizing thread
;;;

(defun mew-summary-thread-print (tree folder column)
  (let ((tree-stack nil)
	(level 0) pos)
    (while tree
      (let* ((me (car tree))
	     (cld (mew-thread-get-child me)))
	(mew-elet
	 (if (= level 0) (mew-thread-insert-separator))
	 (insert (mew-thread-get-line me))
	 (forward-line -1)
	 (move-to-column column)
 	 (cond
 	  ((= level 0)
 	   (setq mew-thread-indent-list
 		 (cons (list (point) (1+ (point)) 0) mew-thread-indent-list)))
 	  ((< level mew-thread-indent-array-size)
 	   (setq pos (point))
 	   (insert (aref mew-thread-indent-array level))
 	   (setq mew-thread-indent-list
 		 (cons (list pos (point) level) mew-thread-indent-list)))
 	  (t
 	   (setq pos (point))
	   (let* ((i 0)
		  (max (1- mew-thread-indent-array-size)) ;; take care
		  (lim (/ level max))
		  (j (% level max)))
	     (while (< i lim)
	       (insert (aref mew-thread-indent-array max))
	       (setq i (1+ i)))
 	     (insert (aref mew-thread-indent-array j))
 	     (setq mew-thread-indent-list
 		   (cons (list pos (point) level) mew-thread-indent-list)))))
	 (end-of-line)
	 (insert " \006 " folder " " (mew-thread-get-msg me))
	 (forward-line))
	(setq tree (cdr tree))
	(cond
	 (cld
	  (setq tree-stack (cons tree tree-stack))
	  (setq tree cld)
	  (setq level (1+ level)))
	 (t
	  (while (and (null tree) tree-stack)
	    (setq tree (car tree-stack))
	    (setq tree-stack (cdr tree-stack))
	    (setq level (1- level)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Visualizing thread (fancy style)
;;;

(defun mew-summary-fancy-thread-print-top (top folder column)
  (while top
    (let* ((me (car top))
	   (cld (mew-thread-get-child me)))
      (mew-elet
       (mew-thread-insert-separator)
       (insert (mew-thread-get-line me))
       (forward-line -1)
       (move-to-column column)
       (setq mew-thread-indent-list
 	     (cons (list (point) (1+ (point)) 0) mew-thread-indent-list))
       (end-of-line)
       (insert " \006 " folder " " (mew-thread-get-msg me))
       (forward-line))
      (if cld (mew-summary-fancy-thread-print-tree cld folder column))
      (setq top (cdr top)))))

(defun mew-summary-fancy-thread-print-tree (tree folder column)
  (let ((tree-stack nil)
	(prefix "")
	(level 1) pos)
    (while tree
      (let* ((me (car tree))
	     (next (cdr tree))
	     (cld (mew-thread-get-child me)))
	(mew-elet
	 (insert (mew-thread-get-line me))
	 (forward-line -1)
	 (move-to-column column)
 	 (setq pos (point))
	 (if next
	     (insert prefix (aref mew-fancy-thread-indent-strings 0))
	   (insert prefix (aref mew-fancy-thread-indent-strings 1)))
 	 (setq mew-thread-indent-list
 	       (cons (list pos (point) level) mew-thread-indent-list))
	 (end-of-line)
	 (insert " \006 " folder " " (mew-thread-get-msg me))
	 (forward-line))
	(setq tree next)
	(cond
	 (cld
	  (if next
	      (setq prefix
		    (concat prefix (aref mew-fancy-thread-indent-strings 2)))
	    (setq prefix
		  (concat prefix (aref mew-fancy-thread-indent-strings 3))))
	  (setq tree-stack (cons tree tree-stack))
	  (setq tree cld)
	  (setq level (1+ level)))
	 (t
	  (while (and (null tree) tree-stack)
	    (setq prefix (substring prefix 0 (- mew-thread-indent-length)))
	    (setq tree (car tree-stack))
	    (setq tree-stack (cdr tree-stack))
	    (setq level (1- level)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marking thread
;;;

(defun mew-thread-mark-review ()
  "Put the '*' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-mark mew-mark-review))

(defun mew-thread-mark-delete ()
  "Put the 'D' mark on all messages of the current sub-thread."
  (interactive)
  (mew-summary-not-in-nntp
   (mew-thread-mark mew-mark-delete 'valid-only)))

(defun mew-thread-mark-unlink ()
  "Put the 'X' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-mark mew-mark-unlink 'valid-only))

(defun mew-thread-mark-multi ()
  "Put the '@' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-mark mew-mark-multi))

(defun mew-thread-mark-refile ()
  "Put the 'o' mark on all messages of the current sub-thread."
  (interactive)
  (mew-thread-only
   (let ((folders (mew-summary-refile-body nil nil nil 'no-mark))
	 alist)
     (when folders
       (setq alist (mew-thread-mark mew-mark-refile))
       (mew-refile-set-from-alist alist folders)))))

(defun mew-thread-mark-copy ()
  "Put the 'o' mark on all messages of the current sub-thread
with the current folder as a candidate in addition to guessed folders."
  (interactive)
  (mew-thread-only
   (let ((folders (mew-summary-refile-body
		   nil nil nil 'no-mark (mew-summary-folder-name)))
	 alist)
     (when folders
       (setq alist (mew-thread-mark mew-mark-refile))
       (mew-refile-set-from-alist alist folders)))))

(defun mew-refile-set-from-alist (alist folders)
  (let (ent fld msg msgs)
    (while alist
      (setq ent (car alist))
      (setq alist (cdr alist))
      (setq fld (car ent))
      (setq msgs (sort (copy-sequence (cdr ent)) '<)) ;; sort has side effect
      (while msgs
	(setq msg (int-to-string (car msgs)))
	(setq msgs (cdr msgs))
	(when (get-buffer fld)
	  (save-excursion
	    (set-buffer fld)
	    (mew-refile-reset msg)
	    (mew-refile-set msg folders)))))))

(defun mew-thread-mark (mark &optional valid-only)
  (mew-thread-only
   (mew-summary-msg-or-part
    (let ((column (mew-vinfo-get-column))
	  indent cindent fld msg alist bottom)
      (mew-summary-goto-message)
      (mew-summary-thread-move-cursor)
      (mew-decode-syntax-delete)
      (save-excursion
	(move-to-column column)
	(setq indent (get-text-property (point) 'mew-thread-indent))
	(unless (mew-summary-msg-invalidp)
	  (mew-summary-mark-as mark 'force)
	  (setq fld (mew-summary-folder-name))
	  (setq msg (mew-summary-message-number))
	  (mew-mark-alist-set alist fld msg))
	(forward-line)
	(catch 'loop
	  (while (not (eobp))
	    (move-to-column column)
	    (unless (or (mew-syntax-number) (mew-summary-msg-invalidp))
	      (when (setq cindent
			  (get-text-property (point) 'mew-thread-indent))
		(if (>= indent cindent)
		    (throw 'loop nil)
		  (mew-summary-mark-as mark 'force)
		  (setq fld (mew-summary-folder-name))
		  (setq msg (mew-summary-message-number))
		  (mew-mark-alist-set alist fld msg))))
	    (forward-line)))
	(beginning-of-line)
	(setq bottom (point))
	(mew-summary-mark-in-physical-alist alist mark))
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      ;; for C-x C-x
      (beginning-of-line)
      (let ((zmacs-regions nil)
	    ;; To treat this thread as if it were a single message.
	    (mew-summary-down-function (lambda () (goto-char bottom))))
	(push-mark (point) t t)
	(mew-summary-display-after mew-summary-mark-direction))
      alist))))

(defsubst mew-thread-unmark-one ()
  (beginning-of-line)
  (when (re-search-forward mew-regex-msg nil t)
    (mew-mark-delete-here)))

(defun mew-thread-unmark ()
  "Unmark messages under this sub-thread."
  (interactive)
  (mew-thread-only
   (mew-summary-msg-or-part
    (let ((column (mew-vinfo-get-column))
	  fld msg alist indent cindent)
      (mew-summary-goto-message)
      (mew-summary-thread-move-cursor)
      (mew-decode-syntax-delete)
      (save-excursion
	(move-to-column column)
 	(setq indent (get-text-property (point) 'mew-thread-indent))
	(setq fld (mew-summary-folder-name))
	(setq msg (mew-summary-message-number))
	(mew-mark-alist-set alist fld msg)
	(mew-thread-unmark-one)
	(forward-line)
	(catch 'loop
 	  (while (not (eobp))
	    (move-to-column column)
	    (when (setq cindent (get-text-property
				 (point) 'mew-thread-indent))
	      (if (>= indent cindent)
		  (throw 'loop nil)
		(setq fld (mew-summary-folder-name))
		(setq msg (mew-summary-message-number))
		(mew-mark-alist-set alist fld msg)
		(mew-thread-unmark-one)))
	    (forward-line)))
	(mew-thread-unmark-physical-from-alist alist))))))

(defun mew-thread-unmark-physical-from-alist (alist)
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
	      (beginning-of-line)
	      (when (eq mew-mark-refile
			(and (looking-at mew-regex-msg-mark)
			     (string-to-char (mew-match-string 2))))
		(mew-refile-reset msg))
	      (mew-thread-unmark-one))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread utilities
;;;

(defun mew-summary-thread-up ()
  "Move onto the top of the current thread. If the current message is
a top node, move onto the top of the previous thread."
  (interactive)
  (mew-thread-only
   (let (here pos)
     (mew-summary-goto-message)
     (save-excursion
       (mew-decode-syntax-delete)
       (beginning-of-line)
       (setq pos (point))
       (catch 'loop
 	 (while (and (not (bobp))
 		     (setq pos (previous-single-property-change
 				pos 'mew-thread-indent)))
 	   (when (and pos (eq (get-text-property pos 'mew-thread-indent) 0))
 	     (throw 'loop (setq here pos))))))
     (if (not here)
	 (message "No more threads")
       (goto-char here)
       (mew-summary-thread-move-cursor)
       (mew-summary-display nil)))))

(defun mew-summary-thread-down ()
  "Move onto the top of the next thread."
  (interactive)
  (mew-thread-only
   (let (here)
     (mew-summary-goto-message)
     (save-excursion
       (mew-decode-syntax-delete)
       (forward-line)
       (setq here (text-property-any (point) (point-max)
 				     'mew-thread-indent 0)))
     (if (not here)
	 (message "No more threads")
       (goto-char here)
       (if (not (mew-summary-message-number)) (forward-line))
       (mew-summary-thread-move-cursor)
       (mew-summary-display nil)))))

(defun mew-summary-thread-parent ()
  "Move onto the parent message of the current message."
  (interactive)
  (mew-summary-goto-message)
  (mew-decode-syntax-delete)
  (let ((pos (point))
	(par-id (mew-summary-parent-id))
	key)
    (if (or (null par-id) (string= par-id ""))
	(message "No required info")
      (setq key (format "\r <%s> " par-id))
      (if (or (search-backward key nil t)
	      (progn
		(goto-char (point-max))
		(search-backward key nil t)))
	  (progn
	    (mew-summary-thread-move-cursor)
	    (mew-summary-display nil)
	    (message "Parent found"))
	(goto-char pos)
	(message "No parent")))))

(defun mew-summary-thread-child ()
  "Move onto the child message of the current message."
  (interactive)
  (mew-summary-goto-message)
  (mew-decode-syntax-delete)
  (let ((pos (point))
	(my-id (mew-summary-my-id))
	key)
    (if (or (null my-id) (string= my-id ""))
	(message "No required info")
      (setq key (format "\r <[^>]*> <%s>" (regexp-quote my-id)))
      (if (or (re-search-forward key nil t)
	      (progn
		(goto-char (point-min))
		(re-search-forward key nil t)))
	  (progn
	    (mew-summary-thread-move-cursor)
	    (mew-summary-display nil)
	    (message "Child found"))
	(goto-char pos)
	(message "No child")))))

(defun mew-summary-thread-sibling-up ()
  "Search backward by one sibling message of the current message."
  (interactive)
  (let ((pos (point)) 
	(par-id (mew-summary-parent-id))
	key)
    (if (or (null par-id) (string= par-id ""))
	(message "No required info")
      (setq key (format "\r <[^>]*> <%s>" (regexp-quote par-id)))
      (if (re-search-backward key nil t)
	  (progn
	    (mew-summary-thread-move-cursor)
	    (mew-summary-display nil)
	    (message "Sibling found"))
	(goto-char pos)
	(message "No sibling")))))

(defun mew-summary-thread-sibling-down ()
  "Search forward by one sibling message of the current message."
  (interactive)
  (let ((pos (point))
	(par-id (mew-summary-parent-id))
	key)
    (if (or (null par-id) (string= par-id ""))
	(message "No required info")
      (setq key (format "\r <[^>]*> <%s>" (regexp-quote par-id)))
      (forward-line)
      (if (re-search-forward key nil t)
	  (progn
	    (mew-summary-thread-move-cursor)
	    (mew-summary-display nil)
	    (message "Sibling found"))
	(goto-char pos)
	(message "No sibling")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thread sub-functions
;;;
(defun mew-summary-thread-move-cursor ()
  "Move cursor after indentation of thread."
  (if (and mew-use-thread-cursor
	   (mew-thread-p)
	   (mew-summary-message-number))
      (let (indent)
 	(move-to-column (mew-vinfo-get-column))
 	(if (setq indent (get-text-property (point) 'mew-thread-indent))
 	    (unless (= indent 0)
 	      (goto-char (next-single-property-change (point) 'mew-thread-indent)))
 	  (beginning-of-line)))
    (beginning-of-line)))

(defun mew-summary-thread-get-msglst (tree &optional add-separator)
  "Get a list of message in the thread order specified by TREE."
  (let ((tree-stack nil) (level 0) msgs me cld)
    (while tree
      (setq me (car tree))
      (setq cld (mew-thread-get-child me))
      (if (and mew-use-thread-separator add-separator (= level 0))
	  (setq msgs (cons "s" msgs))) ;; "s" thread-separator line
      (setq msgs (cons (mew-thread-get-msg me) msgs))
      (setq tree (cdr tree))
      (if (null cld)
	  (while (and (null tree) tree-stack)
	    (setq tree (car tree-stack))
	    (setq tree-stack (cdr tree-stack))
	    (setq level (1- level)))
	(setq tree-stack (cons tree tree-stack))
	(setq tree cld)
	(setq level (1+ level))))
    (if (and mew-use-thread-separator add-separator)
	;; discard first "s"
	(cdr (nreverse msgs))
      (nreverse msgs))))

(provide 'mew-thread)

;;; Copyright Notice:

;; Copyright (C) 2000-2003 Mew developing team.
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

;;; mew-thread.el ends here
