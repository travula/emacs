;;;      emacs.el

;;; pub customization of GNU emacs, v1.0 -- chaynes, 5 June 87


(setq auto-mode-alist (append '(
				("\\.ss$" . scheme-mode)
				("\\.t$" . text-mode)
				("\\.doc$" . text-mode)
			        ("\\.html$" . html-mode))
			      auto-mode-alist))

(autoload 'shell "shell" "Switch to interactive Unix shell buffer." t)

(defvar fix-mismatch nil
  "*True if mismatched closing delimiters are to be replaced by their matching 
open delimiter.")

(setq dired-listing-switches "-alb")
(setq TeX-dvi-print-command "ptex")
(setq search-delete-char ?\^h)
(setq search-quote-char ?\^\\)
(setq mail-header-separator "====")
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)
(setq require-final-newline t)
(setq window-min-height 1)

;; key-bindings defined here

(load "keys")

(defun noop ()
  "Do nothing, quietly."
  (interactive))


;; Cursor Motion

(defun line-to-top-of-window ()
  "Puts current line at top of window."
  (interactive)
  (recenter 0))

(defun line-to-bottom-of-window ()
  "Puts current line at bottom of window."
  (interactive)
  (recenter '-))

(defun save-and-kill-current-buffer ()
  "Save the current buffer and then kill it."
  (interactive)
  (if (buffer-file-name) (save-buffer))
  (kill-buffer (current-buffer)))

(defun buffer-process-send-string (arg)
  "Prompt for a string and send it to the current process, followed by a 
return.  This is useful, for example, when the power of scheme-send-input 
gets in the way, as when responding to the Chez interrupt handler menu."
  (interactive "sString to send process: ")
  (process-send-string (get-buffer-process (current-buffer)) 
		       (concat arg "\n")))

(defun top-of-window ()
  "Position point at first line of window."
  (interactive)
  (move-to-window-line 0))

(defun bottom-of-window ()
  "Position point at last line of window."
  (interactive)
  (move-to-window-line -1))

(defun insert-file-name-with-completion (name)
  "Prompts for a file or directory name and inserts that name after point.
The name may be non-existent.  Useful in Shell mode."
  (interactive "FInsert file name: ")
  (insert  name))

(defun shell-command-on-buffer (command)
  "Prompt for a shell command, and then pipe the whole buffer through it."
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command))

;(setq TeX-mode-hook 'TeX-mode-hook)
   
(defun args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (args-to-list (substring string (+ 1 where)
					  (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (args-to-list (substring string pos (length string)))))))))

(defun rlogin (args)
  "Rlogin to iuvax in a terminal-emulator window."
  (interactive "sRlogin arguments: ")
  (let ((args-list (args-to-list args)))
    (terminal-emulator (concat "*" (car args-list) "*") 
		       "rlogin" args-list)))

(defun shell-other-window ()
  "Switch-to-buffer-other-window and then Shell."
  (interactive)
  (switch-to-buffer-other-window "*shell*")
  (shell))


(defun hangfill-paragraph ()
  "Fill paragraph at or after point with hanging indentation.
The indentation of the first line is unchanged and all other lines
get the indentation of the second line."
  (interactive)
  (let (fill-prefix)
    (save-excursion
      (forward-paragraph)
      (or (bolp) (newline 1))
      (let ((end (point)))
	(backward-paragraph)
	(let ((start (point)))
	  (forward-line 2)
	  (setq fill-prefix (buffer-substring 
			     (point)
			     (progn (skip-chars-forward " \t") (point))))
	  (fill-region-as-paragraph start end))))))

(defun newline-and-indent-relative-maybe ()
  "Newline followed by indentation of previous line."
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun blink-up-list (arg)
  "Up-list and blink-matching-open."
  (interactive "p")
  (up-list arg)
  (blink-matching-open))

(defun blink-matching-open ()
  "Move cursor momentarily to the beginning of the sexp before point."
  ;; Modified from simple.el with fix-mismatch code,
  ;; and error instead of message at end. -- chaynes 
  (interactive)
  (and (> (point) (1+ (point-min)))
       (/= (char-syntax (char-after (- (point) 2))) ?\\ )
       blink-matching-paren
       (let* ((oldpos (point))
	      (blinkpos)
	      (mismatch)
	      (matching-delimiter))
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region (max (point-min)
					(- (point) blink-matching-paren-distance))
				   oldpos))
	     (condition-case ()
		 (setq blinkpos (scan-sexps oldpos -1))
	       (error nil)))
	   (if (and blinkpos (/= (char-syntax (char-after blinkpos))
				 ?\$))
	       (progn
		 (setq matching-delimiter (logand (lsh (aref (syntax-table)
					     (char-after blinkpos))
				       -8)
				  ?\177))
		 (setq mismatch (/= last-input-char matching-delimiter))))
	   (if (and mismatch fix-mismatch)
	       (progn
		 (save-excursion
		   (goto-char oldpos)
		   (backward-char 1)
		   (insert matching-delimiter)
		   (delete-char 1)
		   (setq mismatch nil))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
		(goto-char blinkpos)
		(if (pos-visible-in-window-p)
		    (sit-for 1)
		  (goto-char blinkpos)
		  (message
		   "Matches %s"
		   (if (save-excursion
			 (skip-chars-backward " \t")
			 (not (bolp)))
		       (buffer-substring (progn (beginning-of-line) (point))
					 (1+ blinkpos))
		     (buffer-substring blinkpos
				       (progn
					(forward-char 1)
					(skip-chars-forward "\n \t")
					(end-of-line)
					(point)))))))
	     (cond (mismatch
		    (error "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (error "Unmatched parenthesis"))))))))

;; from replace.el, modified so n works like DEL -- chaynes

(defconst query-replace-help
  "Type Space to replace one match, DEL or RET to skip to next,
ESC to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match."
  "Help message while in query-replace")

(defun perform-replace (from-string to-string
		        query-flag regexp-flag delimited-flag)
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(literal (not regexp-flag))
	(search-function (if regexp-flag 're-search-forward 'search-forward))
	(search-string from-string)
	(keep-going t)
	(lastrepl nil))			;Position after last match considered.
    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (push-mark)
    (while (and keep-going
		(not (eobp))
		(progn
		 (set-mark (point))
		 (funcall search-function search-string nil t)))
      ;; Don't replace the null string 
      ;; right after end of previous replacement.
      (if (eq lastrepl (point))
	  (forward-char 1)
	(undo-boundary)
	(if (not query-flag)
	    (replace-match to-string nocasify literal)
	  (let (done replaced)
	    (while (not done)
	      ;; Preserve the match data.  Process filters and sentinels
	      ;; could run inside read-char..
	      (let ((data (match-data))
		    (help-form
		     '(concat "Query replacing "
			      (if regexp-flag "regexp " "")
			      from-string " with " to-string ".\n\n"
			      (substitute-command-keys query-replace-help))))
		(setq char help-char)
		(while (= char help-char)
		  (message "Query replacing %s with %s: " from-string to-string)
		  (setq char (read-char))
		  (if (= char ??)
		      (setq unread-command-event help-char char help-char)))
		(store-match-data data))
	      (cond ((= char ?\e)
		     (setq keep-going nil)
		     (setq done t))
		    ((= char ?^)
		     (goto-char (mark))
		     (setq replaced t))
		    ((= char ?\ )
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq done t))
		    ((= char ?\.)
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq keep-going nil)
		     (setq done t))
		    ((and (not replaced) (= char ?\,))
		     (replace-match to-string nocasify literal)
		     (setq replaced t))
		    ((= char ?!)
		     (or replaced
			 (replace-match to-string nocasify literal))
		     (setq done t query-flag nil))
		    ((or (= char ?\177) (= char ?\015)) ; DEL or RET
		     (setq done t))
		    ((= char ?\C-l)
		     (recenter nil))
		    ((= char ?\C-r)
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit)))))
		    ((= char ?\C-w)
		     (delete-region (match-beginning 0) (match-end 0))
		     (store-match-data
		       (prog1 (match-data)
			 (save-excursion (recursive-edit))))
		     (setq replaced t))
		    (t
		     (setq keep-going nil)
		     (setq unread-command-event char)
		     (setq done t))))))
	(setq lastrepl (point))))
    (pop-mark)
    keep-going))

(setq initial-major-mode 'lisp-interaction-mode)

;;; Advise kill-all-local-variables not to kill server-buffer-clients.
(fset 'old-kill-all-local-variables
      (symbol-function 'kill-all-local-variables))
(defun kill-all-local-variables ()
  (let ((tmp (assoc 'server-buffer-clients (buffer-local-variables))))
    (old-kill-all-local-variables)
    (if tmp
	(setq server-buffer-clients (cdr tmp)))))

;;; Make a keyboard translate table and initialize it to the identity.
(setq flow-control-keyboard-translate-table (make-string 128 0))
(let ((i 0))
  (while (< i 128)
    (aset flow-control-keyboard-translate-table i i)
    (setq i (1+ i))))
;;; Now, map C-] to C-s, and C-q and C-s to C-^ (on vt100s C-^ is C-`).
(aset flow-control-keyboard-translate-table ?\^] ?\^s)
(aset flow-control-keyboard-translate-table ?\^s ?\^^)
(aset flow-control-keyboard-translate-table ?\^q ?\^^)

(defun flow ()
"Handle C-s/C-q flow control by mapping C-] to C-s and 'disable' C-s and C-q
by mapping them to C-^ (which is not bound)."
  (interactive)
  (setq keyboard-translate-table flow-control-keyboard-translate-table))

(defun unflow ()
"Turn off flow control handling.  See the function flow."
  (interactive)
  (setq keyboard-translate-table nil))

;;; Turn on flow control handling.
; (global-unset-key "\C-q")
; (flow)
