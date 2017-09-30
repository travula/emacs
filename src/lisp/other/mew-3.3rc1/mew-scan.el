;;; mew-scan.el --- Scanning messages for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan info
;;;

(defvar mew-scan-info-list '("folder" "message"))
;; See mew-scan-fields. 0th is fld, 1st is msg (ie num).

(mew-info-defun "mew-scan-" mew-scan-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-scan-form-size-unit '("" "k" "M" "G" "T"))
(defvar mew-vec [0 1 2 3 4 5 6 8 9 10 11 12 13 14 15 16 17 18 19 20]
  "Just for test of (MEW-FOO).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setup
;;;

(defun mew-scan-setup ()
  "Define functions (MEW-FOO) according 'mew-scan-fields-alias'."
  (let ((n (length mew-scan-fields-alias))
	(i 0))
    (while (< i n)
      (fset (intern (concat "MEW-" (nth i mew-scan-fields-alias)))
	    `(lambda () (aref mew-vec ,i)))
      (setq i (1+ i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pre-defined functions for mew-scan-form
;;;

(defun mew-scan-form-num ()
  "A function to return a message number."
  (if VIRTUALP
      (prog1
	(int-to-string (mew-vinfo-get-count))
	(mew-vinfo-set-count (1+ (mew-vinfo-get-count))))
    (MEW-NUM)))

(defun mew-scan-form-mark ()
  "A function to return a mark.
'mew-scan-form-mark-delete' and 'mew-scan-form-mark-review'
effect to this function."
  (let ((id (MEW-ID)) duplicated review)
    (when mew-scan-form-mark-delete
      (when (string-match mew-regex-id id)
	(setq id (match-string 1 id))
	(if (member id (mew-sinfo-get-scan-id)) ;; in Summary mode
	    (setq duplicated t)
	  (mew-sinfo-set-scan-id (cons id (mew-sinfo-get-scan-id))))))
    (when mew-scan-form-mark-review
      (let* ((mew-header-max-depth nil)
	     (to (mew-addrstr-parse-address-list (MEW-TO))))
	(catch 'loop
	  (while to
	    (if (mew-is-my-address mew-regex-my-address-list (car to))
		(throw 'loop (setq review t)))
	    (setq to (cdr to))))))
    (cond
     (duplicated (char-to-string mew-mark-delete))
     (review     (char-to-string mew-mark-review))
     (t " "))))

(defun mew-scan-form-type ()
  "A function to return a mark of content type."
  (let ((ct (MEW-CT))
	(case-fold-search t))
    (cond
     ((mew-scan-message-truncatedp)                "T")
     ((string-match "Multipart/Signed"    ct)      "S")
     ((string-match "Multipart/Encrypted" ct)      "E")
     ((string-match "Application/X-Pkcs7-Mime" ct) "E")
     ((mew-ct-multipartp ct)                       "M")
     ((string-match "Message/Partial"     ct)      "P")
     (t " "))))

(defun mew-scan-form-time ()
  "A function to return a message time, HH:MM"
  (let ((s (MEW-DATE)))
    (if (or (string= s "")
	    (not (string-match mew-time-rfc-regex s)))
	(setq s (mew-time-ctz-to-rfc
		 (mew-file-get-time (mew-expand-folder (MEW-FLD) (MEW-NUM))))))
    (if (string-match mew-time-rfc-regex s)
	(format "%02d:%02d"
		(or (mew-time-rfc-hour) 0)
		(or (mew-time-rfc-min)  0))
      "00:00")))

(defun mew-scan-form-date ()
  "A function to return a date, MM/DD."
  (let ((s (MEW-DATE)))
    (when (or (string= s "")
	      (not (string-match mew-time-rfc-regex s)))
      (setq s (mew-time-ctz-to-rfc
	       (mew-file-get-time (mew-expand-folder (MEW-FLD) (MEW-NUM))))))
    (if (string-match mew-time-rfc-regex s)
	(format "%02d/%02d"
		(mew-time-mon-str-to-int (mew-time-rfc-mon))
		(mew-time-rfc-day))
      "")))

(defun mew-scan-form-year ()
  "A function to return a message year, YYYY"
  (let ((s (MEW-DATE)) year)
    (if (or (string= s "")
	    (not (string-match mew-time-rfc-regex s)))
	"0000"
      (setq year (mew-time-rfc-year))
      (cond
       ((< year 50)
	(setq year (+ year 2000)))
       ((< year 100)
	(setq year (+ year 1900))))
      (int-to-string year))))

(defun mew-scan-form-size ()
  "A function to return the size of the message. Should be used
with -4. See also 'mew-scan-form-size-0k' and 'mew-scan-form-size-huge'."
  (let ((len-1 (1- (length mew-scan-form-size-unit)))
	(SIZE (mew-scan-uid-size (MEW-UID)))
	(i 0) size unit)
    (if (and SIZE (string-match "^[0-9]+$" SIZE))
	(setq size (string-to-int SIZE))
      (setq size (mew-file-get-size (mew-expand-folder (MEW-FLD) (MEW-NUM)))))
    (while (and (< i len-1) (>= size 1000))
      (setq size (/ size 1000))
      (setq i (1+ i)))
    (if (and mew-scan-form-size-huge (>= size 1000))
	"HUGE"
      (setq unit (nth i mew-scan-form-size-unit))
      (if (and mew-scan-form-size-0k (string= unit ""))
	  "0k"
	(concat
	 (if (integerp size)
	     (int-to-string size)
	   (format "%.0f" size))
	 unit)))))

(defun mew-scan-form-extract-addr (addr)
  "Extract addr according to 'mew-scan-form-extract-rule'."
  (let* ((func (if mew-addrbook-for-summary
		   (mew-addrbook-func mew-addrbook-for-summary)))
	 (raw (or (mew-addrstr-parse-address addr) ""))
	 (nickname (if func (funcall func raw)))
	 (rules mew-scan-form-extract-rule)
	 rule ret)
    (catch 'matched
      (while rules
	(setq rule (car rules))
	(setq rules (cdr rules))
	(cond
	 ((and (eq rule 'name)
	       (or (string-match "^\"\\([^\"]+\\)\"[ \t]*<[^>]+>" addr)
		   (string-match "^\\([^<]+\\)<[^>]+>" addr)))
	  (throw 'matched (setq ret (mew-chop (match-string 1 addr)))))
	 ((and (eq rule 'comment)
	       (string-match "^[^(]+(\\(.+\\))" addr))
	  (throw 'matched (setq ret (mew-chop (match-string 1 addr)))))
	 ((eq rule 'address)
	  (throw 'matched (setq ret raw)))
	 ((and (eq rule 'nickname) nickname)
	  (throw 'matched (setq ret nickname)))
	 ((and (stringp rule)
	       (string-match rule addr))
	  (throw 'matched (setq ret (mew-chop (match-string 1 addr))))))))
    (or ret addr)))

(defun mew-scan-form-from ()
  "A function to return an address.
If the message is destined to me AND 'mew-scan-form-from-me-prefix'
is a string, an address on To:, is returned. In this
case, 'mew-scan-form-from-me-prefix' is prepended to the address. 

Otherwise, an address on From: is returned.

Address is converted by 'mew-scan-form-extract-addr'. See also
'mew-scan-form-extract-rule'."
  (let* ((FROM (MEW-FROM)) (TO (MEW-TO))
	 (from (or (mew-addrstr-parse-address FROM) "")))
    (cond
     ((string= FROM "")
      "")
     ((and (stringp mew-scan-form-from-me-prefix)
	   (not (string= TO ""))
	   (mew-is-my-address mew-regex-my-address-list from))
      (mew-replace-white-space
       (concat mew-scan-form-from-me-prefix (mew-scan-form-extract-addr TO))))
     (t
      (mew-replace-white-space (mew-scan-form-extract-addr FROM))))))

(defun mew-scan-form-subj ()
  "A function to return Subject:. Unnecessary white spaces are removed."
  ;; The beginning white spaces have been removed in mew-scan-header
  ;; (mew-keyval).
  (let ((subj (MEW-SUBJ)))
    (if (string= subj "") (setq subj mew-error-no-subject))
    (if mew-decode-broken
	subj
      ;; already well-formatted
      (mew-replace-white-space subj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan from
;;;

(defun mew-summary-scan-form (folder &optional column)
  "Get scan-form from 'mew-summary-refine-scan-form'.
See also 'mew-scan-form-list-list'."
  (let (ret form col)
    (setq ret (mew-folder-spec folder mew-scan-form-list
			       mew-scan-form-list-string-type
			       mew-scan-form-list-list-type))
    (if ret
	(setq form (nth 0 ret) col (nth 1 ret))
      (setq form mew-scan-form))
    (setq form (mew-summary-final-scan-form form))
    (if column
	(if (numberp col) col (mew-thread-column form))
      form)))

(defun mew-summary-final-scan-form (form)
  "Canonicalize the scan-form specified by FORM.
If the first element of FORM is an integer N,
append '((N num) mark) to the FORM.
Otherwise append 'mew-scan-form-header' to FORM."
  (if (integerp (car form))
      (cons (list (car form) 'num) (cons 'mark (cdr form)))
    (append mew-scan-form-header form)))

(defun mew-thread-column (form)
  (let ((col 0) ret ent)
    (catch 'loop
      (while form
	(setq ent (car form))
	(setq form (cdr form))
	(cond
	 ((consp ent)
	  (setq col (+ col (abs (car ent)))))
	 ((stringp ent)
	  (setq col (+ col (string-width ent))))
	 ((eq ent t)
	  (throw 'loop (setq ret col)))
	 (t
	  (setq col (1+ col))))))
    (or ret mew-thread-column)))

(defun mew-summary-scan-form-num (folder)
  (abs (car (rassoc '(num) (mew-summary-scan-form folder)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The engine function to call mew-scan-form-*
;;;

(defun mew-scan-get-piece (spec)
  (let (func width str rightp nopad)
    (if (stringp spec)
	(progn
	  (setq TOTAL (+ TOTAL (length spec))) ;; xxx length
	  spec)
      (if (symbolp spec)
	  (setq width 1 func spec)
	(setq width (nth 0 spec) func (nth 1 spec)))
      (when (and (integerp width) (symbolp func))
	(when (= width 0)
	  (setq width (- WIDTH TOTAL 1))
	  (unless mew-use-spc-padding
	    (setq nopad t)))
	(if (< width 0) (setq width (abs width) rightp t))
	(setq TOTAL (+ TOTAL width))
	(setq func (intern-soft
		    (concat mew-scan-form-func-prefix (symbol-name func))))
	(when (fboundp func)
	  (setq str (funcall func))
	  (if rightp
	      (if (<= (length str) width)
		  (format (format "%%%ds" width) str)
		(setq TOTAL (+ (- TOTAL width) (length str)))
		str) ;; width may exceed.
	    (mew-substring str width nil nopad)))))))

(defun mew-scan-get-line (mew-vec WIDTH &optional VIRTUALP)
  (let* ((TOTAL 0)
	 (line (mapconcat 'mew-scan-get-piece (mew-sinfo-get-scan-form) ""))
	 (my-id "") (par-id "") (first-irt "")
	 fld msg ld onlyone start uid siz)
    (if (string-match mew-regex-id (MEW-ID))
	(setq my-id (match-string 1 (MEW-ID))))
    ;; RFC 2822 says: the "In-Reply-To:" field may be used to identify
    ;; the message (or messages) to which the new message is a reply,
    ;; while the "References:" field may be used to identify a
    ;; "thread" of conversation.
    ;;
    ;; However, even if the References field exists, it may not contain
    ;; a parent's ID. So, if the In-Reply-To field contain one ID, 
    ;; we use it for thread.
    ;;
    ;; (1) The In-Reply-To contains one ID, use it.
    ;; (2) The References contains one or more IDs, use the last one.
    ;; (3) The In-Reply-To contains two or more IDs, use the first one.
    (when (string-match mew-regex-id (MEW-IRT))
      (setq start (match-end 0))
      (setq first-irt (match-string 1 (MEW-IRT)))
      (unless (string-match mew-regex-id (MEW-IRT) start)
	(setq onlyone t)))
    (cond
     (onlyone
      (setq par-id first-irt))
     ((string-match mew-regex-id (MEW-REF))
      (setq start (match-end 0))
      (setq par-id (match-string 1 (MEW-REF)))
      (while (string-match mew-regex-id (MEW-REF) start)
	(setq start (match-end 0))
	(setq par-id (match-string 1 (MEW-REF)))))
     (t
      (setq par-id first-irt)))
    (setq uid (mew-scan-uid-uid (MEW-UID)))
    (cond
     (VIRTUALP
      (setq fld (mew-scan-get-folder mew-vec))
      (setq fld (cdr (assoc fld (mew-vinfo-get-lra))))
      (setq msg (mew-scan-get-message mew-vec))
      (setq ld (format "\r <%s> <%s> %s %s \006 %s %s\n"
		       my-id par-id "" "" fld msg)))
     (uid
      (setq siz (mew-scan-uid-size (MEW-UID)))
      (setq ld (format "\r <%s> <%s> %s %s\n" my-id par-id uid siz)))
     (t
      (setq ld (format "\r <%s> <%s>\n" my-id par-id))))
    (cons line ld)))

;; See also mew-summary-cook-region
(defun mew-scan-insert-line (folder vec width lmsg virtualp)
  (when (get-buffer folder)
    (save-excursion
      (set-buffer folder)
      (let* ((line (mew-scan-get-line vec width virtualp))
	     (opos (point))
	     (omax (point-max))
	     beg med face mark olen nlen)
	(mew-elet
	 (if (null lmsg)
	     (goto-char (point-max))
	   ;; a message marked with 'T'.
	   (goto-char (point-min))
	   (if (not (re-search-forward (mew-regex-jmp-msg lmsg) nil t))
	       (goto-char (point-max)) ;; xxx
	     (beginning-of-line)
	     (setq beg (point))
	     (forward-line)
	     ;; To avoid inserting a line AFTER the cursor underline,
	     ;; keep this line and make it invisible.
	     (put-text-property beg (point) 'invisible t)
	     (forward-line -1)))
	 (setq beg (point))
	 ;; To "insert" just after mew-marker-decode-syntax-end.
	 (insert (car line))
	 (setq med (point))
	 (goto-char beg)
	 (mew-front-nonsticky beg med) ;; for XEmacs
	 (if (and mew-use-highlight-mark
		  (looking-at mew-regex-msg-mark)
		  (setq mark (string-to-char (mew-match-string 2)))
		  (setq face (mew-highlight-mark-get-face mark)))
	     (put-text-property beg med 'face face))
	 (if mew-use-highlight-mouse-line
	     (put-text-property
	      beg med 'mouse-face mew-highlight-mouse-line-face))
	 (goto-char med)
	 (insert (cdr line))
	 (put-text-property med (1- (point)) 'invisible t)
	 ;; Removing the invisible line.
	 (when lmsg
	   ;; UID information will be removed. So, we need to adjust
	   ;; the position.
	   (setq nlen (- (point) beg))
	   (setq beg (point))
	   (forward-line)
	   (when (> opos beg)
	     (setq olen (- (point) beg))
	     (setq opos (- opos (- olen nlen))))
	   (delete-region beg (point))))
	(if (or (eq opos (mew-sinfo-get-start-point))
		(/= opos omax))
	    ;; move the cursor to the original position.
	    (goto-char opos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub-functions for Scan
;;;

(defun mew-scan-header (&optional draftp)
  (let (vec key med str n)
    (setq vec (make-vector (length mew-scan-fields) ""))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (looking-at mew-keyval))
	    (forward-line)
	  (setq key (mew-capitalize (mew-match-string 1)))
	  (setq med (match-end 0))
	  (forward-line)
	  (mew-header-goto-next)
	  (when (setq n (mew-member-case-equal key mew-scan-fields))
	    ;; xxx this may be able to tune up.
	    (mew-header-decode-region key med (point) draftp)
	    ;; We need to keep composite properties of charset.
	    ;; This must be "buffer-substring".
	    (setq str (buffer-substring med (1- (point))))
	    (aset vec n str))))
      (goto-char (point-max)))
    vec))

(defun mew-scan-width ()
  (if (and (integerp mew-summary-scan-width)
	   (> mew-summary-scan-width 40)) ;; xxx
      mew-summary-scan-width
    (if (< (window-width) 80)
	80
      (window-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X-Mew-Uidl:
;;;

(defsubst mew-scan-uid-uid (uid)
  (nth 0 (mew-split uid 32)))

(defsubst mew-scan-uid-size (uid)
  (nth 1 (mew-split uid 32)))

(defun mew-header-insert-xmu (uid siz truncated)
  (when (and (stringp uid) (stringp siz))
    (setq siz (int-to-string (string-to-int siz))) ;; removing 0
    (let (fields)
      (if truncated
	  (setq fields (concat uid " 0" siz)) ;; e.g. 0500 == truncated
	(setq fields (concat uid " " siz)))
      (mew-header-insert mew-x-mew-uidl: fields 'no-fold))))

(defun mew-scan-message-truncatedp ()
  (let ((siz (mew-scan-uid-size (MEW-UID))))
    (and siz (string-match "^0" siz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scanning a folder
;;;

(defun mew-summary-ls (&optional header-only goend)
  "List this folder asynchronously.

In a LOCAL folder: messages in the local folder are scanned according
to the range which you specify.

In a REMOTE folder: messages in the server's folder are cached
according to the range which you specify. If
'mew-pop-header-only'/'mew-imap-header-only'/'mew-nntp-header-only' is
non-nil, only headers of messages are cacched. If executed with
'\\[universal-argument]', these variables are considered reversed."
  (interactive "P")
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let* ((bnm (mew-summary-folder-name 'ext))
	    (case (mew-sinfo-get-case))
	    (fld (mew-sinfo-get-folder))
	    (askp mew-ask-range)
	    (directive 'scan)
	    (get-body (not header-only))
	    scanp range oldformatp dir-newp)
       (mew-summary-folder-cache-load)
       (save-excursion
	 (goto-char (point-min))
	 (if (and (not (eobp))
		  (not (looking-at mew-regex-summary))
		  (mew-folder-localp fld) ;; just in case
		  (y-or-n-p "The format of this scan cache is old. Type 'y' when ready. "))
	     (setq oldformatp t)))
       (if oldformatp
	   (mew-local-retrieve 'scan bnm mew-range-all 'erase)
	 (cond
	  ((interactive-p) ;; "s"
	   (setq askp t)
	   (setq scanp t))
	  ((mew-summary-folder-dir-newp) ;; "g"
	   (setq askp nil)
	   (setq scanp t)))
	 (if (mew-summary-folder-dir-newp) (setq dir-newp t))
	 (if (or (interactive-p) goend) (goto-char (point-max)))
	 (set-buffer-modified-p nil)
	 (if (not scanp)
	     (run-hooks 'mew-summary-ls-no-scan-hook)
	   (mew-window-configure 'summary)
	   ;;
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (mew-unhighlight-cursor-line)
	   ;;
	   (mew-sinfo-set-direction 'down)
	   (cond
	    ((and (mew-folder-remotep fld)
		  (not (mew-folder-imap-queuep)))
	     (if (and dir-newp (mew-folder-imapp fld))
		 (mew-local-retrieve 'scan bnm (mew-range-update bnm))
	       (setq range (mew-input-range-remote bnm))
	       (when (eq range 'sync)
		 (setq range nil)
		 (setq directive 'sync))
	       (cond
		((mew-folder-popp fld)
		 (if (mew-pop-header-only case)
		     (setq get-body (not get-body)))
		 (mew-pop-retrieve  case directive bnm range get-body))
		((mew-folder-imapp fld)
		 (if (mew-imap-header-only case)
		     (setq get-body (not get-body)))
		 (mew-imap-retrieve case directive bnm range get-body))
		((mew-folder-nntpp fld)
		 (if (mew-nntp-header-only case)
		     (setq get-body (not get-body)))
		 (mew-nntp-retrieve case directive bnm range get-body)))))
	    (t ;; local
	     (setq range (mew-input-range bnm askp))
	     (if range
		 (mew-local-retrieve 'scan bnm (nth 0 range) (nth 1 range))
	       (message "range is wrong"))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary file cache
;;;

(defun mew-compare-times (t1 t2)
  ;; Is t1 newer than t2?
  (cond
   ((null t1) nil)
   ((null t2) t) ;; do update
   ((> (nth 0 t1) (nth 0 t2)) t)
   ((= (nth 0 t1) (nth 0 t2))
    (if (> (nth 1 t1) (nth 1 t2)) t nil)) ;; nil if equal
   (t nil)))

(defsubst mew-summary-folder-dir-newp ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (dir (file-chase-links (mew-expand-folder folder)))
	 (t1 (if mew-touch-folder-p
		 (mew-file-get-time
		  (expand-file-name mew-summary-touch-file
				    (mew-expand-folder dir)))
	       (mew-file-get-time dir)))
	 (cache (expand-file-name mew-summary-cache-file dir))
	 (t2 (mew-file-get-time cache)))
    (if (and mew-touch-folder-p (null t1)
	     (file-directory-p dir)
	     (mew-dir-messages (mew-expand-folder dir)))
	t
      (mew-compare-times t1 t2))))

(defsubst mew-summary-folder-cache-newp ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (cache (mew-expand-folder folder mew-summary-cache-file))
	 (t1 (mew-file-get-time cache))
	 (t2 (mew-sinfo-get-cache-time)))
    (mew-compare-times t1 t2)))

(defun mew-summary-folder-cache-load ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (cache (mew-expand-folder folder mew-summary-cache-file)))
    (when (and (file-readable-p cache)
	       (mew-summary-folder-cache-newp))
      (mew-elet
       (mew-erase-buffer)
       (mew-frwlet
	mew-cs-m17n mew-cs-dummy
	(insert-file-contents cache))
       (mew-sinfo-set-cache-time (mew-file-get-time cache))
       (setq mew-summary-buffer-raw t)
       (mew-mark-undo-mark mew-mark-refile 'no-msg)
       (set-buffer-modified-p nil)))))

(defun mew-summary-folder-cache-save ()
  (let* ((folder (mew-summary-folder-name 'ext))
	 (cache (mew-expand-folder folder mew-summary-cache-file)))
    (when (file-writable-p cache)
      (mew-touch-folder folder)
      (save-restriction
	(widen)
	(if (mew-decode-syntax-p)
	    (let ((cbuf (current-buffer))
		  (min (point-min))
		  (max (point-max))
		  (beg (mew-decode-syntax-begin))
		  (end (mew-decode-syntax-end)))
	      (with-temp-buffer
		(mew-insert-buffer-substring cbuf min beg)
		(mew-insert-buffer-substring cbuf end max)
		(mew-frwlet
		 mew-cs-dummy mew-cs-m17n
		 (write-region (point-min) (point-max) cache nil 'no-msg))))
	  ;; (write-region 1 1 ...) does not update the file timestamp
	  ;; but does the directory timestamp. So, we need to delete
	  ;; the file to update the file timestamp.
	  (if (= (point-min) (point-max)) (mew-delete-file cache))
	  (mew-frwlet
	   mew-cs-dummy mew-cs-m17n
	   (write-region (point-min) (point-max) cache nil 'no-msg))
	  (mew-set-file-modes cache))
	(mew-sinfo-set-cache-time (mew-file-get-time cache))))))

;; See also mew-net-folder-clean.
(defun mew-summary-folder-cache-clean (folder)
  "Erase Summary mode then remove and touch the cache file."
  (if (get-buffer folder)
      (save-excursion
	(set-buffer folder)
	(mew-erase-buffer)
	(set-buffer-modified-p nil)))
  (let ((cfile (mew-expand-folder folder mew-summary-cache-file)))
    (if (file-exists-p cfile)
	(write-region "" nil cfile nil 'no-msg))))

(provide 'mew-scan)

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

;;; mew-scan.el ends here
