;;; mew-refile.el --- Refile for Mew

;; Author:  Yoshinari NOMURA <nom@csce.kyushu-u.ac.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jun 11, 1994

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-refile-msgid-alist nil
  "Alist of message-id and folder pair")

(defvar mew-refile-from-alist nil
  "Alist of From: address and folder pair")

(defvar mew-refile-last-folder nil
  "Folder name previously you refiled")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialize function
;;;

(defun mew-refile-setup ()
  ;; load message id alist
  (or mew-refile-msgid-alist
      (setq mew-refile-msgid-alist (mew-lisp-load mew-refile-msgid-file)))
  ;; load from alist
  (or mew-refile-from-alist
      (setq mew-refile-from-alist (mew-lisp-load mew-refile-from-file)))
  (mew-assoc-folder-setup)
  (add-hook 'kill-emacs-hook 'mew-refile-clean-up))

(defun mew-refile-clean-up ()
  (remove-hook 'kill-emacs-hook 'mew-refile-clean-up)
  (if (and mew-refile-from-alist
	   (member 'mew-refile-guess-by-from mew-refile-guess-control))
      (mew-lisp-save mew-refile-from-file mew-refile-from-alist))
  (if (and mew-refile-msgid-alist
	   (member 'mew-refile-guess-by-thread mew-refile-guess-control))
      (mew-lisp-save mew-refile-msgid-file mew-refile-msgid-alist))
  (setq mew-refile-from-alist nil)
  (setq mew-refile-msgid-alist nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess functions 
;;;

;; We have two types of functions in mew-refile-guess-control,
;; guess function and ctrl function.
;; guess function must return a folder list or folder string or nil.
;; guess function must not have a string "ctrl" in its symbol name.
;; ctrl function must have a string "ctrl" in its symbol name.

;; dispatcher returns: ((guess1 guess2 ..) info1 info2 ...) multi  guess mode
;;                     ((guess1)           info1 info2 ...) single guess mode
;;            info1:   ('guess-func-name guess1 guess2 ...)
;;
;; that is, 'car' is a list of judged  folders.
;;          'cdr' is a alist of opinions by guess functions.
;;

(defun mew-refile-guess (&optional auto show-all)
  (let ((funcs mew-refile-guess-control) ret guess info stop)
    (catch 'last
      (while funcs
	(if (string-match "ctrl" (symbol-name (car funcs)))
	    ;; func is control function
	    (when (setq ret (funcall (car funcs) guess auto))
	      (setq stop t)
	      (or show-all (throw 'last t)))
	  ;; func is guess function
	  (setq ret (funcall (car funcs))))
	(unless (listp ret) (setq ret (list ret)))
	(setq info (nconc info (list (cons (car funcs) ret))))
	(unless stop
	  (while ret
	    (mew-addq guess (car ret))
	    (setq ret (cdr ret))))
	(setq funcs (cdr funcs))))
    (setq guess (nreverse guess))
    (if (not mew-refile-ctrl-multi) (setq guess (list (car guess))))
    (cons guess info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess control functions
;;;

(defun mew-refile-ctrl-auto-boundary (guess auto)
  (if auto "stop"))

(defun mew-refile-ctrl-throw (guess auto)
  (if guess "stop"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dispatcher
;;;

(defun mew-refile-guess-from-dispatch (func &optional addr)
  "Dispatcher to make mew-refile-guess-by-from-* consider
mew-refile-guess-from-me-is-special."
  (let ((from (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    ;; if From: is my address, addr is the list extracted from To:, Cc:
    (if (and mew-refile-guess-from-me-is-special
	     (mew-is-my-address mew-regex-my-address-list from))
	(let ((addrs (mew-header-parse-address-list mew-refile-guess-key-list))
	      ret adr)
	  (while addrs
	    (setq adr (funcall func (car addrs)))
	    (mew-addq ret adr)
	    (setq addrs (cdr addrs)))
	  (nreverse ret))
      (funcall func from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by folder
;;;

(defvar mew-assoc-folder nil)

(defun mew-folder-func (full &optional name)
  (cons full name))

(defun mew-assoc-folder-setup ()
  (if mew-use-fast-refile
      (setq mew-assoc-folder 'mew-assoc-folder-fast)
    (setq mew-assoc-folder 'mew-assoc-folder-slow)))

(defun mew-assoc-folder-slow (key alist localp)
  ;; Case-sensitive so you can use capital letters for your folders.
  ;; But this is slow.
  (let ((skey (downcase key))
 	(regex (concat mew-path-separator "$"))
	ent name ret)
    (catch 'loop
      (while alist
 	(setq ent (car alist))
 	(setq name (cdr ent))
 	(when (and (stringp name) (string= (downcase name) skey))
 	  (setq ret ent)
 	  (unless (and localp (string-match regex (car ent)))
	    (throw 'loop nil)))
 	(setq alist (cdr alist))))
    ret))

(defun mew-assoc-folder-fast (key alist localp)
  ;; Case-insensitive so you cannot use capital letters for your folders.
  ;; But this is fast.
  (let ((skey (downcase key)) ret ret2 regex)
    (if (not localp)
	(rassoc skey alist)
      ;; ("+foo/" . "foo")
      ;; ("+foo/foo" . "foo")
      (setq regex (concat mew-path-separator "$"))
      (setq ret (rassoc skey alist))
      (setq ret2 ret)
      (while (and ret (string-match regex (car ret)))
	(setq alist (cdr (member ret alist)))
	(setq ret (rassoc skey alist)))
      (or ret ret2))))

(defun mew-refile-guess-by-folder ()
  ;; Guess folders by the To:/Cc: field with folder alist.
  ;; Mainly used for mailing-list.
  (let* ((to-cc (mew-header-parse-address-list mew-refile-guess-key-list))
	 (proto (mew-sinfo-get-proto))
	 (case (mew-sinfo-get-case))
	 (alist (mew-proto-folder-alist proto case))
	 (localp (mew-folder-localp proto))
	 ent ret ml-addr ml-name)
    (while to-cc
      (setq ml-addr (mew-addrstr-parse-address (or (car to-cc) "")))
      (when ml-addr
	(setq ml-name (mew-addrstr-extract-user ml-addr))
	(setq ent (or (funcall mew-assoc-folder ml-addr alist localp)
		      (funcall mew-assoc-folder ml-name alist localp)))
	(mew-addq ret (nth 0 ent)))
      (setq to-cc (cdr to-cc)))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by alist
;;;

(defun mew-refile-guess-by-alist ()
  ;; Guess folders by user-configured alist.
  (mew-refile-guess-by-alist1 mew-refile-guess-alist))

(defun mew-refile-guess-by-alist1 (rule)
  "Return a guessed folder according to the RULE. The syntax of
RULE is as follows:
    rule      ::= '<rule>
    <rule>    ::= ((<key> <alist>) (<key> <alist>) ... [<special>])
    <alist>   ::= (<value> . <folder>|<rule>) (<value> . <folder>|<rule>) ...
    <special> ::= (t . <folder>) | (nil . <folder>)

There are two special <key>s: @samp{nil} and @samp{t}. @samp{nil} is
used to specify <folder> to be returned when nothing is
guessed. @samp{t} can specify <folder> to be returned in addition to
guessed values. "
  (let (key alist val f-or-r header ent ret)
    (while rule
      (setq key (car (car rule)))
      (setq alist (cdr (car rule)))
      (cond
       ((eq key t)
	(mew-addq ret alist))
       ((eq key nil)
	(or ret (mew-addq ret alist)))
       ((setq header (mew-header-get-value key))
	(while alist
	  (setq val (car (car alist)))
	  (setq f-or-r (cdr (car alist)))
	  (setq ent nil)
	  (when (and (stringp val) (string-match val header))
	    (cond
	     ((stringp f-or-r)
	      (setq ent (mew-refile-guess-by-alist2 val header f-or-r)))
	     ((listp f-or-r)
	      (setq ent (mew-refile-guess-by-alist1 f-or-r)))))
	  (when ent
	    (if (listp ent)
		(while ent
		  (mew-addq ret (car ent))
		  (setq ent (cdr ent)))
	      (mew-addq ret ent)))
	  (setq alist (cdr alist)))))
      (setq rule (cdr rule)))
    (nreverse ret)))

(defun mew-refile-guess-by-alist2 (regex string template)
  ;; regex: "\\([^@]+\\)@\\([^.]+\\)\\.ad\\.jp"
  ;; string: "admin@iij.ad.jp"
  ;; template: "+net/\\2/\\1"
  ;; -> "+net/iij/admin"
  (let (str strs match repl)
    (string-match regex string)
    (setq match (cdr (cdr (match-data))))
    (while match
      (setq str (substring string (car match) (car (cdr match))))
      (setq strs (cons str strs))
      (setq match (cdr (cdr match))))
    (setq strs (cons nil (nreverse strs))) ;; cons nil for 0th
    (while (string-match "\\\\\\([1-9]\\)" template)
      (setq repl (nth (string-to-int (match-string 1 template)) strs))
      (setq template (replace-match repl nil nil template)))
    template))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by thread
;;;

(defun mew-refile-guess-by-thread ()
  ;; Guess folders by thread with alist created by
  ;; mew-refile-guess-by-thread-learn.
  (let ((msgid (or (mew-header-get-value mew-references:)
		   (mew-header-get-value mew-in-reply-to:))))
    ;; search for msgid
    (if (and msgid 
	     (string-match "\\(<[^ \t\n]*>\\)[^>]*\0" (concat msgid "\0")))
	(nth 1 (assoc (match-string 1 msgid) mew-refile-msgid-alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by from folder
;;;

(defun mew-refile-guess-by-from-folder (&optional addr)
  ;; Guess folders by the From: field with folders under +from.
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-from-folder-body addr))

(defun mew-refile-guess-by-from-folder-body (&optional addr)
  (let* ((proto (mew-sinfo-get-proto))
	 (case (mew-sinfo-get-case))
	 (list (mew-proto-friend-folder-list proto case))
	 (sep (if (mew-folder-imapp proto)
		  (mew-imap-separator case)
		mew-path-separator))
	 (qsep (regexp-quote sep))
	 (from (downcase (or addr (mew-header-parse-address mew-from:) "")))
	 (user (mew-addrstr-extract-user from))
	 (from-regex (concat qsep (regexp-quote from) "$"))
	 (user-regex (concat qsep (regexp-quote user) "$")))
    (or
     (mew-member-match2 from-regex list)
     (mew-member-match2 user-regex list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by from
;;;

(defun mew-refile-guess-by-from (&optional addr)
  ;; Guess folders by the From: field with alist created by
  ;; mew-refile-guess-by-from-learn.
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-from-body addr))

(defun mew-refile-guess-by-from-body (&optional addr)
  (let ((from (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    (cdr (assoc from mew-refile-from-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by newsgroup
;;;

(defun mew-refile-guess-by-newsgroups ()
  ;; Guess folders by the Newsgroups field with folder alist.
  (let* ((newsgroups (mew-addrstr-parse-value-list2 
		     (mew-header-get-value mew-newsgroups:)))
	 (proto (mew-sinfo-get-proto))
	 (case (mew-sinfo-get-case))
	 (alist (mew-proto-folder-alist proto case))
	 newsgroup ent ret)
    (while newsgroups
      (setq newsgroup (car newsgroups))
      (setq newsgroups (cdr newsgroups))
      (setq ent (funcall mew-assoc-folder newsgroup alist nil))
      (mew-addq ret (nth 0 ent)))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Guess by default
;;;

(defun mew-refile-guess-by-default (&optional addr)
  ;; Concat +from and user.
  (mew-refile-guess-from-dispatch 'mew-refile-guess-by-default-body addr))

(defun mew-refile-guess-by-default-body (&optional addr)
  (let* ((proto (mew-sinfo-get-proto))
	 (case (mew-sinfo-get-case))
	 (fld (mew-proto-friend-folder proto case))
	 (from (downcase (or addr (mew-header-parse-address mew-from:) ""))))
    (if mew-refile-guess-strip-domainpart
	(setq from (mew-addrstr-extract-user from)))
    (mew-concat-folder fld from case)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Learning functions
;;;

;; mew-refile-guess-learn (buf result)
;;
;; buf is message buffer.
;;
;; result is ((chosen1 chosen2 ...)
;;           (guess-func-name1 guess1 guess2...)
;;           (guess-func-name2 guess1 guess2...))
;;
;; that is, 'car' is a list of user chosen folders.
;;          'cdr' is a list of opinions by guess functions.
;;

(defun mew-refile-guess-learn (buf result)
  (let ((chosen (car result))  ;; (folder1 folder2 ...)
	(info (cdr result))) ;; (guess-func-name guess1 guess2...)
    (save-excursion
      (set-buffer buf)
      (if (member 'mew-refile-guess-by-from mew-refile-guess-control)
	  (mew-refile-guess-by-from-learn chosen info))
      (if (member 'mew-refile-guess-by-thread mew-refile-guess-control)
	  (mew-refile-guess-by-thread-learn chosen info)))))

(defun mew-refile-guess-by-thread-learn (chosen info)
  ;; Create mew-refile-msgid-alist for mew-refile-guess-by-thread.
  (let* ((msgid (mew-header-get-value mew-message-id:)) 
	 (folder (car chosen))
	 ;; ohter people's honest opinion and my honest opinion.
	 (oho info)
	 (mho (cdr (assoc 'mew-refile-guess-by-thread info))))
    (if (and msgid (string-match "<[^ \n>]*>" msgid))
	(setq msgid (match-string 0 msgid)))
    (when (and msgid chosen)
      ;; if my opninion was right, I learn it.
      ;; or a folder was not in other people's opinion,
      ;; I accept it.
      (catch 'match
	(while chosen
	  (if (or (member (car chosen) mho)
		  (not (catch 'find
		    (while oho
		      (and (member (car chosen) (car oho)) (throw 'find t))
		      (setq oho (cdr oho))))))
	      (throw 'match (setq folder (car chosen))))
	  (setq chosen (cdr chosen))))
      (setq mew-refile-msgid-alist
	    (cons (list msgid folder "??")
		  (delq (assoc msgid mew-refile-msgid-alist) ;; delq is right
			mew-refile-msgid-alist))))))

(defun mew-refile-guess-by-from-learn (chosen info)
  ;; Create mew-refile-from-alist for mew-refile-guess-by-from.
  (let* ((from (downcase (or (mew-header-parse-address mew-from:) "")))
	 ;; 'my honest opinion' guessed by mew-refile-guess-by-from.
	 (mho (nth 1 (assoc 'mew-refile-guess-by-from info)))
	 folder to-cc)
    ;; default leaning key X is the address derived from From: header, 
    ;; but only when
    ;;    mew-refile-guess-from-me-is-special is t
    ;;    X is my address.
    ;;    All addresses derived from To: Cc: values
    ;;      point to the only one address Y.
    ;; the learning key is set to Y instead of X.
    (if (and mew-refile-guess-from-me-is-special
	     (mew-is-my-address mew-regex-my-address-list from)
	     (setq to-cc
		   (mew-header-parse-address-list mew-refile-guess-key-list))
	     (= (length to-cc) 1))
	(setq from (car to-cc)))
    (unless (or (or (null from) (null chosen)) (and mho (member mho chosen)))
      ;; I decide which folder is most important among the user chosen
      ;; folders. 
      (catch 'match
	(while chosen
	  ;; searching a folder anyone couldn't predict.
	  (unless (mew-member* (car chosen) info)
	    (throw 'match (setq folder (car chosen))))
	  (setq chosen (cdr chosen))))
      ;; If candidate was found, I memorize it.
      (when folder
	(setq mew-refile-from-alist
	      (cons (cons from folder)
		    (delq (assoc from mew-refile-from-alist) ;; delq is right
			  mew-refile-from-alist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(defun mew-refile-get (msg)
  (assoc msg (mew-sinfo-get-refile)))

(defun mew-refile-set (msg folders)
  (mew-sinfo-set-refile (cons (cons msg folders) (mew-sinfo-get-refile))))

(defun mew-refile-reset (msg)
  (mew-sinfo-set-refile
   (delq (mew-refile-get msg) (mew-sinfo-get-refile))))

(defun mew-refile-change (src dst)
  (let ((ref (mew-refile-get src)))
    (when ref (setcar ref dst))))

(defun mew-refile-folder-check (folder &optional force-to-create)
  "A function to see if FOLDER exists.
Return t if exists or created. Otherwise, return nil."
  (when (stringp folder)
    (cond
     ((mew-folder-popp  folder) nil)
     ((mew-folder-nntpp folder) nil)
     ((mew-folder-virtualp folder) nil)
     ((mew-folder-imapp folder)
      (let* ((case (mew-sinfo-get-case))
	     (mailboxes (mew-imap-folder-alist case)))
	(if (assoc folder mailboxes)
	    t
	  (if (and (y-or-n-p (format "%s does not exist. Create it? " folder))
		   (mew-imap-folder-insert case folder))
	      (progn
		(message "%s will be created" folder)
		t)))))
     (t ;; local
      (let ((absdir (mew-expand-folder folder))  ;; /home/Mail/foo
	    (create-it force-to-create))
	(when absdir
	  (if (file-exists-p absdir)
	      (if (file-directory-p absdir)
		  t ;; exists
		(message "%s is a file" folder)
		nil) ;; xxx exists but a file
	    (unless create-it
	      (if (y-or-n-p (format "%s does not exist. Create it? " folder))
		  (setq create-it t)))
	    (if (not create-it)
		nil ;; not created
	      (mew-make-directory absdir)
	      (mew-local-folder-insert folder)
	      (message "%s has been created" folder)
	      t)))))))) ;; created

(defun mew-refile-decide-folders (buf msg cur-folders &optional auto exfld)
  ;; This functions returns
  ;;  ((folder1 folder2...)
  ;;   (func1 guess11 guess12...)  ;; for learning
  ;;   (func2 guess12 guess22...))
  (let ((proto (mew-proto-to-refile (mew-summary-folder-name 'ext)))
	(case (mew-sinfo-get-case))
	learn-info folders ret cands singlep lst-lst lst fld)
    (save-excursion
      (set-buffer buf)
      ;; copy to buffer local variables
      (mew-sinfo-set-case case)
      (mew-sinfo-set-proto proto)
      (setq learn-info (mew-refile-guess auto)))
    (if auto
	;; if auto is set, simply use the guess.
	(setq folders (car learn-info))
      (cond
       (cur-folders
	;; add new folder 
	(setq cands cur-folders))
       ((nth 1 (car learn-info))
	;; multi guess
	(setq cands (car learn-info)))
       (t 
	;; single guess
	(setq singlep t)
	(setq cands (list (nth 0 (car learn-info))))))
      (setq cands (delete nil cands))
      (when exfld
	;; copying, two folders are necessary
	(setq singlep nil)
	(if (or (null cands) (equal (list exfld) cands))
	    (setq cands (list exfld proto))
	  (setq cands (cons exfld cands))))
      (setq cands (mew-uniq-list cands)) ;; unavoidable
      (unless cands
	(setq cands (list proto)))
      (setq folders (mew-input-refile-folders cands singlep case proto)))
    ;; check folder existence.
    (setq lst-lst (mapcar (lambda (x) (mew-split x ?,)) folders))
    (while lst-lst
      (setq lst (car lst-lst))
      (setq lst-lst (cdr lst-lst))
      (while lst
	(setq fld (car lst))
	(setq lst (cdr lst))
	(if (and fld (not (member fld ret)) (mew-refile-folder-check fld))
	    (setq ret (cons fld ret)))))
    (cons (nreverse ret) (cdr learn-info)))) ;; return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copy
;;;

(defun mew-summary-copy ()
  "Put the refile mark(default is 'o') on this message with
the current folder as a candidate in addition to guessed folders."
 (interactive)
 (mew-summary-msg-or-part
  (mew-summary-not-in-draft
   (mew-summary-local-or-imap
    (mew-summary-refile-body
     nil nil nil nil (mew-summary-folder-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile (aka move)
;;;

(defun mew-summary-refile (&optional report)
 "Put the refile mark(default is 'o') on this message. If already
marked with 'o', it prints where this message will be refiled. This
can overlay other marks. When it overlays, the cursor stays on the
message. If it marks newly, displays the next message. If executed
with '\\[universal-argument]', it displays how the refile rules work in Message mode."
 (interactive "P")
 (mew-summary-msg-or-part
  (mew-summary-not-in-draft
   (mew-summary-local-or-imap
    (if report (mew-summary-refile-report) (mew-summary-refile-body))))))

(defun mew-summary-refile-body (&optional exp-flds auto no-msg no-mark exfld)
  (let (msg folder folders cur-folders mark
	    buf learn-info fld tmp delbuf invalidp)
    (save-excursion
      ;; save the cursor position anyway
      (mew-summary-goto-message)
      (setq invalidp (mew-summary-msg-invalidp))
      ;; on the message
      (setq fld (mew-summary-folder-name))
      (setq msg (mew-summary-message-number)) ;; msg is never nil
      (setq mark (mew-summary-get-mark))) ;; any mark
    (cond
     ((and mark (> (mew-markdb-level mark) (mew-markdb-level mew-mark-refile)))
      (or no-msg
	  (message "Cannot mark here because '%s' is stronger than '%s'"
		   (mew-markdb-name mark) (mew-markdb-name mew-mark-refile))))
     (invalidp
      (or no-msg (message "Cannot mark this invalid message")))
     (t
      (if exp-flds
	  (setq folders exp-flds)
	;; show message if not displayed
	(if (or auto (null (mew-sinfo-get-disp-msg)))
	    (save-excursion
	      (setq buf (generate-new-buffer mew-buffer-prefix))
	      (setq delbuf t)
	      (set-buffer buf)
	      (mew-erase-buffer)
	      (mew-insert-message fld msg mew-cs-autoconv
				  mew-header-reasonable-size)
	      (goto-char (point-min))
	      (if (and (re-search-forward (concat "^$\\|^" mew-subj:) nil t)
		       (not (looking-at "^$")))
		  (let ((med (point)))
		    (forward-line)
		    (mew-header-goto-next)
		    (mew-header-decode-region mew-subj: med (point)))))
	  ;; need to make a cache or a message buffer.
	  (mew-summary-display nil)
	  ;; mew-cache-hit should be first since we want to get 
	  ;; information form the top level header.
	  (setq buf (or (mew-cache-hit fld msg) (mew-buffer-message))))
	(when (and (eq mew-mark-refile mark) (get-buffer fld))
	  (save-excursion
	    (set-buffer fld)
	    (setq cur-folders (cdr (mew-refile-get msg)))))
	(setq learn-info (mew-refile-decide-folders
			  buf msg cur-folders auto exfld))
	(setq folders (car learn-info)))
      ;; we must prevent refiling a message to +queue
      (while folders
	(setq folder (car folders))
	(setq folders (cdr folders))
	(unless (mew-folder-queuep folder)
	  (setq tmp (cons folder tmp))))
      (setq folders (nreverse tmp))
      (setq folders (delete mew-draft-folder folders))
      ;; mark refile
      (unless no-mark
	(when folders
	  (save-excursion
	    (mew-summary-goto-message)
	    (or exp-flds auto (mew-refile-guess-learn buf learn-info))
	    (when (get-buffer fld)
	      (save-excursion
		(set-buffer fld)
		(mew-refile-reset msg)
		(mew-refile-set msg folders)))
	    (mew-mark-unmark))
	  (mew-mark-put-mark mew-mark-refile no-msg) ;; cursor may move
	  (if (mew-virtual-p)
	      (mew-summary-mark-in-physical fld msg mew-mark-refile))))
      (if delbuf (mew-remove-buffer buf))
      ;; memorize last-folder
      (setq mew-refile-last-folder folders)
      (set-buffer-modified-p nil)
      folders)))) ;; return value

(defun mew-summary-refile-report ()
  (let ((proto (mew-proto-to-refile (mew-summary-folder-name 'ext)))
	(case (mew-sinfo-get-case))
	(fld (mew-summary-folder-name))
	(msg (mew-summary-message-number2))
	(win (selected-window))
	(customize-var '(mew-refile-ctrl-multi
			 mew-refile-guess-key-list
			 mew-refile-guess-strip-domainpart
			 mew-refile-guess-from-me-is-special))
	guess)
    (with-temp-buffer
      (mew-insert-message fld msg mew-cs-autoconv
			  mew-header-reasonable-size)
      (mew-sinfo-set-case case)
      (mew-sinfo-set-proto proto)
      (setq guess (mew-refile-guess nil t)))
    (mew-window-configure 'message)
    ;; message buffer
    (mew-elet
     (mew-erase-buffer)
     (save-excursion
       ;; report result of guess.
       (insert (format "** Guess result: %s\n" (car guess)))
       ;; report status of customize variables.
       (insert "\n** Current Configurations:\n\n")
       (while customize-var
	 (insert (format "%-40s:  " (car customize-var)))
	 (insert (format "%s\n"     (eval (car customize-var))))
	 (setq customize-var (cdr customize-var)))
       (insert "\n** Each function's opinion:\n\n")
       ;; report how each functions guessed.
       (setq guess (cdr guess))
       (while guess
	 (insert (format "%-32s  " (car (car guess))))
	 (insert (format "return: %s\n"
			 (mapconcat 'identity (cdr (car guess)) ",")))
	 (setq guess (cdr guess)))))
    (mew-message-clear-end-of)
    (set-buffer-modified-p nil)
    (select-window win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refile again
;;;

(defun mew-summary-refile-again ()
  "Put a refile mark on this message according to the previous 
refile folder."
  (interactive)
  (mew-summary-not-in-draft
   (mew-summary-local-or-imap
    (let (validp)
      (save-excursion
	(mew-summary-goto-message)
	(setq validp (not (mew-summary-msg-invalidp))))
      (when validp
	(message "%s to %s"
		 (mew-summary-message-number)
		 (mew-join ", " mew-refile-last-folder))
	(mew-summary-refile-body mew-refile-last-folder))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auto refile
;;;

(defun mew-summary-auto-refile (&optional mew-mark-review-only)
  "Refile each message in the folder automatically. If 
'mew-refile-auto-refile-skip-any-mark' is non-nil,
any previousely marked message will be skipped.
If '\\[universal-argument]' is specified, only messages marked with
'mew-mark-review' will be conserned."
  (interactive "P")
  (mew-summary-or-thread
   (mew-summary-not-in-draft
    (mew-summary-local-or-imap
     (mew-decode-syntax-delete)
     (let ((mew-use-highlight-x-face nil)
	   (lines (mew-count-lines (point-min) (point-max)))
	   (line 1) (mark nil) msg)
       (cond
	(mew-mark-review-only
	 (setq msg (format "Refile all messages marked with '%c'? "
			   mew-mark-review)))
	(mew-refile-auto-refile-skip-any-mark
	 (setq msg "Refile all non-marked messages? "))
	(t
	 (setq msg "Refile messages including marked with weak marks?")))
       (if (and mew-refile-auto-refile-confirm (not (y-or-n-p msg)))
	   (message "Not refiled")
	 (message "Auto refiling...")
	 (save-window-excursion
	   (goto-char (point-min))
	   (while (not (eobp))
	     (when (looking-at mew-regex-msg-valid)
	       (setq mark (mew-summary-get-mark))
	       (if mew-mark-review-only
		   (and mark
			(char-equal mark mew-mark-review)
			(mew-summary-refile-body nil t 'no-msg))
		 (or (and mark
			  (or mew-refile-auto-refile-skip-any-mark
			      (>= (mew-markdb-level mark)
				  (mew-markdb-level mew-mark-refile))))
		     (mew-summary-refile-body nil t 'no-msg)))
	       (if (= (% (/ (* 100 line) lines) 10) 0)
		   (message "Auto refiling...%s%%" (/ (* 100 line) lines)))
	       (setq line (1+ line)))
	     (forward-line))
	   (message "Auto refiling...done"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; "mx" extension
;;;

(defun mew-summary-mark-copy ()
  "\\<mew-summary-mode-map>
Put the refile mark onto all messages marked with '*', 
with the current folder as a candidate in addition to guessed folders.
This is very convenient to refile all messages picked by '\\[mew-summary-search-mark]'."
  (interactive)
  (mew-summary-not-in-draft
   (mew-summary-local-or-imap
    (let ((mew-use-highlight-x-face nil)
	  last)
      (message "Mark copying...")
      (save-excursion
	(save-window-excursion
	  (goto-char (point-min))
	  (while (re-search-forward mew-regex-msg-valid-review nil t)
	    (setq last (mew-summary-refile-body
			last nil 'no-msg nil (mew-summary-folder-name)))
	    (forward-line))
	  (message "Mark copying...done")))))))

(defun mew-summary-mark-refile ()
  "\\<mew-summary-mode-map>
Put the refile mark onto all messages marked with '*'.
This is very convenient to refile all messages picked by '\\[mew-summary-search-mark]'."
  (interactive)
  (mew-summary-not-in-draft
   (mew-summary-local-or-imap
    (let ((mew-use-highlight-x-face nil)
	  last)
      (message "Mark refiling...")
      (save-excursion
	(save-window-excursion
	  (goto-char (point-min))
	  (while (re-search-forward mew-regex-msg-valid-review nil t)
	    (setq last (mew-summary-refile-body last nil 'no-msg))
	    (forward-line))
	  (message "Mark refiling...done")))))))

(provide 'mew-refile)

;;; Copyright Notice:

;; Copyright (C) 1994-2003 Mew developing team.
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

;;; mew-refile.el ends here
