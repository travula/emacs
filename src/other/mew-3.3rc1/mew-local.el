;;; mew-local.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 12, 2001

(defvar mew-local-folder-alist-file ".mew-folder-alist")
(defvar mew-local-folder-alist nil)
(defvar mew-local-folder-alist-time nil)

(defvar mew-local-friend-folder-list-file ".mew-friend-folder-list")
(defvar mew-local-friend-folder-list nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Listing directories
;;; 

(defsubst mew-dir-list (dir)
  (if (file-directory-p (expand-file-name dir))
      (funcall mew-dir-list-function dir)
    ()))

(defun mew-dir-list-with-link-count (dir)
  "Collecting a directory list under DIR.
Subdirectories are expressed by a list.
This function uses two techniques for speed up.

One is to gather candidates of directory by matching
'mew-regex-folder-candidate'. The default is
\"^[^.0-9]\\|^[0-9].*[^0-9]\". So, typical messages whose name is
numeric are not gathered. This makes it faster to check wether or not
each candidate is a directory in 'while' loop.

The other is to see if the link count of a directory is 2. If so, the
directory does not have subdirectories. So, it is not necessary to
trace down. This technique can be used on UNIX variants."
  (let ((default-directory (expand-file-name dir default-directory))
	file dirent dirs ent subdirs)
    (setq dirent (directory-files "." nil mew-regex-folder-candidate))
    ;; MUST sort
    (while dirent
      (setq file (car dirent))
      (setq ent (mew-file-chase-links file))
      (setq dirent (cdr dirent))
      (when (file-directory-p ent)
	(setq dirs (cons file dirs))
	(when (and (mew-file-get-links ent) ;; necessary
		   (/= (mew-file-get-links ent) 2))
	  (setq subdirs (mew-dir-list-with-link-count file))
	  (if subdirs (setq dirs (cons subdirs dirs))))))
    (nreverse dirs)))

(defun mew-dir-list-without-link-count (dir)
  "Collecting a directory list under DIR.
Subdirectories are expressed by a list.
This function uses one technique for speed up.

It is to gather candidates of directory by matching
'mew-regex-folder-candidate'. The default is
\"^[^.0-9]\\|^[0-9].*[^0-9]\". So, typical messages whose name is
numeric are not gathered. This makes it faster to check wether or not
each candidate is a directory in 'while' loop."
  (let ((default-directory (expand-file-name dir default-directory))
	file dirent dirs ent subdirs)
    (setq dirent (directory-files "." nil mew-regex-folder-candidate))
    ;; MUST sort
    (while dirent
      (setq file (car dirent))
      (setq ent (mew-file-chase-links file))
      (setq dirent (cdr dirent))
      (when (file-directory-p ent)
	(setq dirs (cons file dirs))
	(setq subdirs (mew-dir-list-without-link-count file))
	(if subdirs (setq dirs (cons subdirs dirs)))))
    (nreverse dirs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder list
;;;

(defun mew-local-folder-entry (ent1 ent2 make-list)
  (if make-list
      ent1
    (mew-folder-func ent1 ent2)))

(defun mew-local-folder-make-alist (dirs prefix &optional make-list)
  (let (fldpfx dir ret ent)
    (if (= (length prefix) 1)
	(setq fldpfx prefix)
      (setq fldpfx (file-name-as-directory prefix)))
    (while dirs
      (setq dir (car dirs))
      (setq dirs (cdr dirs))
      (setq ent (concat fldpfx dir))
      (cond
       ((consp (car dirs)) ;; not listp because nil is a list.
	(cond
	 ((or (equal mew-attach-folder ent) (equal mew-draft-folder ent))
	  (setq ret (cons (mew-local-folder-entry ent nil make-list) ret))
	  (setq dirs (cdr dirs))) ;; skip subfolder
	 (t
	  (setq ret (cons (mew-local-folder-entry (file-name-as-directory ent) dir make-list) ret))
	  (setq ret (nconc (mew-local-folder-make-alist (car dirs) ent make-list) ret))
	  (setq dirs (cdr dirs)))))
       (t
	(if (string-match mew-regex-ignore-folders ent)
	    (setq ret (cons (mew-local-folder-entry ent nil make-list) ret))
	  (setq ret (cons (mew-local-folder-entry ent dir make-list) ret))))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folders
;;;

(defun mew-local-folder-clean-up ()
  (setq mew-local-folder-alist nil)
  (setq mew-local-friend-folder-list nil))

(defun mew-local-folder-alist ()
  (let* ((file (expand-file-name mew-local-folder-alist-file mew-conf-path))
	 (t1 (mew-file-get-time file))
	 (t2 mew-local-folder-alist-time))
    (if (mew-compare-times t1 t2)
	(mew-local-folder-load))
    mew-local-folder-alist))

(defun mew-local-friend-folder-list ()
  mew-local-friend-folder-list)

(defun mew-local-folder-load ()
  (let ((file (expand-file-name mew-local-folder-alist-file mew-conf-path)))
    (setq mew-local-folder-alist
	  (mew-lisp-load mew-local-folder-alist-file))
    (setq mew-local-folder-alist-time (mew-file-get-time file))
    (setq mew-local-friend-folder-list
	  (mew-lisp-load mew-local-friend-folder-list-file))))

(defun mew-local-folder-save ()
  (let ((file (expand-file-name mew-local-folder-alist-file mew-conf-path)))
    (mew-lisp-save mew-local-folder-alist-file
		   mew-local-folder-alist)
    (setq mew-local-folder-alist-time (mew-file-get-time file))
    (mew-lisp-save mew-local-friend-folder-list-file
		   mew-local-friend-folder-list)))

(defun mew-local-folder-set (folders friends)
  (setq mew-local-folder-alist folders)
  (setq mew-local-friend-folder-list friends)
  (mew-local-folder-save))

(defun mew-local-update (&optional interactivep)
  (interactive)
  (unless interactivep
    (add-hook 'kill-emacs-hook 'mew-folder-clean-up)
    (mew-local-folder-load))
  ;;
  (when (or interactivep (null mew-local-folder-alist))
    (let* ((mail-dirs (mew-dir-list mew-mail-path))
	   (from-dirs (mew-dir-list (mew-expand-folder mew-friend-folder)))
	   (folders (mew-local-folder-make-alist mail-dirs mew-folder-local))
	   (friends (mew-local-folder-make-alist from-dirs mew-friend-folder t)))
      (setq folders (nreverse folders))
      (setq friends (nreverse friends))
      (mew-local-folder-set folders friends))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder sub function
;;;

(defun mew-local-folder-pair (folder)
  (let* ((dir (directory-file-name (mew-folder-string folder)))
	 ;; foo/bar  -> foo/bar
	 ;; foo/bar/ -> foo/bar
	 (subdir (file-name-nondirectory dir)))
	 ;; foo/bar -> bar 
	 ;; foo -> foo
    (if (string-match mew-regex-ignore-folders folder)
	(mew-folder-func folder)
      (mew-folder-func folder subdir))))

(defun mew-local-folder-insert (folder)
  "Insert FOLDER to 'mew-local-folder-alist'.
Binary search is used for speed reasons."
  (mew-local-folder-insert1 folder)
  (if (and (string-match (concat "^" (regexp-quote mew-friend-folder)) folder)
	   (not (member folder mew-local-friend-folder-list)))
      (setq mew-local-friend-folder-list
	    (cons folder mew-local-friend-folder-list)))
  (mew-local-folder-save))

(defun mew-local-folder-insert1 (folder)
  (unless (mew-assoc-equal folder mew-local-folder-alist 0)
    (let ((case-fold-search nil)
	  (max (1- (length mew-local-folder-alist)))
	  (min 0) mid crr prv parent)
      (while (> (- max min) 20) ;; 20 is enough?
	(setq mid (/ (+ min max) 2))
	(if (string< (car (nth mid mew-local-folder-alist)) folder)
	    (setq min mid)
	  (setq max mid)))
      (setq crr (nthcdr min mew-local-folder-alist))
      (while (and crr (string< (car (car crr)) folder))
	(setq prv crr)
	(setq crr (cdr crr)))
      (if prv
	  (setcdr prv (cons (mew-local-folder-pair folder) crr))
	(setq mew-local-folder-alist
	      (cons (mew-local-folder-pair folder) crr)))
      ;; registering its parent directory
      (setq parent (file-name-directory (directory-file-name folder)))
      (if parent (mew-local-folder-insert1 parent)))))

(defun mew-local-folder-check (folder &optional ask)
  (let ((absdir (mew-expand-folder folder)))
    (if (file-directory-p absdir)
	t
      (if (or (not ask)
	      (y-or-n-p (format "%s does not exist. Create it? " folder)))
	  (progn
	    (mew-make-directory absdir)
	    (mew-local-folder-insert folder)
	    folder)
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process name
;;;

(defvar mew-local-info-list
  '("bnm" "mkdb" "flush" "rcnt" "rttl" "first"))

(mew-info-defun "mew-local-" mew-local-info-list)

(defconst mew-local-info-prefix "mew-local-info-")

(defsubst mew-local-info-name (bnm)
  (format "%s<%s>" mew-local-info-prefix bnm))

(defsubst mew-local-buffer-name (folder)
  (concat mew-buffer-prefix folder))

(defun mew-local-debug (label string)
  (when (mew-debug 'ls)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scan
;;;

(defun mew-scan-mewls-src (folder &optional range)
  (setq folder (mew-expand-folder2 folder))
  (if range
      (list "-s" (format "%s %s" folder range))
    (list "-s" folder)))

(defun mew-local-retrieve (directive &rest args)
  ;; 'inc  folder opts
  ;; 'scan folder range
  ;; 'vir  opts count func
  (mew-summary-with-mewls
   (let* ((process-connection-type mew-connection-type-for-scan)
	  (bnm (mew-summary-folder-name 'ext))
	  (pnm (mew-local-info-name bnm))
	  (buf (get-buffer-create (mew-local-buffer-name bnm)))
	  range pro opts)
     (message "Scanning %s..." bnm)
     (mew-sinfo-set-scan-form (mew-summary-scan-form bnm))
     (mew-sinfo-set-scan-id nil)
     (mew-info-clean-up pnm)
     (mew-local-set-bnm pnm bnm)
     (cond
      ((eq directive 'inc)
       (mew-local-set-flush pnm (nth 1 args))
       (setq opts (append (nth 2 args) (mew-scan-mewls-src (nth 0 args)))))
      ((eq directive 'scan)
       (setq range (nth 1 args))
       (when (nth 2 args)
	 ;; erasing
	 (mew-local-set-mkdb pnm (mew-summary-mark-collect4))
	 (mew-erase-buffer)
	 (mew-summary-folder-cache-save))
       (setq opts (mew-scan-mewls-src (nth 0 args) range)))
      ((eq directive 'vir)
       (setq opts (nth 0 args))
       (mew-vinfo-set-count (nth 1 args))
       (mew-vinfo-set-func  (nth 2 args))
       (mew-vinfo-set-lra   (nth 3 args))
       (unless (nth 4 args) (mew-erase-buffer))))
     (mew-sinfo-set-start-point (point)) ;; after erase-buffer
     (mew-local-set-rcnt pnm 1)
     (save-excursion
       (set-buffer buf)
       (mew-erase-buffer))
     (setq opts (append (list "-b" mew-mail-path
			      "-l" (int-to-string mew-scan-max-field-length)
			      "-w" (int-to-string mew-scan-wait-for)
			      "-f" (mapconcat 'identity
					      (nthcdr 2 mew-scan-fields)
					      ","))
			opts))
     (setq pro (apply 'start-process pnm buf mew-prog-mewls opts))
     (mew-summary-lock pro "Scanning")
     (mew-set-process-cs pro mew-cs-text-for-read mew-cs-dummy)
     ;; text may be broken, so undecided is very dangerous!
     (set-process-filter   pro 'mew-local-filter)
     (set-process-sentinel pro 'mew-local-sentinel)
     (process-kill-without-query pro))))

(defun mew-local-filter (process string)
  (let* ((width (1- (mew-scan-width)))
	 (pnm (process-name process))
	 (bnm (mew-local-get-bnm pnm))
	 (first (mew-local-get-first pnm))
	 (virtualp (mew-folder-virtualp bnm))
	 (draftp (mew-folder-draftp bnm))
	 vec rttl)
    (mew-local-debug "FILTER" string)
    (mew-filter
     (goto-char (point-max))
     (mew-set-buffer-multibyte nil)
     (mew-elet (insert string))
     (goto-char (point-min))
     (unless first
       (when (looking-at "NumOfMsg: \\([0-9]+\\)")
	 (setq rttl (string-to-int (match-string 1)))
	 (mew-local-set-rttl pnm rttl)
	 (forward-line)
	 (delete-region (point-min) (point)))
       (mew-local-set-first pnm t))
     (while (and (re-search-forward mew-eoh nil t) (not (eobp)))
       (mew-net-status3 bnm (mew-local-get-rttl pnm) (mew-local-get-rcnt pnm))
       (mew-local-set-rcnt pnm (1+ (mew-local-get-rcnt pnm)))
       (mew-set-buffer-multibyte t)
       (setq vec (mew-scan-header draftp))
       (mew-set-buffer-multibyte nil)
       (mew-scan-insert-line bnm vec width nil virtualp)
       (forward-line)
       (delete-region (point-min) (point))))))

(defun mew-local-sentinel (process event)
  (let* ((pnm (process-name process))
	 (bnm (mew-local-get-bnm pnm))
	 (virtualp (mew-folder-virtualp bnm))
	 (mdb (mew-local-get-mkdb pnm))
	 (flush (mew-local-get-flush pnm))
	 opos ent msg mrk)
    (mew-local-debug "SENTINEL" event)
    (mew-filter
     (mew-set-buffer-multibyte t)
     (set-buffer bnm)
     (setq opos (point))
     (goto-char (point-min))
     (while mdb
       (setq ent (car mdb))
       (setq mdb (cdr mdb))
       (setq msg (car ent))
       (setq mrk (cdr ent))
       (when (re-search-forward (mew-regex-jmp-msg msg) nil t)
	 (mew-summary-mark-as mrk 'force)
	 (forward-line)))
     (goto-char opos)
     (if (and virtualp (mew-vinfo-get-func))
	 (funcall (mew-vinfo-get-func)))
     (mew-vinfo-set-func nil)
     (mew-info-clean-up pnm)
     (unless virtualp
       (mew-summary-folder-cache-save))
     (set-buffer-modified-p nil)
     (mew-summary-unlock)
     (if (and (mew-folder-imapp bnm)
	      (not (mew-folder-imap-queuep)))
	 (if (mew-local-get-rttl pnm)
	     (message (substitute-command-keys "\\<mew-summary-mode-map>Type '\\[mew-summary-ls]' to override invalid messages"))
	   ;; This folder has been just created.
	   ;; Ideally scan should not be called. But there is no way
	   ;; to avoid the call.
	   (message ""))
       (message "Scanning %s...done" bnm))
     (run-hooks 'mew-scan-sentinel-hook)
     (when (and mew-auto-flush-queue flush)
       (mew-smtp-flush-queue mew-case-output)))))

;;; Code:

(require 'mew)

(provide 'mew-local)

;;; Copyright Notice:

;; Copyright (C) 2001-2003 Mew developing team.
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

;;; mew-local.el ends here
