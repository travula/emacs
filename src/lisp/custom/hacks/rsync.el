(require 'pathname)

;; Rsyncing from dired buffers
;;============================
;; Customizable variables
;; ----------------------

;; association list of rsync attributes. See .emacs for use.
(defvar rsync-info nil)

;; absolute path name of the local directory name
(defvar rsync-local-dir  nil)

;; absolute pathname of directory ending in "/"
;; of the rsync dest dir on server
(defvar rsync-server-dir  nil)

;; name of server
(defvar rsync-server  nil)

;; name of the command for exporting
(defvar rsync-cmd "rsync -rC")
;; -r recursive
;; -C  ignore .cvs type of files

;;============================================================
;; computes the quotient of dir  wrt to
;; rsync-local-dir and appends the result to
;; rsync-remote-dir

(defun compute-rsync-remote-dir (dir)
  (if (not (pathname-descendent-p dir rsync-local-dir))
      (error "directory %s is not a descendent of the wwwhome directory %s"
	     dir rsync-local-dir)
    (concat rsync-server-dir  ;; should use pathname-merge !!!
	    (file-name-as-directory 
	     (file-relative-name dir rsync-local-dir)))))

(defun dired-rsync ()
  "rsyncs marked files, which are assumed to be under
rsync-local-dir to the corresponding directory under
rsync-server-dir on rsync-server"
  (interactive)
  (save-excursion
    (let* ((fns (dired-get-marked-files t))
	   (fns-with-space (mapcar (function (lambda (x) (concat x " "))) fns))
	   ;; default-directory is the directory of the current, i.e., dired buffer
	   (target-dir (compute-rsync-remote-dir default-directory))
	   (maybe-server (if rsync-server (concat rsync-server ":") ""))
	   (maybe-ssh (if rsync-server (concat "ssh " rsync-server) ""))
	   )
      (let* ((mkdir-cmd (concat maybe-ssh " mkdir -p " target-dir " ; "))
	     (rsync-cmd-list `(,rsync-cmd " " 
					  ,@fns-with-space " "
					  ,maybe-server
					  ,target-dir 
					  " &"))
	     (cmd (concat mkdir-cmd (apply 'concat rsync-cmd-list))))
	(print cmd)
	(shell-command cmd)
	))))

(defun rsync-se-101 ()
  (interactive)
  (rsync 'se-101))

;;; For testing, open dired buffer for rsync-local-dir and
;;; uncomment.
;; (with-current-buffer (get-buffer "unit-testing")
;;    (rsync-se-101))


(defun rsync (tag)
  (let* ((info (assq tag rsync-info))
	 (info (if info (cdr info)
		 (error "unable to locate rsync info for tag %s" tag)))
	 (rsync-local-dir  (nth 0 info)) 
	 (rsync-server-dir (nth 1 info)) 
	 (rsync-server     (nth 2 info)) 
	 (rsync-cmd        (nth 3 info)))
    (dired-rsync)
    ))

(provide 'rsync)