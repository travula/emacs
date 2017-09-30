;; Finds out if we're connected to the outside world.  Sends a
;; "hostname -d" command to the shell.  If we're connected, this
;; will immediately return the domain in a newline terminated
;; string.  Otherwise, after timing out, this will return nil.
;; Bug: the shell process will continue to run for a while if
;; there is no connection.  We need to figure out a way to kill the
;; hostname process

(defun domain-name ()
  (let ((output 'nil))
    (set-process-filter (get-process "shell")
			(function (lambda (process string)
				    (setq output string))))
    (process-send-string "shell" "hostname -d \n")
    (accept-process-output (get-process "shell") 1) ; time out after 1 sec
    (set-process-filter (get-process "shell") nil)
    output))

;; (defmacro start-process-command-with-args 
;;   (process-name buffer-name command &rest args)
;;   `(start-process ,process-name ,buffer-name ,command ,@args))
(provide 'domain-name)
