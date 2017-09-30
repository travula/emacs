start-process-command-with-args

(setq kept nil)
nil

(defun keep-output (process output)
  (setq kept (cons output kept)))
keep-output

keep-output


(defun shell-command-with-preemption ()
  (start-process process-name buffer-name "hostname" "-d")
;    (start-process-command-with-args process-name buffer-name command args)
  (set-process-filter (get-process "shell") keep-output)
  (process-send-string "shell" "date\n")
  (accept-process-output (get-process "shell") 5)
  kept)

(domain-name)
"ornl.gov
"
mew-case-output

























(find-domain-name)
(domain-name)







nil

nil


