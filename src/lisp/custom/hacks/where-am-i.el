;; determines where I am relative to  iiitmk
;; Invokes the shell script

;; route | grep --max-count=1 default | tr -s " " "|" | cut -d "|" -f2


(defun where-am-i ()

  (interactive)
  (let ((cmd 
	 "route | grep --max-count=1 default | tr -s \" \" \"|\" | cut -d \"|\" -f2"))
    (shell-command-cmd)))

