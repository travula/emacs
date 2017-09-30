(setq shell-mode-hook
      '(lambda ()
	 (setq file-name-beginning-delimiter-regexp "[ \t]")
	  (require 'shell-completion  "shell-completion")
         (process-kill-without-query (get-process "shell"))))

;(setq initial-major-mode 'shell-mode)
;(setq shell-read-history nil)
;(shell)
