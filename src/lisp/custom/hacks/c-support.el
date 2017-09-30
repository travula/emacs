;; C stuff
(autoload 'set-c-style "c-style" "C program beautifier" t)
(setq c-mode-hook '(lambda ()
		     (setq compile-command
                         (concat "gcc -g -o "
                                 (substring (buffer-name) 0 -2)
                                 " "
                                 (buffer-name)))
		     (local-set-key "\C-xc" 'compile)
		     (setq c-indent-level 8)
		     (setq c-continued-statement-offset 8)
		     (setq c-brace-offset -8)
		     (setq argdecl-indent 8)
		     (setq c-label-offset -8)
;                   (set-c-style 'BSD)
		     ))

