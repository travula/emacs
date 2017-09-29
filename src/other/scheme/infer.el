;; Support for Infer 
;; - Venkatesh Choppella

(setq auto-mode-alist (append '(("\\.is$" . scheme-mode))
			      auto-mode-alist))

(autoload 'run-alt-scheme "scheme-support" 
	  "Switch to interactive Chez Scheme buffer." t)

;(setq alt-scheme-program-name
;      "/u/dzeng/infer/infer.tup/Scheme4.1b/infer2")

(setq alt-scheme-program-name
      "/u/choppell/infer/implementation/exec/infer.exec")

(setq alt-scheme-argument "")

(setq alt-scheme-hook
 '(lambda ()
    (setq comint-prompt-regexp "^|- *")
    (put 'the 'scheme-indent-hook t)
    (put 'type 'scheme-indent-hook t)
    (put 'lambda* 'scheme-indent-hook t)
    (put 'abstype 'scheme-indent-hook t)
    (put 'record 'scheme-indent-hook t)
    (put 'pair 'scheme-indent-hook t)
    (put 'proc 'scheme-indent-hook t)
    (put 'proc* 'scheme-indent-hook t)))

