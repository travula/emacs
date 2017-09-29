;; Support for Scheme
;; 
;; Venkatesh Choppella 
;; 4/6/94

(load "iuscheme" nil t)

(setq completion-ignored-extensions
 (cons ".so" completion-ignored-extensions))
(setq alt-scheme-argument "~/.schemerc")

;; For file name completion in scheme

(defun scheme-mode-tab-binding ()
  (interactive)
  (funcall (complete-or-default 'scheme-indent-line)))

(setq scheme-mode-hook
      '(lambda ()
	 (turn-on-auto-fill)

					;For file-name completion
					;in scheme mode
	 (require 'shell-completion "shell-completion")
	 (setq file-name-beginning-delimiter-regexp "\"")
	 (local-set-key "\C-i" 'scheme-mode-tab-binding)

					;Anurag's scheme library
;	 (load-file "/nfs/moose/u/anurag/emacs/schlib.el")
;	 (setq scheme-lib-path '("/nfs/moose/u/anurag/scheme/scheme-lib.ss"
;			"/nfs/moose/u/anurag/scheme/scheme-functions.hpx"))

					;Abbrevs
	 (read-abbrev-file "~/emacs/abbrevs")
	 (abbrev-mode 1)

	 (put 'extend-syntax 'scheme-indent-hook 1)
	 (put 'record-case 'scheme-indent-hook 1)
	 (put 'variant-case 'scheme-indent-hook 1)
	 (put 'when 'scheme-indent-hook 1)
	 (put 'unless 'scheme-indent-hook 1)
	 (put 'with 'scheme-indent-hook 1)
	 (put 'safe-record-case 'scheme-indent-hook 1)

))


;; font-lock stuff, jrossie 5 feb 95
;
;(setq scheme-mode-hook 'font-lock-mode)
;
;(if (x-display-color-p)
;    (progn
;
;      (set-foreground-color "white")
;      (set-background-color "darkgreen")
;
;      (copy-face 'bold 'bold-orange)
;      (set-face-foreground 'bold-orange "orange")
;
;      (copy-face 'default 'default-orange)
;      (set-face-foreground 'default-orange "orange")
;
;      (copy-face 'default 'default-red)
;      (set-face-foreground 'default-red "red")
;
;      (copy-face 'bold 'bold-yellow)
;      (set-face-foreground 'bold-yellow "yellow")
;
;      (copy-face 'default 'default-yellow)
;      (set-face-foreground 'default-yellow "yellow")
;
;      (copy-face 'bold 'bold-blue)
;      (set-face-foreground 'bold-blue "plum")
;
;      (copy-face 'default-yellow 'keyword)
;
;      (setq font-lock-string-face 'bold-blue)
;
;      (setq font-lock-comment-face 'default-orange)
;      
;      (setq font-lock-keyword-face 'keyword))
;  
;    ;; black & white font lock
;
;  (progn
;
;      (set-foreground-color "black")
;      (set-background-color "white")
;
;      (copy-face 'bold 'keyword)
;
;      (setq font-lock-string-face 'underline)
;
;      (setq font-lock-comment-face 'italic)
;      
;      (setq font-lock-keyword-face 'keyword)))
;
;
;
;(setq font-lock-keywords 
;  (append
;    '("[[(]class\\b"
;       "[[(]public\\b"
;       "[[(]methods\\b"
;       "[[(]method\\b"
;       "[[(]own\\b"
;       "[[(]shared\\b"
;       "[[(]base\\b"
;       "[[(]hidden\\b"
;       "[[(]read-only\\b"
;       "[[(]private\\b"
;       "[[(]protected\\b"
;       "[[(]base-init\\b"
;       "[[(]inst-vars\\b"
;       "[[(]base-inst-vars\\b"
;       "[[(]base-methods\\b"
;       )
;    '("(and\\b"
;       "(begin\\b"
;       "(case\\b"
;       "(case-lambda\\b"
;       "(cond\\b"
;       "(define\\b"
;       ("(define-record\\b" 0 'keyword keep)
;       ("(define-syntax\\b" 0 'keyword keep)
;       "(extend-syntax\\b"
;       "(fluid-let\\b"
;       "(if\\b"
;       "(lambda\\b"
;       "(let\\b"
;       ("(letrec\\b" 0 'keyword keep)
;       ("(let\\*\\W" 0 'keyword keep)
;       "(or\\b"
;       "(quote\\b"
;       "(rec\\b"
;       "(record-case\\b"
;       "(set!\\W"
;       "(syntax\\b"
;       ("(syntax-case\\b" 0 'keyword keep)
;       "(trace-lambda\\b"
;       "(unless\\b"
;       "(variant-case\\b"
;       "(when\\b"
;       "(with-syntax\\b"
;       "(with\\b"
;       ("\\(#\\\\.\\|#\\\\w+\\|#[tf]\\)" 0 font-lock-string-face keep)
;       ("'()" 0 font-lock-string-face keep)
;       ("'[-$%><_=?:*]*\\w+\\([-_<>=?:*$%.]+\\w*\\)*" 0 font-lock-string-face
;	 keep)
;       "("
;       ")"
;       "[][`',@]"
;       )))
;
;
;(defun toggle-tabify ()
;  (interactive)
;  (setq indent-tabs-mode (not indent-tabs-mode)))





