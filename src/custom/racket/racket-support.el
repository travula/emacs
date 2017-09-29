;;; Racket mode
;; ============
;; see https://github.com/greghendershott/racket-mode/blob/master/README.md

(require 'racket-mode)

;; (add-hook 'racket-mode-hook
;;           (lambda ()
;;             (define-key racket-mode-map (kbd "C-c r")
;; 	      'racket-run)))


;;; keyword lambda:
(add-hook 'racket-mode-hook
	  (lambda ()
		(define-key global-map [(control c) (control r)] 'racket-repl)
	    (font-lock-add-keywords 
	     nil 
	     '(
	       ("\\<\\(lambda:\\)" . font-lock-keyword-face)
               ("\\<\\(scons\\)" . font-lock-keyword-face)
               ("\\<\\(rec\\)" . font-lock-keyword-face)
	       ))))

;;; suggested customizations

(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

(setq tab-always-indent 'complete)

(setq racket-smart-open-bracket-enable t)


(add-to-list ' auto-mode-alist '("\\.scbl$" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'"  . racket-mode))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))



(provide 'racket-support)



