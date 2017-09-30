;; sml mode uses old shell.el.  see
;;http://mail.gnu.org/pipermail/help-gnu-emacs/1995-March/003029.html
(defalias 'make-shell 'make-comint)
(setq sml-prog-name "/usr/local/bin/sml")
(setq sml-mode-hook '(lambda () 'do-nothing))
(setq auto-mode-alist (append '(("\\.ml$" . sml-mode) 
				("\\.sml$" . sml-mode)) 
			      auto-mode-alist))
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)

