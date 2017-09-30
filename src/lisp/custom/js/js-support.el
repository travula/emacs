;;; MOZREPL
;;; -------
;; no longer using mozrepl choppell
;;; js-mode is javascript major mode for MozRepl
;; (autoload 'js-mode "js-mode")
;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
;; (add-hook 'js-mode-hook 'js-mozrepl-custom-setup)
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (defun js-mozrepl-custom-setup ()
;;   (moz-minor-mode 1)
;;   (setq comint-input-sender-no-newline nil))


;; Javascript mode
;; ---------------
;; javascript major mode
;; (autoload 'javascript-mode "js-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
;; (add-hook 'javascript-mode-hook 'javascript-mode-custom-setup)


;; js-comint  
;; http://js-comint-el.sourceforge.net/

(require 'js-comint)
;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")

(setq inferior-js-program-command "~/apps/node-v4.6.0-linux-x64/bin/node")

(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

(defun javascript-mode-custom-setup ()
  (autoload 'javascript-shell "javascript-shell"
    "Switch to interactive Javascript buffer." t)
  (setq comint-input-sender-no-newline nil)
  (setq javascript-shell-prompt-pattern "> ")
  (setq javascript-shell-command "java")
  (setq javascript-shell-command-args 
	'("-jar"  "/home/choppell/apps/rhino1_6R7/js.jar")))


(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

;; Javascipt js2 major mode
;; ------------------------
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'javascript-mode-custom-setup)

(setenv "NODE_NO_READLINE" "1")

