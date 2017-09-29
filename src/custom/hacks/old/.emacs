;; Emacs support
;; Venkatesh Choppella

; (setq debug-on-error 1)
(setq max-lisp-eval-depth 2000)
(setq max-specpdl-size 6000)

(setq load-path
 (append (list
	  "/u/choppell/emacs/lisp"
	  "/u/choppell/emacs/lisp/lemacs"
;	  "/u/chaynes/emacs/"
	  )
	 load-path))

(load "emacs" nil t)
;(load "ispell" nil t)

;; Emacs library from anurag
;; =========================
;(load-file "/nfs/moose/u/anurag/emacs/emlib.el")
(load-file "~/emacs/lisp/emlib.elc")
(global-set-key "]" 'complete-parens-from-point)
(global-set-key ";" 'comment-out-region)

(setq blink-matching-paren-distance 100000)
(setq emacs-lisp-mode-hook 'turn-on-auto-fill)
(setq text-mode-hook 'turn-on-auto-fill)

;; exported to shell-completions
(make-variable-buffer-local 'file-name-beginning-delimiter-regexp)

;; If you want to use C-s and C-q (and they don't cause problems), uncomment:
;(unflow)

;; Goodbye RMAIL
(global-unset-key "\C-xr")
(global-unset-key "\C-xm")

;; VM
;; (load "vm-support" nil t)

;; C 
;; =
(load "c-support" nil t)

;; Scheme
;; ======

;; A hack to implement lazy loading
;; None of the scheme stuff gets loaded unless either run-scheme or
;; scheme invoked. Basically, this avoids definition of the
;; scheme-mode-hooks and other stuff in scheme-support.el to be loaded
;; when unless scheme (or it's major mode) gets invoked.

(autoload 'run-scheme "scheme-support"
  "Support for scheme process."
  t)

;; Assumes the role of scheme-mode-hook
;(defvar my-scheme-mode-hook nil
;  "*Hook for customizing scheme mode.")
;
;(setq scheme-mode-hook
;      '(lambda ()
;	 (load "scheme-support" nil t)
;					;Should load scheme-support
;					;only the first time.
;					;see my scheme.el for full story
;	 (setq scheme-mode-hook nil)
;	 ))
;
;(fset 'start-scheme "2o")


;; Scheme and Infer
;; ================
(load "scheme-support" nil t)
(infer)					; defined in iuscheme

;; SML 
;; ===
;; (load "sml-support" nil t)

;; TeX
;; ===
(load "tex-support" nil t)


;; CALENDAR
;; (load "calendar-support" nil t)

;; SHELL
;; =====

(defconst shell-prompt-pattern 
  "^\[[A-Za-z0-9]*:[^]]*\]"

  "*Regexp used by Newline command to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not reexecuted.")

(load "shell-support" nil t)
(put 'eval-expression 'disabled nil)
(display-time)



;; What is this for?
;(defvar screen-title-format nil)
;(if screen-title-format ;; for lemacs only
;    (setq load-path 
;	  (append (list "/u/chaynes/emacs/lemacs/")
;		  load-path)))
