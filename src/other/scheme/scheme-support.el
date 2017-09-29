;;; Racket mode
;; ============

(setq auto-mode-alist
      (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))

;; SCRIBBLE
;; ========
(setq auto-mode-alist
      (cons '("\\.scbl$" . scheme-mode) auto-mode-alist))


(provide 'scheme-support)



;;; Quack
;;; =====
(require 'quack)

;; (setq quack-default-program "/home/choppell/apps/racket/racket-6.1.1/bin/mzscheme")
;; (setq quack-programs  '("/home/choppell/apps/racket/racket-6.1.1/bin/mzscheme"))

;;; from http://www.stifflog.com/files/emacs.txt

;;; Scheme config

(defun my-run-scheme ()
  (split-window-vertically)
  (run-scheme)
  (enlarge-window -15)
  (windmove-up))

(defun my-surround-sexp()
  (insert "(")
  (forward-sexp)
  (insert ")")
  (backward-sexp)
  (forward-char))

(defun scheme-load-current-file()
  (scheme-load-file buffer-file-name))

(add-hook 'scheme-mode-hook
          (lambda () 
	    (setq scheme-program-name 
		  "/home/choppell/apps/racket/racket-6.1.1/bin/mzscheme"
		  )

	    (define-key scheme-mode-map [(return)] 'quack-newline)
	    (define-key scheme-mode-map [(?\s-l)] 'my-run-scheme)
	    (define-key scheme-mode-map [(?\s-c)] 'scheme-load-current-file)
;;;	    (define-key scheme-mode-map [(?\s-p)] 'my-surround-sexp)
;;;	    (define-key scheme-mode-map [(?\c-c r)] 'scheme-send-region-and-go)

))

(add-hook 'inferior-scheme-mode-hook
	  (lambda ()
	    (define-key inferior-scheme-mode-map [(return)] 'quack-newline)
;;;	    (define-key scheme-mode-map [(?\c-h)] 'help)
	    (define-key inferior-scheme-mode-map [(shift return)] 'comint-send-input)))


;; begin commented 2015-04-07
(require 'geiser)
;; (load "~/emacs/lisp/geiser-0.6/build/elisp/geiser-load")
 (setq geiser-impl-installed-implementations '(racket guile))
 (setq geiser-active-implementations '(racket))
 (setq geiser-default-implementation 'racket)
 (setq geiser-racket-binary "/home/choppell/apps/racket/racket-6.1.1/bin/mzscheme")
;; end commented 2015-04-07
(require 'cl)



;; 2015-04-07
          ;; 8.x demands the geiser package for scheme
;;          "/home/choppell/emacs/lisp/geiser-0.6/build/elisp"
;;          "/home/choppell/emacs/lisp/geiser-0.6/elisp"
