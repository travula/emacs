(defvar ESC-O-prefix (make-keymap) "Keymap for arrow and keypad keys")
(global-set-key "\eO" ESC-O-prefix)
; (global-set-key "\e[" ESC-O-prefix)
(define-key ESC-O-prefix "D" 'backward-char)		; <-
(define-key ESC-O-prefix "C" 'forward-char)		; ->
(define-key ESC-O-prefix "A" 'previous-line)		; up-arrow
(define-key ESC-O-prefix "B" 'next-line)		; dn-arrow
(define-key ESC-O-prefix "H" 'home)			; Home

(defvar Control-Z-prefix (make-keymap) "Prefix for control-Z.")
(global-set-key  "\C-z" Control-Z-prefix)

(define-key Control-Z-prefix "|" 'shell-command-on-buffer)
(define-key Control-Z-prefix "!" 'shell-other-window)
(define-key Control-Z-prefix "," 'top-of-window)
(define-key Control-Z-prefix "." 'bottom-of-window)
(define-key Control-Z-prefix "\C-a" 'run-alt-scheme)
(define-key Control-Z-prefix "\C-b" 'line-to-bottom-of-window)
(define-key Control-Z-prefix "\C-c" 'run-scheme)
(define-key Control-Z-prefix "\C-d" 'mark-defun)
(define-key Control-Z-prefix "\C-e" 'mark-sexp)
(define-key Control-Z-prefix "\C-f" 'find-file-other-window)
(define-key Control-Z-prefix "\C-g" 'abort-recursive-edit)
(define-key Control-Z-prefix "\C-h" 'backward-kill-sentence)
(define-key Control-Z-prefix "\C-l" 'line-to-top-of-window)
(define-key Control-Z-prefix "\C-m" 'manual-entry)
(define-key Control-Z-prefix "\C-n" 'gnus)
(define-key Control-Z-prefix "\C-o" 'blink-matching-open)
(define-key Control-Z-prefix "\C-p" 'set-fill-prefix)
(define-key Control-Z-prefix "\C-t" 'toggle-read-only)
(define-key Control-Z-prefix "\C-u" 'blink-up-list)
(define-key Control-Z-prefix "\C-x" 'buffer-process-send-string)
(define-key Control-Z-prefix "\C-z" 'help-command)
(define-key Control-Z-prefix "a" 'append-to-file)
(define-key Control-Z-prefix "b" 'switch-to-buffer-other-window)
(define-key Control-Z-prefix "c" 'scheme-mode)
(define-key Control-Z-prefix "f" 'insert-file-name-with-completion)
(define-key Control-Z-prefix "g" 'goto-line)
(define-key Control-Z-prefix "l" 'latex-mode)
(define-key Control-Z-prefix "m" 'mail-other-window)
(define-key Control-Z-prefix "p" 'ispell-buffer)
(define-key Control-Z-prefix " " 'ispell-word)
(define-key Control-Z-prefix "\C-i" 'ispell-complete-word)
(define-key Control-Z-prefix "q" 'quoted-insert)
(define-key Control-Z-prefix "r" 'rlogin)
(define-key Control-Z-prefix "t" 'tex-mode)
(define-key Control-Z-prefix "x" 'electric-command-history)

(global-set-key "\C-\\" 'call-last-kbd-macro)
(global-set-key "\C-^" 'noop)
(global-set-key "\C-h" 'delete-backward-char) ; overrides help-char
(global-set-key "\C-x " 'set-mark-command)
(global-set-key "\C-x%" 'query-replace-regexp)
(global-set-key "\C-x\C-@" 'set-mark-command)
(global-set-key "\C-x\C-k" 'save-and-kill-current-buffer)
(global-set-key "\C-x!" 'shell)
(global-set-key "\C-x_" 'shrink-window)
(global-set-key "\C-x," 'beginning-of-buffer)
(global-set-key "\C-x." 'end-of-buffer)       ; overrides set-fill-prefix
(global-set-key "\C-x?" 'apropos)
(global-set-key "\C-xc" 'calendar)
(global-set-key "\C-xr" 'rmail) ; overrides copy-rectangle-to-register
(global-set-key "\C-xv" 'view-file)
(global-set-key "\e\C-h" 'backward-kill-word) ; NTS intercepts ESC DEL
					      ; overrides mark-defun

( global-set-key "t" 'text-mode)
( global-set-key "1a" 'first-alphanumeric-character-of-line )
( global-set-key "1c" 'start-scheme )
( global-set-key "1m" 'middle-of-screen )
( global-set-key "1s" 'search-forward )
( global-set-key "e" 'replace-string )

(global-set-key "\es" 'center-line)

