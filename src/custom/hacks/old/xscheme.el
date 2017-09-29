;;; -*-Emacs-Lisp-*- Scheme under emacs stuff.
;; Copyright (C) 1985 Bill Rozas & Richard M. Stallman

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Adapted from shell.el to scheme.  

;; Adapted for IU Schemes and cleaned up.
;; Stan Jefferson's history mechanism added.
;; On exit, doesn't ask for confirmation before killing scheme.
;; -- chaynes
;;
;; Modified scheme-send-input to send all s-exps between
;; end-of-buffer and "\n>".  Yes, it's a hack.
;; -- jashley

(require 'scheme)
(require 'shell)

(defvar inferior-scheme-mode-map nil)
(if inferior-scheme-mode-map
    nil
  (setq inferior-scheme-mode-map (copy-alist shell-mode-map))
  (scheme-mode-commands inferior-scheme-mode-map)
  (define-key inferior-scheme-mode-map "\C-c\C-h" 'find-lisp-input)
  (define-key inferior-scheme-mode-map "\n" 'newline-and-indent)
  (define-key inferior-scheme-mode-map "\r" 'scheme-send-input))

(defun inferior-scheme-mode ()
  "Major mode for interacting with an inferior Scheme process.

The following Scheme mode commands are available:

\\{inferior-scheme-mode-map}

C-h converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest
 of expression rigidly with the current line.
C-c i does Tab on each line starting within following expression,
and C-c C-i similarly reindents the current definition.

Return sends the s-expression ending on the current line as input
to Scheme.  If the current line is not the last line of the buffer, 
the s-expression is copied to the end first.

Entry to this mode calls the value of scheme-mode-hook with no arguments,
if that value is non-nil.

You can send text to the inferior Scheme from other buffers
using the commands send-region, send-string and \\[scheme-send-definition]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-scheme-mode)
  (setq mode-name "Inferior Scheme")
  (setq mode-line-format 
        "--%1*%1*-Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
  (scheme-mode-variables)
  (use-local-map inferior-scheme-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (process-kill-without-query (get-process "scheme"))
  (run-hooks 'scheme-mode-hook))

(defun scheme-send-input ()
  "Return sends the s-expression ending on the current line as input
to Scheme.  If the current line is not the last line of the buffer, 
the s-expression is copied to the end first."
  (interactive)
  (let* ((process (get-buffer-process (current-buffer)))
         (pmark (process-mark process))
         (send-it t))
    (end-of-line)
    (if (eobp)
        (progn
          (insert ?\n)
          (save-excursion
            ;(backward-sexp 1)                ; these commented out for \n>
            ;(re-search-backward "[^ \\t]")   ; 
            (re-search-backward "\n>")
            (forward-char 2)
            (if (> (point) pmark) (setq send-it nil)))
          (move-marker last-input-start pmark)
          (move-marker last-input-end (point)))
      (backward-sexp)
      (let ((copy (buffer-substring (point)
                                    (progn (forward-sexp) (point)))))
        (goto-char (point-max))
        (move-marker last-input-start (point))
        (insert copy ?\n)
        (move-marker last-input-end (point))))
    (if send-it
        (progn
          (send-region process last-input-start last-input-end)
          (set-marker pmark (point))))
    (scheme-indent-line)))

(defvar scheme-program-name "scheme"
  "Program invoked by the scheme and run-scheme commands")

(defvar alt-scheme-program-name "scheme"
  "Program invoked by the alt-scheme and run-alt-scheme commands")

(defvar alt-scheme-argument ""
  "Argument for the run-alt-scheme command")

(defun scheme (args)
  "Run an inferior Scheme process reading a command line from the terminal.
By default, the program 'scheme' (generally Chez scheme) is run.
This may be be changed by assigning to the variable scheme-program-name."
  (interactive "sExtra arguments to scheme: ")
  (switch-to-buffer
   (make-comint "scheme" scheme-program-name (args-to-list args)))
  (inferior-scheme-mode))

(defun run-scheme (arg)
  "Run an inferior Scheme process.
Input and output via buffer *scheme*.
With argument it asks for a command line.
By default, the program 'scheme' (generally Chez scheme) is run.
This may be be changed by assigning to the variable scheme-program-name."
  (interactive "P")
  (if (and nil arg)
      (call-interactively 'scheme)
      (progn 
        (switch-to-buffer (make-comint "scheme" scheme-program-name '()))
	(inferior-scheme-mode))))

(defun alt-scheme (args)
  "Run an inferior Scheme process reading a command line from the terminal.
By default, the program 'scheme84' is run.
This may be be changed by assigning to the variable alt-scheme-program-name."
  (interactive "Extra arguments to alt-scheme: ")
  (switch-to-buffer
   (make-comint "scheme" alt-scheme-program-name (list alt-scheme-argument)))
  (inferior-scheme-mode))

(defun run-alt-scheme (arg)
  "Run an inferior Scheme process.
Input and output via buffer *scheme*.
With argument it asks for a command line.
By default, the program 'scheme' is run with the null argument.
This may be be changed by assigning to the variables alt-scheme-program-name
and alt-scheme-argument."
  (interactive "P")
  (if arg 
      (call-interactively 'alt-scheme)
      (progn
	(switch-to-buffer 
	 (make-comint "scheme" alt-scheme-program-name '())
	 (inferior-scheme-mode)))))

(defun scheme-send-definition ()
  "Send the current definition to the inferior Scheme process."
  (interactive)
  (save-excursion
   (end-of-defun)
   (let ((end (point)))
     (beginning-of-defun)
     (send-region "scheme" (point) end)
     (send-string "scheme" "\n"))))

(defun scheme-send-definition-and-go ()
  "Send the current definition to the inferior Scheme process, 
and switch to the *scheme* buffer."
  (interactive)
  (scheme-send-definition)
  (pop-to-buffer "*scheme*"))

;; History mechanisms:

(defconst lisp-input-pattern-prefix "^>+ *("
"A regular expression that matches possible lisp prompt for the
function find-lisp-input.  The outermost left parenthesis is 
usually included for convenience.")

(defun find-lisp-input (occurrence) 
"Implements a history mechanism for interactive lisp buffers (e.g.*lisp*).
When invoked it prompts for a regular expression.  It then searches
for the most recent input expression whose outermost left parenthesis
is followed by an occurrence of the regular expression input by the
user.  If such an expression is found, then a copy is inserted before
point and enclosed in a region.  An immediate re-invocation deletes
the just-copied expression and continues the search for the next most
recent matching expression.

With a numeric prefix argument, the n'th most recent matching
expression is obtained.

Since an empty regular expression will result in a match with every
expression, the entire input history can be cycled through.  To input
the empty string, just press RETURN.

The variable lisp-input-pattern-prefix is used for matching prompts
and allows some customization."

  (interactive "p")
  (let ((destination (point-marker))
        (again (eq last-command 'find-lisp-input))
        (match-beg nil)
        (match-end nil)
        )
    (cond ((not again)
           (setq pattern (read-string "Pattern: "))
           (setq complete-pattern (concat lisp-input-pattern-prefix pattern))
           )
          (t
           (setq complete-pattern last-complete-pattern)
           (delete-region (region-beginning) (region-end))))
    (setq last-complete-pattern complete-pattern)
    (save-excursion
      (if again
          (goto-char last-find-position)
        (beginning-of-line))
      (cond ((re-search-backward complete-pattern (point-min) t occurrence)
             (setq last-find-position (point))
             (search-forward "(")
             (backward-char)
             (mark-sexp 1)
             (setq match-beg (region-beginning))
             (setq match-end (region-end)))
            (t (setq this-command 'foobar)
               (error "Can't find it."))))
    (insert (buffer-substring match-beg match-end))
    (mark-sexp -1)
    (setq this-command 'find-lisp-input)))



