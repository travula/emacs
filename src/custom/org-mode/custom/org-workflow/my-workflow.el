;;; WORKFLOW
(setq org-todo-keywords
      (quote (
	      (sequence "ACTION(a!)")
	      (sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)")
)))

(setq org-todo-keyword-faces 
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)
              ("ACTION" :foreground "forest green" :weight bold)
)))



;;; Hook for

(defun org-custom-insert-active-timestamp-sans-hm ()
  (interactive)
  (org-insert-time-stamp nil))

(defun org-custom-insert-heading-active-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (org-custom-insert-active-timestamp-sans-hm)))

(add-hook 'org-insert-heading-hook 'org-custom-insert-heading-active-timestamp 'append)

(defun org-custom-insert-state (name)
  (interactive "sState: ")
  (org-insert-heading)
  (insert name)
  (insert " "))

(defun org-custom-insert-action ()
  (interactive)
  (org-custom-insert-state "ACTION"))

(defun org-custom-insert-todo ()
  (interactive )
  (org-custom-insert-state "TODO"))

(defun org-custom-insert-note ()
  (interactive)
  (org-custom-insert-state "NOTE"))

