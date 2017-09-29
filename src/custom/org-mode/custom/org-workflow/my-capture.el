;; adapted  from http://doc.norang.ca/org-mode.html#Capture

;; org-protocol.el is at ~/emacs/org-7.5/lisp
;; (require 'org-protocol)


;;; org-capture
;;; ===========
(require 'org-capture)


(setq org-directory "~/org")
(setq org-default-notes-file  (concat org-directory "/refile.org"))
(define-key global-map "\C-ck" 'org-capture)

;; %U = inactive time-stamp
;; %a = the link
;; %i = active region when capture is called
;; %? = cursor location 
;; 
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n  %i")

              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i")

              ("b" "bookmark" entry (file "~/org/refile.org")
               "* %? \n%U\n%a\n  %i")

              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n%U\n  %i")

              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n  %i" :immediate-finish t)

              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n%U")

              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))))


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol




;;; org-agenda-files
;;; ================
(setq org-agenda-files 
      (mapcar (lambda (c) (concat org-directory "/" c))
         '("agenda.org" "notes.org" "per.org" "refile.org")))





;;; refiling
;;; ========

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;;; Org-refiling
;;; ============
;;; Adapted from http://doc.norang.ca/org-mode.html#Refiling

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))


;;;; Refile settings

; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;;; I see no problem with refiling DONE tasks - vxc [2015-08-08]

; (setq org-refile-target-verify-function 'bh/verify-refile-target)

;;; 

;; getting org-protocol to work
;; http://orgmode.org/worg/org-contrib/org-protocol.html
(require 'org-protocol)


(setq require-final-newline t)



(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)



(global-set-key (kbd "<f9> t")
		'bh/insert-inactive-timestamp)

(setq org-export-with-timestamps nil)


(setq org-read-date-prefer-future nil)
(setq org-read-date-prefer-future 'time)


(setq org-agenda-persistent-filter t)

; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)


(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-src-fontify-natively t)


(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)



;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)


(setq org-catch-invisible-edits 'error)



(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix
				      . utf-8-unix))



(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


(setq org-id-link-to-org-use-id
      'create-if-interactive-and-no-custom-id)


(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>"
                                 "</code>" verbatim))))



(setq org-use-sub-superscripts nil)



(setq org-odd-levels-only nil)

