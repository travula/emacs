;;; Extracted from [[file:org-mode-pristine
;;;v1.0.0-104-g7e3d.org]] and adapted with local
;;;customizations  [choppell 2011-09-24]


(print "begin Venkatesh norang-custom.el")
(print "begin Venkatesh norang-custom.el")
(print "begin Venkatesh norang-custom.el")

;;;
;;; Org Mode
;;;
;;; load-paths are set globally in init.org
;;; To enable bindings in this file, add norang-customize

(require 'cl)
(require 'pp)
;; (pp load-path)

(add-to-list 'auto-mode-alist 
	     '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org-install)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)



(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
;;  (add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)  [choppell 2011-10-03]
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook '(lambda () (setq fill-column 72)) 'append)
(add-hook 'message-mode-hook 
	  '(lambda () (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))
	  'append)

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            ;; (make-variable-buffer-local 'yas/trigger-key)
	    ;; ;; commented out [choppell 2012-06-12]
            ;; (org-set-local 'yas/trigger-key [tab])
	    ;; ;; commented out [choppell 2012-06-12]
            ;; (define-key yas/keymap [tab]
	    ;; 'yas/next-field-group)
	    ;; ;; commented out [choppell 2012-06-12]
            ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
            ;; It expands the files in the directories individually
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined)
            ;; (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)
;;          commented out choppell 2013-07-31
	    ) 'append)

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

;; [choppell 2011-10-03]
;; (setq org-agenda-files (quote ("~/git/org"
;;                                "~/git/org/client1"
;;                                "~/git/org/bzflag"
;;                                "~/git/client2")))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;; (global-set-key (kbd "<f9> b") 'bbdb) [choppell 2011-10-03]
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> i") 'info)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> u") 'bh/untabify)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-M-r") 'org-capture)
;; (global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c k") 'org-capture)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun bh/untabify ()
  (interactive)
  (untabify (point-min) (point-max)))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

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
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING"))
              ("STARTED" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED")))))


;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)
;; I use C-c r to start capture mode when using SSH from my Android phone
(global-set-key (kbd "C-c r") 'org-capture)

;; [choppell 2011-10-03] replaced ~/git/org with ~/org
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n%U\n  %i" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n  %i" :immediate-finish t)
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refile Configuration ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;

; Targets include this file and any file contributing to the agenda - up to 2 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 1)
                                 (org-agenda-files :maxlevel . 1))))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Notes and Tasks to Refile")
                       (org-agenda-overriding-header "Tasks to Refile")))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                (tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAITING"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (todo "WAITING|SOMEDAY"
                      ((org-agenda-overriding-header "Waiting and Postponed tasks")
                       (org-agenda-skip-function 'bh/skip-projects-and-habits)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Notes and Tasks to Refile")
                (org-agenda-overriding-header "Tasks to Refile")))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'bh/skip-projects-and-habits)
                (org-agenda-todo-ignore-scheduled t)
                (org-agenda-todo-ignore-deadlines t)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function 'bh/skip-projects-and-habits)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" todo "WAITING|SOMEDAY"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-agenda-skip-function 'bh/skip-projects-and-habits))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))))

(setq org-tags-match-list-sublevels nil)

(defun bh/weekday-p ()
  (let ((wday (nth 6 (decode-time))))
    (and (< wday 6)
         (> wday 0))))

(defun bh/working-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (bh/weekday-p)
         (or (and (>= hour 8) (<= hour 11))
             (and (>= hour 13) (<= hour 16))))))

(defun bh/mark-p ()
  (let ((hour (nth 2 (decode-time))))
    (or (and (bh/weekday-p)
             (or (= hour 8)
                 (and (>= hour 16) (<= hour 21))))
        (and (not (bh/weekday-p))
             (>= hour 9)
             (<= hour 21)))))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "@farm")
         t)
        ((string= tag "mark")
         (not (bh/mark-p)))
        ((or (string= tag "@errand") (string= tag "phone"))
         (let ((hour (nth 2 (decode-time))))
           (or (< hour 8) (> hour 21))))
        (t
         (if (bh/working-p)
             (setq tag "PERSONAL")
           (setq tag "WORK"))
         (unless (member (concat "-" tag) org-agenda-filter)
           tag)))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;
;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Small windows on my Eee PC displays only the end of long lists which isn't very useful
(setq org-clock-history-length 10)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task to STARTED when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (let ((parent-task (save-excursion (org-back-to-heading) (point))))
    (while (org-up-heading-safe)
      (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
        (setq parent-task (point))))
    (goto-char parent-task)
    parent-task))

(add-hook 'org-agenda-mode-hook '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock) 'append))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (org-get-at-bol 'org-hd-marker))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (cond
       ((equal major-mode 'org-agenda-mode)
        (org-with-point-at pom
          (org-agenda-set-restriction-lock restriction-type)))
       ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
        (org-agenda-set-restriction-lock 'file))
       (t
        (org-with-point-at pom
          (org-agenda-set-restriction-lock restriction-type)))))))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at (or parent-task)
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16)))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)  
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil))))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks 
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

(defun bh/clock-in-bzflagt-task ()
  (interactive)
  (bh/clock-in-task-by-id "dcf55180-2a18-460e-8abb-a9f02f0893be"))

(defun bh/resume-clock ()
  (interactive)
  (if (marker-buffer org-clock-interrupted-task)
      (org-with-point-at org-clock-interrupted-task
        (org-clock-in))
    (org-clock-out)))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items (quote (clock)))

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("PHONE" . ?p)
                            ("QUOTE" . ?q)
                            ("WAITING" . ?w)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("MARK" . ?M)
                            ("NOTE" . ?n)
                            ("BZFLAG" . ?B)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; (require 'bbdb)  [choppell 2011-10-03]
;; (require 'bbdb-com) [choppell 2011-10-03]

;;  (global-set-key (kbd "<f9> p") 'bh/phone-call)[choppell 2011-10-03]

;;
;; [choppell 2011-10-03]
;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
;; (defun bh/phone-call ()
;;   "Return name and company info for caller from bbdb lookup"
;;   (interactive)
;;   (let* (name rec caller)
;;     (setq name (completing-read "Who is calling? "
;;                                 (bbdb-hashtable)
;;                                 'bbdb-completion-predicate
;;                                 'confirm))
;;     (when (> (length name) 0)
;;       ; Something was supplied - look it up in bbdb
;;       (setq rec
;;             (or (first
;;                  (or (bbdb-search (bbdb-records) name nil nil)
;;                      (bbdb-search (bbdb-records) nil name nil)))
;;                 name)))

;;     ; Build the bbdb link if we have a bbdb record, otherwise just return the name
;;     (setq caller (cond ((and rec (vectorp rec))
;;                         (let ((name (bbdb-record-name rec))
;;                               (company (bbdb-record-company rec)))
;;                           (concat "[[bbdb:"
;;                                   name "]["
;;                                   name "]]"
;;                                   (when company
;;                                     (concat " - " company)))))
;;                        (rec)
;;                        (t "NameOfCaller")))
;;     (insert caller)))

(setq org-agenda-ndays 1)

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask)))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\) " subtree-end t)))))
    (if (and (bh/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      next-headline)))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (cond
     ((bh/is-project-p)
      subtree-end)
     ((org-is-habit-p)
      subtree-end)
     (t
      nil))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (cond
     ((bh/is-project-p)
      next-headline)
     ((org-is-habit-p)
      next-headline)
     (t
      nil))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    ;; Consider only tasks with done todo headings as archivable candidates
    (if (member (org-get-todo-state) org-done-keywords)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (daynr (string-to-int (format-time-string "%d" (current-time))))
               (a-month-ago (* 60 60 24 (+ daynr 1)))
               (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
               (this-month (format-time-string "%Y-%m-" (current-time)))
               (subtree-is-current (save-excursion
                                     (forward-line 1)
                                     (and (< (point) subtree-end)
                                          (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
          (if subtree-is-current
              next-headline ; Has a date in this month or last month, skip it
            nil))  ; available to archive
      (or next-headline (point-max)))))

;; (setq org-ditaa-jar-path "~/java/ditaa0_6b.jar")  [choppell 2011-10-03]
;; (setq org-plantuml-jar-path "~/java/plantuml.jar") [choppell 2011-10-03]

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
; (setq org-confirm-babel-evaluate nil)  [choppell 2011-09-24]

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

(setq org-export-latex-listings t)

(setq org-export-html-xml-declaration (quote (("html" . "")
                                              ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                              ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

(setq org-export-allow-BIND t)

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

;; commented out [choppell 2011-10-03]
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;; ;; Make TAB the yas trigger key in the org-mode-hook and enable flyspell mode and autofill
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             ;; yasnippet
;;             (make-variable-buffer-local 'yas/trigger-key)
;;             (org-set-local 'yas/trigger-key [tab])
;;             (define-key yas/keymap [tab] 'yas/next-field-group)
;;             ;; flyspell mode for spell checking everywhere
;;             (flyspell-mode 1)
;;             ;; auto-fill mode on
;;             (auto-fill-mode 1)))

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo ()
  (interactive)
  (widen)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (widen)
  (org-reveal))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/git/org/diary.org")

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Start the weekly agenda today
(setq org-agenda-start-on-weekday nil)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote((daily today remove-match)
                                  #("----------------" 0 16
                                    (org-heading t))
                                  (800 1000 1200 1400 1600 1800 2000))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;;(add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp"))

(require 'org-checklist)

(setq org-enforce-todo-dependencies t)

(setq org-hide-leading-stars nil)

(setq org-startup-indented t)

(setq org-odd-levels-only nil)

(setq org-cycle-separator-lines 2)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings nil)

(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

;; commented out [choppell 2011-10-03]
;; (setq org-link-frame-setup ((vm . vm-visit-folder)
;;                             (gnus . org-gnus-no-new-news)
;;                             (file . find-file-other-window)))

(setq org-log-done (quote time))
(setq org-log-into-drawer t)

(setq org-clock-sound "/usr/local/lib/tngchime.wav")

; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (
;; 			  org-bbdb [choppell 2011-10-03]
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; global STYLE property values for completion
(setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq global-auto-revert-mode t)

(require 'org-crypt)
; Encrypt all entries before saving
;;(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
;(setq org-crypt-key "F0B66B40")

(setq org-use-speed-commands t)
`(setq org-speed-commands-user (quote (("1" . delete-other-windows)
                                      ("2" . split-window-vertically)
                                      ("3" . split-window-horizontally)
                                      ("h" . hide-other)
                                      ("k" . org-kill-note-or-show-branches)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . org-reveal)
                                      ("s" . org-save-all-org-buffers)
                                      ("z" . org-add-note)
                                      ("c" . self-insert-command)
                                      ("C" . self-insert-command)
                                      ("J" . org-clock-goto))))

(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

(require 'org-protocol)

(setq require-final-newline nil)

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-return-follows-link nil)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(setq org-remove-highlights-with-change nil)

;; (add-to-list 'Info-default-directory-list
;; "~/git/org-mode/doc") [choppell 2011-10-03]

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))

(setq org-tags-match-list-sublevels t)

(setq org-agenda-persistent-filter t)

(setq split-width-threshold 9999)

;; Mark parent tasks as started
(defvar bh/mark-parent-tasks-started nil)

(defun bh/mark-parent-tasks-started ()
  "Visit each parent task and change TODO states to STARTED"
  (unless bh/mark-parent-tasks-started
    (when (equal state "STARTED")
      (let ((bh/mark-parent-tasks-started t))
        (save-excursion
          (while (org-up-heading-safe)
            (when (member (nth 2 (org-heading-components)) (list "TODO" "NEXT"))
              (org-todo "STARTED"))))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-parent-tasks-started 'append)

(setq org-enable-priority-commands nil)

(setq org-cycle-include-plain-lists nil)

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))))

(setq org-use-sub-superscripts nil)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(print "end Venkatesh norang.el")
(print "end Venkatesh norang.el")
(print "end Venkatesh norang.el")

