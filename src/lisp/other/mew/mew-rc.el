(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;(add-hook 'mew-init-hook (lambda () (require 'mew-fancy-summary)))

(add-hook 'mew-init-hook 
 (lambda ()
   (setq mew-case-output 
; (mew-guess-output-case)
	 "work-out"
	 )
;   (setq mew-case-input "ornl-in")
   (setq mew-case-input "local")
   (setq mew-prog-pgp "gpg")
   (setq mew-ask-fcc t)
))


(setq mew-icon-directory "/home/choppell/apps/mew-3.3/etc")

     ;; Optional setup (Read Mail menu for Emacs 21):
     (if (boundp 'read-mail-command)
         (setq read-mail-command 'mew))

     ;; Optional setup (e.g. C-xm for sending a message):
     (autoload 'mew-user-agent-compose "mew" nil t)
     (if (boundp 'mail-user-agent)
         (setq mail-user-agent 'mew-user-agent))
     (if (fboundp 'define-mail-user-agent)
         (define-mail-user-agent
           'mew-user-agent
           'mew-user-agent-compose
           'mew-draft-send-message
           'mew-draft-kill
           'mew-send-hook))

;; If you are using Emacs with the --unibyte option or the EMACS_UNIBYTE
;; environment variable for Latin-1, put the folloing into your "~/.emacs".

;;     (set-language-environment "Latin-1")
;;     (set-input-method "latin-1-prefix") ;; or "latin-1-postfix"

;; If you use the following configuration for Latin-1, please remove it.
;; This is an obsolete handling of Latin-1 that can cause Mew to function
;; incorrectly.

;;      (standard-display-european 1)

;; When booting, Mew reads the file "~/.mew.el". All Mew configurations
;; should be written in this file.


;; User configuration
(setq mew-name "Venkatesh Choppella") ;; (user-full-name)
(setq mew-mail-domain "iiitmk.ac.in") 
(setq mew-user "choppell")

;; If you want to use a local mailbox to receive e-mail messages, the
;; followings are necessary.

;; To use local mailbox "mbox" or "maildir" instead of POP
;; (setq mew-mailbox-type 'mbox)
;; (setq mew-mbox-command "incm")
;; (setq mew-mbox-command-arg "-d /var/spool/mail/choppell")
     ;; If /path/to/mbox is a file, it means "mbox".
     ;; If /path/to/mbox is a directory, it means "maildir".

;; If you want to use IMAP to receive e-mail messages, the followings are
;; necessary.

;;     (setq mew-proto "%")
     ;; (setq mew-imap-user "your IMAP account")  ;; (user-login-name)
;;     (setq mew-imap-server "your IMAP server")    ;; if not localhost

;; To read and/or write articles of NetNews, the followings are necessary.

     ;; (setq mew-nntp-user "your NNTP account")
     ;; (setq mew-nntp-server "your NNTP server")


;; If you set `mew-case-synchronize' to `nil', you can specify the
;; receiving case and the sending case independently. With this
;; configuration, `C' is for the receiving case and `C-uC' is for the
;; sending case.
(setq mew-case-synchronize nil)

(setq mew-use-cached-passwd t)
(setq mew-passwd-lifetime 6) 
;; minutes cached = 
;; mew-passwd-lifetime * mew-passwd-timer-unit (default 10)

;; (setq ornl-in
;;       '(("inbox-folder"   . "+inbox-ornl")
;; 	("mail-domain"    . "ornl.gov")
;; 	("user"           . "choppellav")
;; 	("pop-server"     . "mail.ornl.gov") ; IP Addr?
;; 	("pop-ssl"        . t)
;; 	("pop-ssl-port"   . "995")
;; 	("pop-auth"       . pass)
;; ;	("pop-auth-list"  . ("CRAM-MD5"))
;; 	("pop-user"       . "vcc")
;; 	;; delete messages on server after 60 days
;;	("pop-delete"     . 300))) 

;; (setq imap-ornl-in
;;       '(("inbox-folder"   . "+inbox-ornl")
;; 	("mail-domain"    . "ornl.gov")
;; 	("user"           . "choppellav")
;; 	("imap-server"     . "mail.ornl.gov") ; IP Addr?
;; 	("imap-ssl"        . t)
;; 	("imap-ssl-port"   . "993")
;; ;	("imap-auth"       . nil)
;; ;	("pop-auth-list"  . ("CRAM-MD5"))
;; 	("imap-user"       . "vcc")
;; 	("imap-delete"     . 300))) ;; delete messages on server after 60 days

;; (setq ornl-remote 
;;       '(("inbox-folder"   . "$")
;; 	("mail-domain"    . "ornl.gov")
;; 	("user"           . "choppellav")
;; 	("pop-server"     . "mail.ornl.gov") ; 160.91.4.92
;; 	("pop-user"       . "vcc")
;; 	("pop-auth"       . "pass")
;; 	("pop-ssl"        . t)
;; 	("pop-delete"     . nil)))



;; Outgoing mail setup from iiitmk's mail server
;; =============================================

(setq ssh-mapped-smtp-port 9999)
;; This command maps the local ssh-mapped-smtp-port to the smtp
;; server's smtp port
;; Mail sent locally to the ssh-mapped-smtp-port is sent to the server.  


; _____________________________________________________________________
(setq work-out
      '(("smtp-server"    . 
;         "202.88.239.62"   ; mail.iiitmk.ac.in
	 "mail.iiitmk.ac.in"
;	 "192.168.0.5"
;; uncomment these when using iiitmk smtp server
;	 "mail.iiitmk.ac.in" ; 192.168.0.5  internal
	 )
	("smtp-user"      . "choppell")
        ("smtp-ssh-server" . 
	 "mail.iiitmk.ac.in"
;         "202.88.239.62"   ; mail.iiitmk.ac.in
;         "192.168.0.5"   ; mail.iiitmk.ac.in internal
	 )
))

(setq iitd-out
      '(("smtp-server"  .  "ccmailer.iitd.ac.in")
	("mail-domain"  .  "mgh.iitd.ac.in")
))

(setq home-out
      '(("smtp-server"    . "202.88.239.62") ;; mail.iiitmk.ac.in external
	("smtp-user"      . "choppell")
	("smtp-ssl"       . t)
	("smtp-auth-list" . ("LOGIN"))
	("mail-domain"    . "iiitmk.ac.in")))

(setq local 
      '(("mailbox-type"   . mbox)
	("mbox-command"   . "incm")
	("mbox-command-arg" . "-d /var/spool/mail/choppell")))

(setq mew-config-alist
      `(("local"   . ,local)
	("work-out" . ,work-out)
	("iitd-out" . ,iitd-out)
	("home-out" . ,home-out)
 	("default"  . ,local)))
; _____________________________________________________________________

;; default
;; (setq mew-case-default "ornl-in")
;; 'mew-auto-get' is 'nil', just visit to the folder determined by 'proto'.
;; When executed with '\\[universal-argument]', 'mew-auto-get' is
;; considered reversed."
;;  (setq mew-auto-get nil)

;; initialization
(setq mew-theme-file "~/emacs/lisp/mew/mew-theme-custom.el")

(require 'domain-name)
(require 'cl)

(defun mew-guess-output-case ()
  (let ((domain (domain-name)))
    (cond ((or (null domain)(search "localdomain" domain)) "local")
	  ((search "iiitmk" domain) "work-out")
	  ((search "comcast" domain) "home-out")
	  (t "local"))))


(add-hook 'mew-draft-mode-hook
	  (lambda ()
	    ;;  if local then set draft-send-mail to queue messages
	    ;; \C-c\C-c should be bound to mew-draft-make-message
;	    (define-key mew-draft-mode-map "\C-c\C-c" 'mew-draft-make-message)
	    (define-key mew-draft-mode-map
	      "\C-c\C-c" 'mew-draft-save-then-make-message)
	    (define-key mew-draft-header-map "\ep" 
	      'mew-draft-expand)))

(defun mew-draft-save-then-make-message (&optional privacy signer)
  (interactive)
  (save-buffer)
  (mew-draft-make-message privacy signer))
  
(setq mew-header-alist
 '(("Cc:" . "choppell@iiitmk.ac.in")))
(setq mew-noreplyto-to-list '("From:" "To:" "Cc:"))
   

(setq mew-refile-guess-alist
      '(("To:"
	 ("alg-synth@mailhub.ornl.gov" . "+work/research/alg-synth")
	 ("tce-devel@cis.ohio-state.edu" . "+work/research/tce-devel"))
	("From:"
	 ("wolfelm@ornl.gov" . "+admin")
	 ("manningnj@ornl.gov" . "+admin"))))


;; Starting behavior
;; don't fetch messages on start
(setq mew-auto-get nil)

(setq mew-prog-ssl "/usr/sbin/stunnel")

;; without the .localdomain, the mail server mail.iiitmk.ac.in will reject
;; with a 504 "need fully qualified domain " error.  
(setq mew-smtp-helo-domain "localhost.localdomain")


