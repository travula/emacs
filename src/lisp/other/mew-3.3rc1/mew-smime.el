;;
;; This program is for merge purpose only. Need to fix many things.
;;

;; configuration variables
(defcustom mew-smime-digital-id nil
  "*Filename containing your digital ID in the PEM format."
  :group 'mew-privacy
  :type '(file :must-match t))

(defcustom mew-smime-CA-file nil
  "*Filename containing certificates of the trusted CAs, such as VeriSign."
  :group 'mew-privacy
  :type '(file :must-match t))

(defcustom mew-smime-pubkey-dir nil
  "*Directory storing public keys of others"
  :group 'mew-privacy
  :type 'directory)

(defcustom mew-smime-encryption-algorithm "-des3"
  "*Encryption Algorithm"
  :group 'mew-privacy
  :type 'string)


(defcustom mew-smime-additional-certificates nil
  "*Filename containing additional certificates"
  :group 'mew-privacy
  :type '(file :must-match t))

(defun mew-smime-sign-configuration-check ()
  (cond
   ((or (not mew-smime-digital-id)
	(not (file-readable-p mew-smime-digital-id)))
    "Please set mew-smime-digital-id")
   ((or (not mew-smime-additional-certificates)
	(not (file-readable-p mew-smime-additional-certificates)))
    "Please set mew-smime-additional-certificates")
   ((not (mew-which-exec "openssl")) "OpenSSL is not installed.")))

(defun mew-smime-verify-configuration-check ()
  (cond
   ((or (not mew-smime-CA-file) (not (file-readable-p mew-smime-CA-file)))
    "Please set mew-smime-CA-file")
   ((or (not mew-smime-pubkey-dir)
	(not (file-directory-p mew-smime-pubkey-dir)))
    "Please set mew-smime-pubkey-dir")
   ((not (mew-which-exec "openssl")) "OpenSSL is not installed.")))

;; internal variables
(defvar mew-smime-running nil)

(defvar mew-smime-prompt-enter-pass "Enter S/MIME pass phrase: ")

(defconst mew-smime-msg-enter-pass  "Enter PEM pass phrase:")

(defvar mew-smime-string nil)

(defvar mew-smime-sign-error nil)

(defconst mew-smime-passtag "S/MIME")

(defconst mew-smime-signature-filename "smime.p7s")

;; The following variables are used only in the variable
;; mew-decode-multipart-signed-switch in mew-decode.el.
(defvar mew-smime-ver 0)

(defvar mew-prog-smime "openssl")

(defun mew-smime-passphrase ()
  (mew-input-passwd mew-smime-prompt-enter-pass mew-smime-passtag))

(defun mew-smime-process-filter1 (process string)
  ;; sign or decrypt, not verify
  (setq mew-smime-string (concat mew-smime-string string))
  (cond
   ;; pass phrase for sign or decrypt
   ((string-match mew-smime-msg-enter-pass string)
    (process-send-string process (format "%s\n" (mew-smime-passphrase)))
    (set-process-filter process nil))))

(defun mew-smime-process-sentinel (process event)
  (if (string-match "finished" event)
      (progn
	(setq mew-smime-running nil)
	(setq mew-smime-sign-error nil))
    (setq mew-smime-running nil)
    (mew-passwd-set-passwd mew-smime-passtag nil)
    (setq mew-smime-sign-error (mew-chop event)))
  (mew-remove-buffer (process-buffer process)))

(defun mew-smime-sign (file1)
  (if (mew-smime-sign-configuration-check)
      (list nil nil nil (mew-smime-sign-configuration-check))
    (message "S/MIME signing...")
    (setq mew-smime-string nil)
    (setq mew-smime-running 'signing)
    (let ((process-connection-type mew-connection-type2)
	  (file2 (mew-make-temp-name))
	  (buf (generate-new-buffer (concat mew-buffer-prefix "smime")))
	  process)
      ;; not perfectly unique but OK
      (setq process
	    (mew-start-process-lang
	     "S/MIME sign"
	     buf
	     "openssl"
	     "smime" "-sign" "-in" file1 "-out" file2 "-outform" "DER"
	     "-signer" mew-smime-digital-id
	     "-certfile" mew-smime-additional-certificates))
      (mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
      (set-process-filter process 'mew-smime-process-filter1)
      (set-process-sentinel process 'mew-smime-process-sentinel)
      (mew-rendezvous mew-smime-running)
      (message "S/MIME signing...done")
      (list file2 mew-b64 "sha1" mew-smime-sign-error
            (list "application/x-pkcs7-signature")
	    mew-smime-signature-filename))))

(defun mew-smime-verify (file1 file2)
  (message "S/MIME verifying...")
  (if (mew-smime-verify-configuration-check)
      (mew-smime-verify-configuration-check)
    (let ((pubkey-file (mew-make-temp-name)))
      (if (equal 0 (mew-call-process-lang
		    "openssl" nil nil nil
		    "smime" "-verify" "-inform" "DER" "-in" file2
		    "-content" file1
		    "-CAfile" mew-smime-CA-file "-signer" pubkey-file))
	  (concat "valid S/MIME digital signature by " 
		  (mew-smime-move-pubkey-and-extract-email pubkey-file))
	"S/MIME signature verification failed"))))

(defun mew-smime-email-address-to-filename (addr)
  (let ((addr2 (copy-sequence addr)) i)
    (while (setq i (string-match "[/\\\\]" addr2))
      (aset addr2 i ?_))
    (convert-standard-filename
     (expand-file-name addr2 mew-smime-pubkey-dir))))

(defun mew-smime-move-pubkey-and-extract-email (pubkey-file)
  (let (email-addr pubkey-moved)
    (with-temp-buffer
      (mew-call-process-lang "openssl" nil t nil
			     "x509" "-noout" "-email" "-in" pubkey-file)
      (goto-char (point-min))
      (replace-string "\n" "") ;; XXX replace-string should not be used.
      (setq email-addr (mew-buffer-substring (point-min) (point-max))))
    (setq pubkey-moved (mew-smime-email-address-to-filename email-addr))
    (if (and (not (file-exists-p pubkey-moved))
	     (file-writable-p pubkey-moved))
	(copy-file pubkey-file pubkey-moved))
    (mew-delete-file pubkey-file)
    email-addr))

;; The followin function is based on mew-decode-multipart-encrypted.
;; A major problem is that we cannot distinguish failure of signature
;; verification from that of decryption.

(defun mew-smime-decrypt-or-verify (syntax cnt ctl cte)
  ;; called in narrowed region
  ;;
  ;;     CT: Application/X-Pkcs7-Mime
  ;;
  (if (mew-smime-verify-configuration-check)
      syntax
    (mew-decode-mime-body nil (mew-syntax-get-value ctl 'cap) cte)
    (let ((encrypted-file (mew-make-temp-name))
	  (decrypted-file (mew-make-temp-name))
	  process syntax3 proto buf
	  result privacy (pubkey-file (mew-make-temp-name)))
      ;; XXX WHY DON'T YOU USE mew-save-decode-form!!!
      (mew-flet
       (write-region (mew-syntax-get-begin syntax)
		     (point-max)
		     encrypted-file nil 'no-msg))
      ;; signature verification
      (message "S/MIME verifying...")
      (if (equal 0 (mew-call-process-lang
		    "openssl" nil nil nil
		    "smime" "-verify" "-inform" "DER"
		    "-in" encrypted-file
		    "-CAfile" mew-smime-CA-file
		    "-signer" pubkey-file "-out" decrypted-file))
	  (setq result (concat "valid S/MIME digital signature by " 
			       (mew-smime-move-pubkey-and-extract-email pubkey-file)))
	;; decryption
	(message "S/MIME decrypting...")
	(setq mew-smime-running 'decrypt)
	(setq mew-smime-string nil)
	(setq buf (generate-new-buffer (concat mew-buffer-prefix "smime")))
	(setq process (mew-start-process-lang
		       "S/MIME decrypt" buf "openssl" "smime" "-decrypt"
		       "-inform" "DER" "-in" encrypted-file
		       "-recip" mew-smime-digital-id 
		       "-out" decrypted-file))
	(mew-set-process-cs process mew-cs-autoconv mew-cs-dummy)
	(set-process-filter process 'mew-smime-process-filter1)
	(set-process-sentinel process 'mew-smime-process-sentinel)
	(mew-rendezvous mew-smime-running)
	(message "S/MIME decrypting...done")
	(if (null mew-smime-sign-error)
	    (setq result "S/MIME decrypted")
	  (mew-xinfo-set-not-decrypted t)))
      (when result
	(delete-region (point-min) (point-max))
	(mew-flet 
	 (insert-file-contents decrypted-file)
	 (put-text-property (point-min) (point-max) 'mew-noncontents nil)
	 ;; because of RICH functionality of RFC1847... Gee dirty!
	 (mew-decode-crlf-magic)))
      (mew-delete-file encrypted-file)
      (mew-delete-file decrypted-file)
      ;; Analyze the decrypted part
      (if (not result) syntax
	(goto-char (point-min))
	(setq syntax3 (mew-decode-singlepart cnt nil nil))
	(setq privacy (mew-syntax-get-privacy syntax3))
	(if privacy (setq result (concat result "\n\t")))
	(mew-syntax-set-privacy
	 syntax3 (cons (list mew-ct-mle proto result) privacy))
	syntax3))))
     
(defun mew-smime-encrypt (file1 decrypters)
  (if (mew-smime-verify-configuration-check)
      (list nil (mew-smime-verify-configuration-check))
    (message "S/MIME encrypting...")
    ;;  (print decrypters)
    (let ((pubkey-list (mapcar 'mew-smime-email-address-to-filename decrypters))
	  (encrypt-file (mew-make-temp-name)))
      (print pubkey-list)
      (print encrypt-file)
      (if (eval (cons 'and (mapcar 'file-readable-p pubkey-list)))
	  (if (equal 0 (eval (append '(mew-call-process-lang
				       "openssl" nil nil nil
				       "smime"
				       "-encrypt" mew-smime-encryption-algorithm
				       "-outform" "DER"
				       "-in" file1 "-out" encrypt-file) pubkey-list)))
	      (list encrypt-file nil)
	    (list encrypt-file "openssl exited abnormally."))
	;; XXX Is this English correct??
	(list nil "Not all public keys of reciepients are available.")))))

(defun mew-smime-sign-message ()
  "Sign the entire draft with S/MIME. Input your passphrase."
  (interactive)
  (mew-draft-make-message 'smime-signature))

(defun mew-smime-encrypt-message ()
  "Encrypt the entire draft with S/MIME."
  (interactive)
  (mew-draft-make-message 'smime-encryption))

(defun mew-smime-sign-encrypt-message (&optional arg)
  "Sign then encrypt the entire draft with S/MIME. Input your passphrase."
  (interactive "P")
  (mew-draft-make-message 'smime-signature-encryption))

(defun mew-smime-encrypt-sign-message (&optional arg)
  "Encrypt then sign the entire draft with S/MIME. Input your passphrase."
  (interactive "P")
  (mew-draft-make-message 'smime-encryption-signature))

(provide 'mew-smime)

;;; Copyright Notice:

;; Copyright (C) 2001-2003 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-smime.el ends here
