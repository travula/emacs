;;; mew-vars2.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb  1, 1999

;;; Code:

(require 'mew-vars)

;;; (1) Variables closer to constant.
;;; (2) Variables for soft-coding.
;;; (3) Difficult options with some macros.
;;; Use mew-add-first, mew-insert-after, mew-replace-with, 
;;; and mew-remove-entry to modify the variables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marks
;;;

(defvar mew-mark-multi  ?@)
(defvar mew-mark-review ?*)
(defvar mew-mark-delete ?D)
(defvar mew-mark-unlink ?X)
(defvar mew-mark-refile ?o)
(defvar mew-mark-tmp    ?\0) ;; temporary use only.
(defvar mew-mark-clean
  `(,mew-mark-delete ,mew-mark-unlink ,mew-mark-refile))
(defvar mew-mark-all
  `(,mew-mark-multi ,mew-mark-review ,mew-mark-delete ,mew-mark-unlink ,mew-mark-refile))
(defvar mew-mark-default-walk mew-mark-review)
(defvar mew-mark-walk         mew-mark-default-walk)
(defvar mew-mark-duplicated   mew-mark-delete)
(defvar mew-mark-show-list (list mew-mark-review))

(defvar mew-summary-mark-undo-marks
  `(,mew-mark-delete ,mew-mark-unlink ,mew-mark-refile)
  "A list of marks to be canceled by \\<mew-summary-mode-map>\\[mew-summary-mark
-undo-all].")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Field Magic
;;;

(defvar mew-reply-string "Re: ")
(defvar mew-reply-regex
  "Re\\(\\|[*^]?[0-9]+\\|\\[[0-9]+\\]\\|([0-9]+)\\)[:>] *"
  "Regexp of various Re: expression in Subject:")
(defvar mew-forward-string "Fw: ")
(defvar mew-forward-regex "\\(Fw\\|Fwd\\|Forward\\): *"
  "Regexp of various Fw: expression in Subject:")
(defvar mew-was-regex " *[[(] *\\(was[^a-z]\\|Re:\\).*[])] *$"
  "Regexp of various (was ...) expression in Subject:")

(defvar mew-subject-simplify-replace-alist
  (list
   ;; (REGEXP . NEW) --- NEW may be nil for deletion (same to "")
   ;; replace multiple Re: and Fw: into single Re:
   (cons (concat "^" ;; regexp
		 mew-reply-regex
		 "\\("
		 mew-reply-regex
		 "\\|"
		 mew-forward-regex
		 "\\)*")
	 'mew-reply-string) ;; new text
   ;; replace multiple Fw: and Re: into single Fw:
   (cons (concat "^" ;; regexp
		 mew-forward-regex
		 "\\("
		 mew-reply-regex
		 "\\|"
		 mew-forward-regex
		 "\\)*")
	 'mew-forward-string) ;; new text
   ;; delete extra string (no new text means delete)
   (cons mew-was-regex nil)) ;; regexp
  "*Replacement alist to simplify Subject: field body
Each element is cons cell whos car is REGEXP to replace,
cdr is new text. ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error
;;;

(defvar mew-error-unknown-charset    "**UNKNOWN CHARSET**")
(defvar mew-error-broken-string      "**BROKEN STRING**")
(defvar mew-error-illegal-b-encoding " **B ENCODING ERROR** ")
(defvar mew-error-illegal-q-encoding " **Q ENCODING ERROR** ")
(defvar mew-error-no-subject         "** no subject **")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Files
;;;

;; In each folder

(defvar mew-summary-cache-file ".mew-cache"
  "Cache file for Summary mode contents.")

(defvar mew-summary-touch-file ".mew-touch"
  "Time-stamp file for message folders.")

;; Under mew-conf-path

(defcustom mew-addrbook-file "Addrbook"
  "*A file which contains AddrBook information."
  :group 'mew-addrbook
  :type 'string)

(defvar mew-alias-auto-file ".mew-alias")

(defvar mew-refile-msgid-file ".mew-refile-msgid-alist")

(defvar mew-refile-from-file ".mew-refile-from-alist")

(defvar mew-smtp-log-file "Smtplog")
(defvar mew-nntp-log-file "Nntplog")

;; xxx Home

(defvar mew-fib-item-file "~/.mew-fib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modes
;;;

(defvar mew-folder-mode 448 ;; decimal for octal 0700
  "Secure file mode for folders. 448(0700 in octal) is STRONGLY
recommended for privacy reasons.")

(defvar mew-file-mode-mask 432 ;; decimal for octal 0660
  "Secure file mode mask. 432(0660 in octal) is STRONGLY recommended
for privacy reasons.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separators
;;;

(defvar mew-path-separator "/")
(defvar mew-regex-drive-letter (file-name-as-directory "^[a-zA-Z]:"))
(defvar mew-regex-file-absolute (format "^[~%s]" mew-path-separator))
(defvar mew-header-separator "----")
(defvar mew-eoh nil) ;; set by mew-regex-setup
(defvar mew-lwsp "^[ \t]")
(defvar mew-lwsp+ "^[ \t]+")
(defvar mew-address-separator ":, \t\n")
(defvar mew-page-delimiter "^\^L")
(defvar mew-keyval "^\\([^ \t:]+:?\\)[ \t]*")
;; "^\\([^ \t:]+:?\\)[ \t]*\\(.*\\)$" faces a stupid error. 
;; "$" does not mean end-of-line if the second match is empty, sigh.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Explanation string in messages
;;;

(defvar mew-bcc-subject "A blind carbon copy")
(defvar mew-bcc-body "This is a blind carbon copy.\n")

(defvar mew-field-comment "(modified by Mew)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook
;;;

(defvar mew-addrbook-switch
  '((shortname . mew-addrbook-shortname-get)
    (address   . identity)
    (username  . mew-addrstr-extract-user)
    (nickname  . mew-addrbook-nickname-get)
    (name      . mew-addrbook-name-get))
  "Function database to get each field of Addrbook.
'shortname, 'address, 'username, 'nickname, and 'name is defined.")

(defsubst mew-addrbook-func (key)
  (cdr (assq key mew-addrbook-switch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME types
;;;

(defsubst mew-ct-textp (ct)
  (string-match "^Text/" (capitalize ct)))

(defsubst mew-ct-imagep (ct)
  (string-match "^Image/" (capitalize ct)))

(defsubst mew-ct-modelp (ct)
  (string-match "^Model/" (capitalize ct)))

(defsubst mew-ct-multipartp (ct)
  (string-match "^Multipart/" (capitalize ct)))

(defsubst mew-ct-messagep (ct)
  (string-match "^Message/" (capitalize ct)))

(defsubst mew-ct-linebasep (ct)
  (or (mew-ct-textp ct)
      (mew-case-equal ct mew-ct-aps)
      (mew-case-equal ct mew-ct-msg))) ;; xxx mew-ct-messagep?

(defvar mew-mime-content-type-multipart-list
  `(,mew-ct-mlm ,mew-ct-mla)
  "Candidate of 'Content-Type: Multipart/' when CT: is changed
in draft buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;

(defconst mew-custom-type-of-field-completion
  '(alist key-type: string :value-type function))

(defcustom mew-field-completion-switch
  '(("To:"          . mew-complete-address)
    ("Cc:"          . mew-complete-address)
    ("Dcc:"         . mew-complete-address)
    ("Bcc:"         . mew-complete-address)
    ("Reply-To:"    . mew-complete-address)
    ("Fcc:"         . mew-complete-local-folder)
    ("Resent-To:"   . mew-complete-address)
    ("Resent-Cc:"   . mew-complete-address)
    ("Resent-Dcc:"  . mew-complete-address)
    ("Resent-Bcc:"  . mew-complete-address)
    ("Newsgroups:"  . mew-complete-newsgroups)
    ("Followup-To:" . mew-complete-newsgroups))
  "*Completion function alist concerned with the key."
  :group 'mew-complete
  :type mew-custom-type-of-field-completion)

(defcustom mew-field-circular-completion-switch
  '(("To:"          . mew-circular-complete-domain)
    ("Cc:"          . mew-circular-complete-domain)
    ("Dcc:"         . mew-circular-complete-domain)
    ("Bcc:"         . mew-circular-complete-domain)
    ("Reply-To:"    . mew-circular-complete-domain)
    ("Resent-To:"   . mew-circular-complete-domain)
    ("Resent-Cc:"   . mew-circular-complete-domain)
    ("Resent-Dcc:"  . mew-circular-complete-domain)
    ("Resent-Bcc:"  . mew-circular-complete-domain)
    ("From:"        . mew-circular-complete-from)
    ("Resent-From:" . mew-circular-complete-from))
  "*Circular completion function alist concerned with the key."
  :group 'mew-complete
  :type mew-custom-type-of-field-completion)

(defcustom mew-field-expansion-switch
  '(("To:"         . mew-expand-address)
    ("Cc:"         . mew-expand-address)
    ("Dcc:"        . mew-expand-address)
    ("Bcc:"        . mew-expand-address)
    ("Reply-To:"   . mew-expand-address)
    ("Resent-To:"  . mew-expand-address)
    ("Resent-Cc:"  . mew-expand-address)
    ("Resent-Dcc:" . mew-expand-address)
    ("Resent-Bcc:" . mew-expand-address))
  "*Expansion function alist concerned with the key."
  :group 'mew-complete
  :type mew-custom-type-of-field-completion)

(defsubst mew-field-get-func (key switch)
  (cdr (mew-assoc-match key switch 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME control
;;;

(defvar mew-content-type mew-ct-txt
  "*The default Content-Type: for a file whose suffix is unknown.")

(defvar mew-mime-content-type
  '(("multipart/"  nil         nil     nil            mew-icon-multipart)
    ;;
    ("audio/basic"  "\\.au$"    mew-b64 mew-prog-audio mew-icon-audio)
    ("audio/x-wav"  "\\.wav$"   mew-b64 mew-prog-audio mew-icon-audio)
    ("audio/x-aiff" "\\.aif?f$" mew-b64 mew-prog-audio mew-icon-audio)
    ("audio/x-midi" "\\.midi?$" mew-b64 mew-prog-audio mew-icon-audio)
    ("audio/x-mpeg" "\\.mpga$\\|\\.mp[23]$" mew-b64 mew-prog-audio mew-icon-audio)
    ;;
    ("image/gif"   "\\.gif$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/tiff"  "\\.tif?f$" mew-b64 mew-prog-image mew-icon-image)
    ("image/jpeg"  "\\.jpe?g$" mew-b64 mew-prog-image mew-icon-image)
    ("image/png"   "\\.png$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/x-xwd" "\\.xwd$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/x-xbm" "\\.xbm$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/x-xpm" "\\.xpm$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/x-bmp" "\\.bmp$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/x-pcx" "\\.pcx$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/x-tga" "\\.tga$"   mew-b64 mew-prog-image mew-icon-image)
    ("image/"      "^$"        mew-b64 mew-prog-image mew-icon-image)
    ;;
    ("model/iges" "\\.ige?s$" mew-b64 mew-prog-iges  mew-icon-image) ;; xxx
    ("model/vrml" "\\.wrl$"   mew-b64 mew-prog-vrml  mew-icon-image)
    ("model/mesh" "\\.me?sh$" mew-b64 mew-prog-mesh  mew-icon-image)
    ("model/"      "^$"       mew-b64 mew-prog-model mew-icon-image)
    ;;
    ("video/mpeg"      "\\.mpe?g$" mew-b64 mew-prog-video mew-icon-video)
    ("video/quicktime" "\\.mov$"   mew-b64 mew-prog-video mew-icon-video)
    ("video/x-msvideo" "\\.avi$"   mew-b64 mew-prog-video mew-icon-video)
    ;;
    ("message/rfc822"          "^[0-9]+$" nil
     mew-prog-rfc822           mew-icon-message/rfc822)
    ("message/external-body"   "\\.ext$"  nil
     mew-prog-external-body    mew-icon-message/external-body)
    ("message/delivery-status" "^$"       nil
     mew-prog-delivery-status  mew-icon-text)
    ;;
    ("application/postscript"        "\\.e?ps$" mew-qp
     mew-prog-postscript             mew-icon-application/postscript)
    ("application/pdf"               "\\.pdf$"  mew-b64
     mew-prog-pdf                    mew-icon-application/postscript)
    ("application/xml"               "\\.xml$"  mew-b64
     mew-prog-xml2                   mew-icon-text)
    ("application/msword"            "\\.doc$"  mew-b64
     mew-prog-msword                 mew-icon-text)
    ("application/vnd.ms-excel"      "\\.xls$"  mew-b64
     mew-prog-msexcel                mew-icon-text)
    ("application/vnd.ms-powerpoint" "\\.ppt$"  mew-b64
     mew-prog-mspowerpoint           mew-icon-text)
    ("application/pgp-keys"          "\\.pka$"  nil
     mew-prog-pgp-keys               mew-icon-unknown)
    ("application/x-pkcs7-signature" "\\.p7s$"  mew-b64
     nil			     mew-icon-unknown)
    ("application/vnd.fujitsu.oasys" "\\.oas$"  mew-b64
     mew-prog-oasys                  mew-icon-text)
    ("application/vnd.fujitsu.oasys2" "\\.oa2$" mew-b64
     mew-prog-oasys                  mew-icon-text)
    ("application/vnd.fujitsu.oasys3" "\\.oa3$" mew-b64
     mew-prog-oasys                  mew-icon-text)
    ("application/octet-stream"
     "\\.tar$\\|\\.tar\\.\\|\\.gz$\\|\\.Z$\\|\\.taz$\\|\\.tgz$\\|\\.bz2?$\\|\\.lzh$\\|\\.zip$\\|\\.bin$\\|\\.pgp$\\|\\.gpg$\\|\\.exe$\\|\\.dll$"
      mew-b64 mew-prog-octet-stream mew-icon-application/octet-stream)
    ;;
    ("text/html"     "\\.html?$" nil     mew-prog-html      mew-icon-text)
    ("text/enriched" "\\.rtf$"   nil     mew-prog-enriched  mew-icon-text)
    ("text/css"      "\\.css$"   nil     mew-prog-text      mew-icon-text)
    ("text/sgml"     "\\.sgml$"  nil     mew-prog-text      mew-icon-text)
    ("text/plain"    "\\.txt$\\|\\.c$\\|\\.h$\\|\\.el$\\|\\.diff$\\|\\.patch$"
                                 nil     mew-prog-plain     mew-icon-text)
    ("text/xml"      "\\.xml$"   nil     mew-prog-xml       mew-icon-text)
    ("text/rfc822-headers" "\\.hdr$" nil
     mew-prog-rfc822-headers     mew-icon-message/rfc822)
    ("text/"         "^$"        nil     mew-prog-text      mew-icon-text)
    ;; Unkown CT: matches here.
    (t               "^$"       nil   mew-prog-octet-stream mew-icon-unknown)
    ;; Unknown suffix matches here and return the entry specified 
    ;; by mew-content-type.
    (nil             ".*"))
  "(content-type filename <encoding> <prog> <icon>)")

(defsubst mew-ctdb-by-ct (ct)
  (mew-assoc-match2 ct mew-mime-content-type 0))

(defsubst mew-ctdb-by-file (file)
  (mew-assoc-match2 file mew-mime-content-type 1))

(defsubst mew-ctdb-ct (attr)
  (and (nth 0 attr) (mew-capitalize (nth 0 attr))))

(defsubst mew-ctdb-regex (attr)
  (nth 1 attr))

(defsubst mew-ctdb-cte (attr)
  (symbol-value (nth 2 attr)))

(defsubst mew-ctdb-prog (attr)
  (let ((val (symbol-value (nth 3 attr))))
    (if (and (listp val) (eq 'if (car val)))
	(eval val)
      val)))

(defsubst mew-ctdb-icon (attr)
  (symbol-value (nth 4 attr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME image control
;;;

(defvar mew-mime-image-alist
  '(("Image/Tiff" . tiff) ("Image/Gif" . gif) 
    ("Image/Jpeg" . jpeg) ("Image/Png" . png)
    ("Image/X-Xwd" . xwd) ("Image/X-Xbm" . xbm)
    ("Image/X-Xpm" . xpm) ("Image/X-Bmp" . bmp)))

(defsubst mew-mime-image-format-name (ct)
  (cdr (assoc ct mew-mime-image-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MIME Content-Disposition:
;;;

(defvar mew-mime-content-disposition
  '(("text/"      "inline"     t)
    ("image/"     "inline"     t)
    ("message/"   "inline"     nil)
    ("multipart/" nil          nil)
    (t            "attachment" t))
  "(content-type inline/attachment filename)")

(defsubst mew-cdpdb-by-ct (ct)
  (mew-assoc-match2 ct mew-mime-content-disposition 0))

(defsubst mew-cdpdb-val (attr)
  (nth 1 attr))

(defsubst mew-cdpdb-file (attr)
  (nth 2 attr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Privacy
;;;

(defcustom mew-privacy-database
  `((smime-signature ((,mew-ct-mls ,mew-ct-sms)) "SS")
    (smime-encryption ((,mew-ct-mle ,"application/x-pkcs7-mime")) "SE")
    (smime-signature-encryption
     ((,mew-ct-mls ,mew-ct-sms) (,mew-ct-mle ,"application/x-pkcs7-mime")) "SSSE")
    (smime-encryption-signature
     ((,mew-ct-mle ,"application/x-pkcs7-mime") (,mew-ct-mls ,mew-ct-sms)) "SESS")
    (pgp-signature  ((,mew-ct-mls ,mew-ct-pgs)) "PS")
    (pgp-encryption ((,mew-ct-mle ,mew-ct-pge)) "PE")
    (pgp-signature-encryption
     ((,mew-ct-mls ,mew-ct-pgs) (,mew-ct-mle ,mew-ct-pge)) "PSPE")
    (pgp-encryption-signature
     ((,mew-ct-mle ,mew-ct-pge) (,mew-ct-mls ,mew-ct-pgs)) "PEPS"))
  "*Alist of key, a list of privacy Content-Type, and its mark."
  :group 'mew-privacy
  :type 'sexp)

(defsubst mew-pcdb-services ()
  (mapcar 'car mew-privacy-database))

(defsubst mew-pcdb-by-service (service)
  (assq service mew-privacy-database))

(defsubst mew-pcdb-ct (pcdb)
  (nth 1 pcdb))

(defsubst mew-pcdb-mark (pcdb)
  (nth 2 pcdb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Multipart/Alternative
;;;

(defvar mew-mime-multipart-alternative-list
  `(,mew-ct-txt ".*"))

(defsubst mew-multipart-alternative-preference (part)
  (let ((ct (mew-syntax-get-value (mew-syntax-get-ct part) 'cap)))
    (mew-member-match ct mew-mime-multipart-alternative-list)))

(defvar mew-mime-external-body-list
  '("anon-ftp" "url" "mail-server"))

(defsubst mew-mime-external-body-preference (part)
  (let* ((ctl (mew-syntax-get-ct part))
	 (ct (mew-syntax-get-value ctl 'cap))
	 (access-type (mew-syntax-get-param ctl "access-type")))
    (if (and (string= ct mew-ct-ext) (stringp access-type))
	(mew-member-match access-type mew-mime-external-body-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Field database
;;;

;; strictly, content-* are necessary but they are not practical.
(defvar mew-field-database
  `((,mew-to:            mailbox    struct)
    (,mew-cc:            mailbox    struct)
    (,mew-from:          mailbox    struct)
    (,mew-apparently-to: mailbox    struct)
    (,mew-bcc:           mailbox    struct)
    (,mew-dcc:           mailbox    struct)
    (,mew-reply-to:      mailbox    struct)
    (,mew-resent-to:     mailbox    struct)
    (,mew-resent-cc:     mailbox    struct)
    (,mew-resent-from:   mailbox    struct)
    (,mew-mv:            mime       struct)
    (,mew-subj:          text       text)
    (,mew-keywords:      comma-text text)
    (,mew-received:      unstruct   unstruct)
    (,mew-message-id:    unstruct   unstruct)
    (,mew-references:    unstruct   unstruct)
    (,mew-in-reply-to:   unstruct   unstruct)
    (,mew-x-face:	 unstruct   unstruct)
    (,mew-face:          unstruct   unstruct))
  "(field enc dec)")

(defsubst mew-field-type-for-encoding (key)
  (or (nth 1 (assoc (mew-capitalize key) mew-field-database)) 'unstruct))

(defsubst mew-field-type-for-decoding (key)
  (or (nth 2 (assoc (mew-capitalize key) mew-field-database)) 'text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Theme
;;;

(defvar mew-theme-file "mew-theme")

(defface mew-face-header-subject nil
  "*Face to highlight the value of Subject:"
  :group 'mew-highlight)

(defface mew-face-header-from nil
  "*Face to highlight the value of From:"
  :group 'mew-highlight)

(defface mew-face-header-to nil
  "*Face to highlight the value of To:"
  :group 'mew-highlight)

(defface mew-face-header-date nil
  "*Face to highlight the value of Date:"
  :group 'mew-highlight)

(defface mew-face-header-key nil
  "*Face to highlight by default"
  :group 'mew-highlight)

(defface mew-face-header-private nil
  "*Face to highlight private field-keys"
  :group 'mew-highlight)

(defface mew-face-header-important nil
  "*Face to highlight important field-keys"
  :group 'mew-highlight)

(defface mew-face-header-marginal nil
  "*Face to highlight marginal field-values"
  :group 'mew-highlight)

(defface mew-face-header-xmew nil
  "*Face to highlight the value of X-Mew:"
  :group 'mew-highlight)

(defface mew-face-header-xmew-bad nil
  "*Face to highlight the value of X-Mew: in bad cases"
  :group 'mew-highlight)

(defface mew-face-body-url nil
  "*Face to highlight URL in Message/Draft mode"
  :group 'mew-highlight)
  
(defface mew-face-body-comment nil
  "*Face to highlight comments in a body"
  :group 'mew-highlight)

(defface mew-face-body-cite1 nil
  "*Face to highlight the first citation"
  :group 'mew-highlight)

(defface mew-face-body-cite2 nil
  "*Face to highlight the second citation"
  :group 'mew-highlight)

(defface mew-face-body-cite3 nil
  "*Face to highlight the third citation"
  :group 'mew-highlight)

(defface mew-face-body-cite4 nil
  "*Face to highlight the forth citation"
  :group 'mew-highlight)

(defface mew-face-body-cite5 nil
  "*Face to highlight the fifth citation"
  :group 'mew-highlight)

(defface mew-face-mark-review nil
  "*Face to highlight the review mark"
  :group 'mew-highlight)

(defface mew-face-mark-multi nil
  "*Face to highlight the multi mark"
  :group 'mew-highlight)

(defface mew-face-mark-delete nil
  "*Face to highlight the delete mark"
  :group 'mew-highlight)

(defface mew-face-mark-unlink nil
  "*Face to highlight the unlink mark"
  :group 'mew-highlight)

(defface mew-face-mark-refile nil
  "*Face to highlight the refile mark"
  :group 'mew-highlight)

(defface mew-face-eof-message nil
  "*Face to highlight the 'end of message' string"
  :group 'mew-highlight)

(defface mew-face-eof-part nil
  "*Face to highlight the 'end of part' string"
  :group 'mew-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight header
;;;

(defcustom mew-field-spec
  '(("^Resent-\\(From\\|To\\|Cc\\|Date\\)" t
     mew-face-header-important
     mew-face-header-important)
    ("^Subject:$" t
     mew-face-header-important
     mew-face-header-subject)
    ("^From:$" t
     mew-face-header-important
     mew-face-header-from)
    ("^\\(To\\|Apparently-To\\):$" t
     mew-face-header-important
     mew-face-header-to)
    ("^\\(Cc\\|Dcc\\|Bcc\\):$" t
     mew-face-header-important
     mew-face-header-to)
    ("^Newsgroups:$" t
     mew-face-header-important
     mew-face-header-to)
    ("^Date:$" t
     mew-face-header-important
     mew-face-header-date)
    ("^Reply-To:$" t)
    ("^X-Mailer:$" t)
    ("^X-Mew:$" t
     mew-face-header-important
     mew-face-header-xmew)
    ("^\\(Received\\|Return-Path\\|Sender\\|Errors-To\\):$" nil)
    ("^\\(Path\\|Distribution\\|Xref\\):$" nil)
    ("^NNTP-Posting-" nil)
    ("^\\(Message-Id\\|Posted\\|In-Reply-To\\|References\\|Precedence\\):$" nil)
    ("^Delivered-" nil)
    ("^List-" nil) ;; RFC 2369
;;    ("^Content-" t)
    ("^\\(Mime-Version\\|Lines\\):$" nil)
    ("^From$" nil)
    ("^Status:$" nil)
    ("^Face:$" nil
     mew-face-header-private
     mew-face-header-marginal)
    ("^X-" nil
     mew-face-header-private
     mew-face-header-marginal))
  "*An alist of field spec for Message mode. Each spec
consists of field-regular-expression, visible-p, face-for-key, 
and face-for-value. Fields whose visible-p is t are displayed in 
Message mode in the defined order. Fields whose visible-p is nil are
hidden in Message mode. Type DEL to see them. Fields not matched
to field-regular-expressions are operated by the value of
'mew-field-other-visible'. If face-for-key is omitted, 
'mew-face-header-key' is used. If face-for-value is not
present, mew-face-header-marginal is used."
  :group 'mew-highlight
  :type '(alist :key-type regexp
                :value-type
                  (choice (list boolean)
                          (list boolean face face))))

;; cons the position to the spec. 
(defsubst mew-nspec-by-key (key)
  (mew-assoc-match3 key mew-field-spec 0))

(defsubst mew-nspec-n (nspec)
  (nth 0 nspec))

(defsubst mew-nspec-visiblep (nspec)
  (nth 2 nspec))

(defsubst mew-nspec-keyface (nspec)
  (nth 3 nspec))

(defsubst mew-nspec-valface (nspec)
  (nth 4 nspec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight body
;;;

(defvar mew-highlight-body-regex-comment "^#+.*")

(defvar mew-highlight-body-regex-cite
  "^\\(\\([ \t]*\\([>:|]\\|\\w+\\(['._-]+\\w+\\)*>+\\)\\)+\\).*")

(defcustom mew-highlight-body-prefix-width 20
  "*Maximum string width assume prefix for fancy highlight a body."
  :group 'mew-highlight
  :type 'integer)

(defvar mew-highlight-body-cite-faces
  '(mew-face-body-cite1
    mew-face-body-cite2
    mew-face-body-cite3
    mew-face-body-cite4
    mew-face-body-cite5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight mark
;;;

(defvar mew-highlight-mark-keywords
  `((,mew-mark-review . mew-face-mark-review)
    (,mew-mark-multi  . mew-face-mark-multi)
    (,mew-mark-delete . mew-face-mark-delete)
    (,mew-mark-unlink . mew-face-mark-unlink)
    (,mew-mark-refile . mew-face-mark-refile))
  "A list of mark-face pair to used in Summary/Virtual mode.")

(defsubst mew-highlight-mark-get-face (mark)
  (cdr (assoc mark mew-highlight-mark-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mark spec
;;;

(defvar mew-mark-afterstep-spec
  `((,mew-mark-review 0 0 0 0 0 0 0)
    (,mew-mark-multi  0 0 0 0 0 0 0)
    (,mew-mark-delete 2 0 2 0 0 0 0)
    (,mew-mark-unlink 2 0 2 0 0 0 0)
    (,mew-mark-refile 2 0 2 0 0 0 0))
  "*A list of cursor action spec.
The cursor action spec is a list of a mark and seven cursor
actions after marking.

0th (the first element of a list) is a mark value. A mark value is
ASCII code of the mark. For example, the value of the '*' mark
(mew-mark-review) is 42.

Seven values following a mark value means as follows:

1st is the case of no mark.
2nd is the case where the new mark is equal to the old one.
3rd is the case where level of the new mark is greater than that of the 
    old one.
4th, 5th, and 6th is the case where levels are equal.
    4th and 5th is the case that the old mark has state.
        4th means that the old mark is overrode.
        5th means that the old mark remains.
    6th is the case that the old mark does not have state.
7th is the case where level of the new mark is smaller than that of the 
    old one.

The value of cursor actions means as follows:

	0 means staying.
	1 means moving according to the direction,
	2 means moving according to the direction 
	  then displaying the next message.

For more detail, see mew-mark-put-mark and mew-mark-afterstep.")

(defsubst mew-markas-nth (mark case)
  (nth case (assoc mark mew-mark-afterstep-spec)))

(defvar mew-mark-spec
  `((,mew-mark-review "review" 0 nil nil nil nil nil )
    (,mew-mark-multi  "multi"  0 nil nil nil nil nil)
    (,mew-mark-delete "delete" 1 nil t   nil mew-mark-exec-delete nil)
    (,mew-mark-unlink "unlink" 1 nil t   nil mew-mark-exec-unlink nil)
    (,mew-mark-refile "refile" 1 t   mew-mark-kill-refile mew-mark-unrefile
		                     mew-mark-exec-refile mew-mark-sanity-refile))
  "*A list of lists which consists of
mark, name, level, statefullp, kill-line-p, 
undo-func, exec-func, and sanity-fucn.")

;;

(defun mew-mark-get-all-marks ()
  "Collecting all defined marks."
  (mapcar 'car mew-mark-spec))

(defsubst mew-markdb-by-mark (mark)
  (assoc mark mew-mark-spec))

(defsubst mew-markdb-name (mark)
  (nth 1 (mew-markdb-by-mark mark)))

(defsubst mew-markdb-level (mark)
  (nth 2 (mew-markdb-by-mark mark)))

(defsubst mew-markdb-statefullp (mark)
  (nth 3 (mew-markdb-by-mark mark)))

(defsubst mew-markdb-killp (mark)
  (nth 4 (mew-markdb-by-mark mark)))

(defsubst mew-markdb-func-undo (mark)
  (nth 5 (mew-markdb-by-mark mark)))

(defsubst mew-markdb-func-exec (mark)
  (nth 6 (mew-markdb-by-mark mark)))

(defsubst mew-markdb-func-sanity (mark)
  (nth 7 (mew-markdb-by-mark mark)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; External-body
;;;

(defvar mew-ext-url-alist
  '(("^application/" "Fetch by w3" mew-ext-url-fetch-by-w3 nil)
    (t		     "Browse by mozilla" "mozilla" ("-install")))
  "*Alist of (REGEXP DOC PROGRAM ARGS-LIST) to define
an appropriate method for a content-type in external-body. Note
this phantom body is defined RFC 2017.

If REGEXP is 't', every content-type is matched to the list.
If PROGRAM is a string, it is considered an external program.
If PROGRAM is a symbol, the lisp function whose name is PROGRAM is called.

'mew-ext-url-show-by-w3' and 'mew-ext-url-fetch-by-w3' are pre-defined
as lisp function.

If you want to use \"w3.el\" instead of \"mozilla\", put the 
following in .emacs.
	(setq mew-ext-url-alist
              '((t (\"Browse by w3\" mew-ext-url-show-by-w3 nil))))
If you want to use \"lynx\" instead of \"mozilla\", put the 
following in .emacs.
        (setq mew-ext-url-alist
              '((t (\"Browse by lynx\" \"kterm\" (\"-e\" \"lynx\" \"-color\")))))
")

(defsubst mew-ext-url-by-ct (ct)
  (mew-assoc-match2 ct mew-ext-url-alist 0))

(defsubst mew-ext-url-get-doc (prog-list)
  (nth 1 prog-list))

(defsubst mew-ext-url-get-prog (prog-list)
  (nth 2 prog-list))

(defsubst mew-ext-url-get-args (prog-list)
  (nth 3 prog-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Proto
;;;

(defvar mew-proto mew-folder-local)

(defvar mew-proto-spec
  ;; key go refile flush
  '(("+" "+" "+" "+")
    ("$" "+" nil "+")
    ("%" "%" "%" "+")
    ("*" nil nil nil)
    ("-" "-" nil "-")))

(defun mew-proto-to-body (fld n)
  (if (mew-folder-virtualp fld)
      (setq fld (mew-thread-to-folder fld)))
  (setq fld (mew-folder-folder fld))
  (let ((proto (mew-folder-prefix fld)))
    (unless (member proto mew-folder-prefixes)
      (setq proto (mew-proto))) ;; default proto
    (nth n (assoc proto mew-proto-spec))))

(defun mew-proto-to-go (fld)
  (mew-proto-to-body fld 1))

(defun mew-proto-to-refile (fld)
  (mew-proto-to-body fld 2))

(defun mew-proto-to-flush (fld)
  (mew-proto-to-body fld 3))

(defun mew-proto-inbox-folder (proto &optional case)
  (or proto (setq proto (mew-proto case)))
  (cond
   ((mew-folder-popp  proto) mew-pop-inbox-folder)
   ((mew-folder-imapp proto) mew-imap-inbox-folder)
   ((mew-folder-nntpp proto) (mew-nntp-newsgroup case))
   (t ;; local 
    (mew-inbox-folder case))))

(defun mew-proto-queue-folder (proto &optional case)
  (or proto (setq proto (mew-proto case)))
  (cond
   ((mew-folder-nntpp proto) (mew-postq-folder case))
   (t (mew-queue-folder case))))

(defun mew-proto-friend-folder (proto &optional case)
  (cond
   ((mew-folder-imapp proto) (mew-imap-friend-folder case))
   (t ;; local 
    mew-friend-folder)))

(defun mew-proto-friend-folder-list (proto &optional case)
  (cond
   ((mew-folder-imapp proto) (mew-imap-friend-folder-list case))
   (t ;; local 
    (mew-local-friend-folder-list))))

(defun mew-proto-folder-alist (proto &optional case)
  (cond
   ((mew-folder-popp  proto) (mew-pop-folder-alist))
   ((mew-folder-imapp proto) (mew-imap-folder-alist case))
   ((mew-folder-nntpp proto) (mew-nntp-folder-alist case))
   (t ;; local 
    (mew-local-folder-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Config
;;;

(defcustom mew-config-alist nil
  "*Alist of config. This is a list of 
	(<case> (<key> . <value>) (<key> . <value>) ...).
  - <case> is a string of case.
  - <key> is a string of Mew value with the \"mew-\" prefix removed.
  - <value> is a string.

Currently, the following keys are supported: 
\"name\", \"user\", \"mail-domain\", 
\"cc\", \"fcc\", \"dcc\", \"reply-to\",
\"organization\", \"header-alist\", 
\"smtp-server\", \"smtp-port\", \"smtp-ssh-server\", 
\"smtp-ssl\", \"smtp-ssl-port\", 
\"smtp-msgid-user\", \"smtp-msgid-domain\", 
\"smtp-helo-domain\", \"smtp-mail-from\", 
\"smtp-user\", \"smtp-auth\", \"smtp-auth-list\", 
\"pop-server\", \"pop-port\", \"pop-ssh-server\", 
\"pop-ssl\", \"pop-ssl-port\", 
\"pop-user\", \"pop-auth\", 
\"pop-size\", \"pop-body-lines\", \"pop-delete\", \"pop-folder\", 
\"imap-server\", \"imap-port\", \"imap-ssh-server\", 
\"imap-ssl\", \"imap-ssl-port\", 
\"imap-user\", \"imap-auth\", \"imap-size\", \"imap-folder\", 
\"nntp-server\", \"nntp-port\", \"nntp-ssh-server\", 
\"nntp-ssl\", \"nntp-ssl-port\", 
\"nntp-user\", \"nntp-size\", \"nntp-folder\",
\"nntp-msgid-user\", \"nntp-msgid-domain\", 
\"inbox-folder\", \"queue-folder\",
\"mailbox-type\", \"mbox-command\", \"mbox-command-arg\", 
\"signature-file\", \"content-type\" .

from = name <user@mail-domain>
message-id = *random*.smtp-msgid-user@smtp-msgid-domain
message-id = *random*.nntp-msgid-user@nntp-msgid-domain

An example is as follows:

(setq mew-config-alist
      '((\"mew\"
	 (\"mail-domain\"  . \"mew.org\")
	 (\"inbox-folder\" . \"+inbox-mew\"))
	(\"keio\"
	 (\"cc\"           . \"kazu@iijlab.net\")
	 (\"user\"         . \"pooh\")
	 (\"mail-domain\"  . \"sfc.keio.ac.jp\"))
	(\"default\"
	 (\"name\"         . \"Kazu Yamamoto\")
	 (\"mail-domain\"  . \"iijlab.net\"))))
"
  :group 'mew-env
  :type '(alist :key-type string
                :value-type (repeat (cons string string))))

(defsubst mew-cfent-by-case (case)
  (if (null case)
      (assoc mew-case-default mew-config-alist)
    (assoc case mew-config-alist)))

(provide 'mew-vars2)

;;; Copyright Notice:

;; Copyright (C) 2000-2003 Mew developing team.
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

;;; mew-vars2.el ends here
