;;;; COPY FROM HERE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight header
;;;

(mew-face-spec-set
 'mew-face-header-subject
 '((((class color) (type tty)) (:foreground "red" :bold t))
   (((class color) (background light)) (:foreground "Firebrick" :bold t))
   (((class color) (background dark))  (:foreground 
					;"OrangeRed" 
					 "sienna"
					:bold nil))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-from
 '((((class color) (type tty)) (:foreground "yellow" :bold t))
   (((class color) (background light)) (:foreground "DarkOrange4" :bold t))
   (((class color) (background dark))  (:foreground 
					; "Gold" 
					"indian red"
					:bold nil))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-date
 '((((class color) (type tty)) (:foreground "green" :bold t))
   (((class color) (background light)) (:foreground "ForestGreen" :bold t))
   (((class color) (background dark))  (:foreground 
					; "LimeGreen" 
					"dark goldenrod"
					:bold nil))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-to
 '((((class color) (type tty)) (:foreground "magenta" :bold t))
   (((class color) (background light)) (:foreground "DarkViolet" :bold t))
   (((class color) (background dark))  (:foreground "violet" :bold nil))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-key
 '((((class color) (type tty)) (:foreground "green" :bold t))
   (((class color) (background light)) (:foreground "ForestGreen" :bold t))
   (((class color) (background dark))  (:foreground "LimeGreen" :bold nil))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-private
 '((((class color) (type tty)) (:bold t))
   (((class color) (background light)) (:bold t))
   (((class color) (background dark))  (:bold t))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-important
 '((((class color) (type tty)) (:foreground "cyan" :bold t))
   (((class color) (background light)) (:foreground "MediumBlue" :bold t))
   (((class color) (background dark))  (:foreground "SkyBlue" :bold t))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-marginal
 '((((class color) (type tty)) (:bold t))
   (((class color) (background light)) (:foreground "gray50" :bold t))
   (((class color) (background dark))  (:foreground "gray50" :bold t))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-xmew
 '((((class color) (type tty)) (:foreground "yellow" :bold t))
   (((class color) (background light)) (:foreground "chocolate" :bold t))
   (((class color) (background dark))  (:foreground "chocolate" :bold t))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-header-xmew-bad
 '((((class color) (type tty)) (:foreground "red" :bold t))
   (((class color) (background light)) (:foreground "red" :bold t))
   (((class color) (background dark))  (:foreground "red" :bold t))
   (t (:bold t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight body
;;;

(mew-face-spec-set
 'mew-face-body-url
 '((((class color) (type tty)) (:foreground "red" :bold t))
   (((class color) (background light)) (:foreground "Firebrick" :bold t))
   (((class color) (background dark))  (:foreground "OrangeRed" :bold nil))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-body-comment
 '((((class color) (type tty)) (:foreground "blue"))
   (((class color) (background light)) (:foreground "gray50"))
   (((class color) (background dark))  (:foreground "gray50"))
   (t nil)))
(mew-face-spec-set
 'mew-face-body-cite1
 '((((class color) (type tty)) (:foreground "green"))
   (((class color) (background light)) (:foreground "ForestGreen"))
   (((class color) (background dark))  (:foreground
					; "LimeGreen"
					"SlateGray4"
					))
   (t nil)))
(mew-face-spec-set
 'mew-face-body-cite2
 '((((class color) (type tty)) (:foreground "cyan"))
   (((class color) (background light)) (:foreground "MediumBlue"))
   (((class color) (background dark))  (:foreground "SkyBlue"))
   (t nil)))
(mew-face-spec-set
 'mew-face-body-cite3
 '((((class color) (type tty)) (:foreground "magenta"))
   (((class color) (background light)) (:foreground "DarkViolet"))
   (((class color) (background dark))  (:foreground "violet"))
   (t nil)))
(mew-face-spec-set
 'mew-face-body-cite4
 '((((class color) (type tty)) (:foreground "yellow"))
   (((class color) (background light)) (:foreground "DarkOrange4"))
   (((class color) (background dark))  (:foreground "Gold"))
   (t nil)))
(mew-face-spec-set
 'mew-face-body-cite5
 '((((class color) (type tty)) (:foreground "red"))
   (((class color) (background light)) (:foreground "Firebrick"))
   (((class color) (background dark))  (:foreground "OrangeRed"))
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight mark
;;;

(mew-face-spec-set
 'mew-face-mark-review
 '((((class color) (type tty)) (:foreground "cyan"))
   (((class color) (background light)) (:foreground "MediumBlue"))
   (((class color) (background dark))  (:foreground "SkyBlue"))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-mark-multi
 '((((class color) (type tty)) (:foreground "magenta"))
   (((class color) (background light)) (:foreground "DarkViolet"))
   (((class color) (background dark))  (:foreground "violet"))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-mark-delete
 '((((class color) (type tty)) (:foreground "red"))
   (((class color) (background light)) (:foreground "Firebrick"))
   (((class color) (background dark))  (:foreground "OrangeRed"))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-mark-unlink
 '((((class color) (type tty)) (:foreground "yellow"))
   (((class color) (background light)) (:foreground "DarkOrange4"))
   (((class color) (background dark))  (:foreground "Gold"))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-mark-refile
 '((((class color) (type tty)) (:foreground "green"))
   (((class color) (background light)) (:foreground "ForestGreen"))
   (((class color) (background dark))  (:foreground "LimeGreen"))
   (t (:bold t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight eof
;;;

(mew-face-spec-set
 'mew-face-eof-message
 '((((class color) (type tty)) (:foreground "green" :bold t))
   (((class color) (background light)) (:foreground "ForestGreen" :bold t))
   (((class color) (background dark))  (:foreground "LimeGreen" :bold t))
   (t (:bold t))))
(mew-face-spec-set
 'mew-face-eof-part
 '((((class color) (type tty)) (:foreground "yellow" :bold t))
   (((class color) (background light)) (:foreground "DarkOrange4" :bold t))
   (((class color) (background dark))  (:foreground "Gold" :bold t))
   (t (:bold t))))

;;;; COPY TO HERE
