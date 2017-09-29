(require 'faces)

(font-lock-mode 1)

(set-cursor-color "green")


(defun dark ()
  
;; Dark 
;; -----


;; BACKGROUNDS

 (set-face-background 'default "Black")
;; (set-face-background 'default "DarkSlateGray")
;; (set-face-background 'default "Grey")
;; (set-background-color "Black")
;; (set-face-background 'default "DarkSlateGray")
;; (set-face-background 'default "Grey")
;; (set-face-background 'default "gray4")
;; (set-face-background 'default "gray12")
;; (set-face-background 'default "DimGray")

;; FOREGROUNDS
;; (set-face-foreground 'default "White")
;; (set-face-foreground 'default "Tan")
  (set-face-foreground 'default "DimGray")
;;  (set-face-foreground 'default "Gray20")
;;  (set-face-foreground 'default "Gray25")
;;  (set-face-foreground 'default "Gray30")
;;  (set-face-foreground 'default "Gray40")
;;  (set-face-foreground 'default "Gray50")
;;  (set-face-foreground 'default "Gray60")
;;  (set-face-foreground 'default "Gray70")
;;  (set-face-foreground 'default "Gray80")
;;  (set-face-foreground 'default "Gray70")

;; MODELINE
;;; http://mitchfincher.blogspot.in/2014/11/emacs-lisp-error-error-invalid-face.html
   (set-face-background 'mode-line "Blue")

;; For some reason the background for the default
;; and modeline faces get messed up on the suns at work
;; [vc 3/21/99]
;; Wheat, Tan, White


;; this doesn't work, for some reason.  Set this interactively
   (set-face-background 'region "ivory")

   (set-face-foreground font-lock-comment-face "Brown")  ; Laptop
)


;;______________________________________________________________________
;; Light 
;; _____________________________________________________________________

(defun light ()
;; 
;; BACKGROUND
 (set-background-color "White")

;; DEFAULT-FACE Foreground
;; On Laptop
;; (set-face-foreground 'default "DimGray")
 (set-face-foreground 'default "DarkSlateGray")
;; (set-face-foreground 'default "Wheat")
;; (set-face-foreground 'default "Tan")
;; (set-face-foreground 'default "Thistle")
;; (set-face-foreground 'default "DarkOrchid")
;; (set-face-foreground 'default "NavajoWhite4")
;; (set-face-foreground 'default "Yellow")
;; (set-face-foreground 'default "khaki4")
;; (set-face-foreground 'default "burlywood4")
;; (set-face-foreground 'default "Grey")
;; (set-face-foreground 'default "Grey60")
;; (set-face-foreground 'default "White")
;; (set-face-foreground 'default "cornsilk3")
;; (set-face-foreground 'default "beige")
;; (set-face-foreground 'default "Sienna")
;; (set-face-foreground 'default "SkyBlue")

;; (set-face-foreground 'default "Black")

;; On Desktop
;; (set-face-foreground 'default  "CadetBlue") 


;; MODELINE
   (set-face-background 'mode-line "Grey")

;; For some reason the background for the default
;; and modeline faces get messed up on the suns at work
;; [vc 3/21/99]
;; Wheat, Tan, White

;; STRING-FACE

;; (set-face-foreground font-lock-string-face "MediumAquamarine")
;; (set-face-foreground font-lock-string-face "SteelBlue")
;;   (set-face-foreground font-lock-string-face "DarkGreen")
;;   (set-face-foreground font-lock-string-face "CadetBlue")
 (set-face-foreground font-lock-string-face "Navyblue")


;; COMMENT-FACE
;; (set-face-foreground font-lock-comment-face "Grey")
;; (set-face-foreground font-lock-comment-face "LightGray")
;; (set-face-foreground font-lock-comment-face "LightSteelBlue")
   (set-face-foreground font-lock-comment-face "Brown")  ; Laptop
;; (set-face-foreground font-lock-comment-face "DarkTurquoise")
;; (set-face-foreground font-lock-comment-face "BlueViolet")
;; (set-face-foreground font-lock-comment-face "CadetBlue")

;; KEYWORD-FACE
;; (set-face-foreground font-lock-keyword-face "Maroon")
;; (set-face-foreground font-lock-keyword-face "IndianRed")
;; (set-face-foreground font-lock-keyword-face "Coral")
   (set-face-foreground font-lock-keyword-face "DarkOrchid")


;; VARIABLE-NAME-FACE
;; (set-face-foreground font-lock-variable-name-face "LightGoldenrod")
   (set-face-foreground font-lock-variable-name-face "Coral")

;; FUNCTION-NAME-FACE
;; (set-face-foreground font-lock-function-name-face "LightSkyBlue")
(set-face-foreground font-lock-function-name-face "DarkOliveGreen")

;; REFERENCE-FACE
;; (set-face-foreground font-lock-reference-face "Aquamarine")
   (set-face-foreground font-lock-reference-face "Firebrick")

;; TYPE-FACE
   (set-face-foreground font-lock-type-face "MediumAquamarine")

;; HIGHLIGHT

   (set-face-background 'highlight 
			"MediumBlue"
;;			"MediumAquamarine"
			)

;; ITALIC
   (set-face-foreground 'italic "Sienna")

;; BOLD-ITALIC
   (set-face-foreground 'bold-italic "Sienna")

;; BOLD
   (set-face-foreground 'bold "Sienna")

;; ITALIC
   (set-face-foreground 'underline "Khaki")

;; this doesn't work, for some reason.  Set this interactively
   (set-face-background 'region "light green")
)

(add-hook 'info-mode-hook
  (function (lambda ()
	      (set-face-foreground 'info-node "White")
	      (set-face-foreground 'info-xref "Green")
	      (set-face-foreground 'info-menu-5 "Yellow"))))


		     
