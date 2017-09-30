;;; mew-highlight.el --- Highlight for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 18, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cursor line
;;;

(defun mew-highlight-cursor-line ()
  "A function to highlight the cursor line in Summary and Virtual mode."
  (when mew-use-highlight-cursor-line
    (mew-elet
     (if (overlayp (mew-sinfo-get-cursor-line))
	 (move-overlay (mew-sinfo-get-cursor-line)
		       (save-excursion (beginning-of-line) (point))
		       (save-excursion (end-of-line) (point)))
       (mew-sinfo-set-cursor-line
	(mew-overlay-make
	 (save-excursion (beginning-of-line) (point))
	 (save-excursion (end-of-line) (point))))
       (overlay-put
	(mew-sinfo-get-cursor-line) 'face mew-highlight-cursor-line-face))))
  (when mew-use-cursor-mark
    (unless (markerp overlay-arrow-position)
      (make-local-variable 'overlay-arrow-position)
      (setq overlay-arrow-position (make-marker)))
    (unless (stringp overlay-arrow-string)
      (make-local-variable 'overlay-arrow-string)
      (setq overlay-arrow-string mew-cursor-mark))
    (set-marker overlay-arrow-position
		(save-excursion (beginning-of-line) (point)))))

(defun mew-unhighlight-cursor-line ()
  (if (overlayp (mew-sinfo-get-cursor-line))
      (move-overlay (mew-sinfo-get-cursor-line) 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Marks
;;;

(defun mew-highlight-mark-line (mark)
  (when mew-use-highlight-mark
    (let ((face (mew-highlight-mark-get-face mark)))
      (when face
	(put-text-property
	 (save-excursion (beginning-of-line) (point))
	 (save-excursion (end-of-line) (point))
	 'face face)))))

(defun mew-highlight-unmark-line ()
  (remove-text-properties 
   (save-excursion (beginning-of-line) (point))
   (save-excursion (end-of-line) (point))
   '(face nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Header
;;;

(defun mew-unhighlight-region (BEG END)
  (mew-overlay-delete-region BEG END))

(defun mew-unhighlight-header ()
  (save-restriction
    (widen)
    (mew-unhighlight-region (point-min) (mew-header-end))))

(defun mew-highlight-header-region (BEG END)
  "A function to highlight header in Message and Draft mode."
  (when mew-use-highlight-header
    (let (key beg med nspec overlay key-face val-face)
      (save-excursion
	(mew-elet
	 (mew-unhighlight-region BEG END)
	 (save-restriction
	   (narrow-to-region BEG END)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (if (not (looking-at mew-keyval))
		 (forward-line)
	       (setq key (mew-match-string 1))
	       (setq beg (match-beginning 0))
	       (setq med (match-end 0))
	       (forward-line)
	       (setq nspec (mew-nspec-by-key key))
	       (setq key-face (or (mew-nspec-keyface nspec)
				  'mew-face-header-key))
	       (setq val-face (or (mew-nspec-valface nspec)
				  'mew-face-header-marginal))
	       (setq overlay (mew-overlay-make beg med))
	       (overlay-put overlay 'face key-face)
	       (setq overlay (mew-overlay-make med (1- (point))))
	       (overlay-put overlay 'face val-face)
	       (while (looking-at mew-lwsp+)
		 (forward-line)
		 (setq overlay (mew-overlay-make (match-end 0) (1- (point))))
		 (overlay-put overlay 'face val-face))))))))))
	       

(defun mew-highlight-header ()
  (save-restriction
    (widen)
    (mew-highlight-header-region (point-min) (mew-header-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Body
;;;

(defun mew-highlight-body-region (BEG END &optional draft rehighlight)
  (when (and (or mew-use-highlight-body mew-use-highlight-url)
	     (<= BEG mew-highlight-body-max-size))
    (if (> END mew-highlight-body-max-size)
	(setq END mew-highlight-body-max-size))
    (let* ((inhibit-point-motion-hooks t)
	   (cite-regex mew-highlight-body-regex-cite)
	   (cmt-regex mew-highlight-body-regex-comment)
	   (url-regex mew-regex-url)
	   (fancy-num 0)
	   (fancy-length (length mew-highlight-body-cite-faces))
	   beg end face fancy-alst fancy-prefix)
      (save-excursion
	(mew-elet
	 (when draft
	   (mew-rear-nonsticky BEG END)
	   (if rehighlight
	       (remove-text-properties BEG END '(face nil mouse-face nil))))
	 (when mew-use-highlight-body
	   (goto-char BEG)
	   (while (and (<= (point) END) (re-search-forward cite-regex END t))
	     (setq beg (match-beginning 0))
	     (setq end (match-end 0))
	     (if (match-beginning 1)
		 (setq fancy-prefix (mew-match-string 1))
	       (setq fancy-prefix nil))
	     (when (and fancy-prefix
			(< (string-width fancy-prefix)
			   mew-highlight-body-prefix-width))
	       (if (setq face (cdr (assoc fancy-prefix fancy-alst)))
		   (put-text-property beg end 'face face)
		 (setq face (nth fancy-num mew-highlight-body-cite-faces))
		 (setq fancy-alst (cons (cons fancy-prefix face) fancy-alst))
		 (setq fancy-num (1+ fancy-num))
		 (when (= fancy-length fancy-num)
		   (setq fancy-num 0))
		 (put-text-property beg end 'face face)))
	     (forward-line))
	   (goto-char BEG)
	   (while (and (<= (point) END) (re-search-forward cmt-regex END t))
	     (setq beg (match-beginning 0))
	     (setq end (match-end 0))
	     (put-text-property	beg end 'face 'mew-face-body-comment)))
	 (when mew-use-highlight-url
	   (goto-char BEG)
	   (while (and (<= (point) END) (re-search-forward url-regex END t))
	     (setq beg (match-beginning 0))
	     (setq end (match-end 0))
	     (put-text-property beg end 'face 'mew-face-body-url)
	     (put-text-property
	      beg end 'mouse-face mew-highlight-url-mouse-face))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X Face
;;;

(defun mew-highlight-x-face (beg end)
  (when (and mew-use-highlight-x-face window-system)
    (save-excursion
      (goto-char beg)
      (mew-elet
       (let ((regex1 "^X-Face: *\\(.*\\(\n[ \t].*\\)*\\)\n")
	     (buf (current-buffer))
	     overlay xface beg0 end0 xbmp)
	 (while (re-search-forward regex1 end t)
	   (setq beg0 (match-beginning 0))
	   (setq end0 (match-end 0))
	   (with-temp-buffer
	     (mew-insert-buffer-substring
	      buf (match-beginning 1) (match-end 1))
	     (setq xbmp (mew-x-face-compface-to-xbm))
	     (if xbmp (setq xface (mew-x-face-create))))
	   (when xface
	     (setq overlay (mew-overlay-make beg0 end0))
	     (overlay-put overlay 'invisible t)
	     (save-restriction
	       (narrow-to-region beg end)
	       (mew-x-face-display xface )))))))))

(defun mew-x-face-compface-to-xbm ()
  (when (and (mew-which-exec mew-prog-uncompface)
	     (mew-which-exec mew-prog-icontopbm)
	     (mew-which-exec mew-prog-pbmtoxbm))
    (mew-set-buffer-multibyte nil)
    (mew-flet
     (call-process-region (point-min) (point-max) mew-prog-uncompface t t nil)
     (goto-char (point-min))
     (insert "/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */\n")
     (call-process-region (point-min) (point-max) mew-prog-icontopbm t t nil)
     (if mew-use-highlight-x-face-inversion
	 (call-process-region (point-min) (point-max) mew-prog-pbminvert t t nil))
     (call-process-region (point-min) (point-max) mew-prog-pbmtoxbm t t nil))
    t)) ;; return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cooking
;;;

(defun mew-summary-cook-window (&optional WIN BEG)
  (let* ((win (or WIN (selected-window)))
	 (beg (or BEG (window-start win)))
	 (end (window-end win t))
	 (buf (window-buffer win)))
    (save-excursion
      (set-buffer buf)
      (funcall mew-summary-cook-function beg end))))

;; See also mew-scan-insert-line
(defun mew-summary-cook-region (beg end &optional interrupt)
  (when (and (mew-summary-or-virtual-p) mew-summary-buffer-raw)
    (let ((inhibit-point-motion-hooks t)
	  (regex mew-regex-msg-mark)
	  ret mark face start med)
      (catch 'loop
	(save-excursion
	  (mew-elet
	   (goto-char beg)
	   (if (mew-in-decode-syntax-p)
	       (goto-char (mew-decode-syntax-end)))
	   (if (and (mew-thread-p)
		    mew-use-thread-separator
		    (not (mew-summary-message-number)))
	       (forward-line))
	   (setq start (point))
	   (while (and (< (point) end) ;; we cannot trust end
		       (search-forward "\r" end t))
	     (if (and interrupt (input-pending-p))
		 (throw 'loop (setq ret t)))
	     (setq med (match-beginning 0))
	     (forward-line)
	     (mew-front-nonsticky start med) ;; for XEmacs
	     (if (and mew-use-highlight-mouse-line window-system)
		 (put-text-property
		  start med 'mouse-face mew-highlight-mouse-line-face))
	     (put-text-property med (1- (point)) 'invisible t)
	     (if (mew-in-decode-syntax-p)
		 (goto-char (mew-decode-syntax-end)))
	     (if (and (mew-thread-p)
		      mew-use-thread-separator
		      (not (mew-summary-message-number)))
		 (forward-line))
	     (setq start (point)))
	   (when mew-use-highlight-mark
	     (goto-char beg)
	     (while (and (< (point) end) ;; we cannot trust end
			 (re-search-forward regex end t))
	       (if (and interrupt (input-pending-p))
		   (throw 'loop (setq ret t)))
	       (setq mark (string-to-char (mew-match-string 2)))
	       (setq face (mew-highlight-mark-get-face mark))
	       (beginning-of-line)
	       (setq start (point))
	       (forward-line)
	       (if face (put-text-property start (1- (point)) 'face face)))))))
      (set-buffer-modified-p nil)
      ret)))

(defun mew-summary-cook-folders ()
  (let ((bufs mew-buffers) buf)
    (save-excursion
      (while bufs
	(setq buf (car bufs))
	(setq bufs (cdr bufs))
	(when (and (get-buffer buf) (not (input-pending-p)))
	  (set-buffer buf)
	  (setq mew-summary-buffer-raw
		(mew-summary-cook-region
		 (point-min) (point-max) 'interrupt)))))))

(defvar mew-highlight-timer-id nil)

(defun mew-highlight-timer-setup ()
  (if mew-highlight-timer-id (cancel-timer mew-highlight-timer-id))
  (setq mew-highlight-timer-id
	(run-with-idle-timer mew-highlight-timer-interval
			     t 'mew-summary-cook-folders)))

(defun mew-highlight-timer-clean-up ()
  (if mew-highlight-timer-id (cancel-timer mew-highlight-timer-id))
  (setq mew-highlight-timer-id nil))

(provide 'mew-highlight)

;;; Copyright Notice:

;; Copyright (C) 1997-2003 Mew developing team.
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

;;; mew-highlight.el ends here
