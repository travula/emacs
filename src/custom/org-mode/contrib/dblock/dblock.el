;;; For getting custom latex macros to work with HTML export
;;; http://stackoverflow.com/questions/12717654/customizing-org-mode-exports

(defun org-dblock-write:block-macro (params)
  (let ((block-name (or (plist-get params :from) "macros"))
    (org-buf (current-buffer)))
    (with-temp-buffer
      (let ((tmp-buf (current-buffer)))
    (set-buffer org-buf)
    (save-excursion
      (org-babel-goto-named-src-block block-name)
      (org-babel-mark-block)
      (let ((mblock-begin (region-beginning))
            (mblock-end (region-end)))
        (set-buffer tmp-buf)
        (insert-buffer-substring org-buf mblock-begin mblock-end)))
    (set-buffer org-buf)
    (insert "#+BEGIN_HTML\n\\(\n")
    (insert-buffer-substring tmp-buf)
    (insert "\\)\n#+END_HTML\n")
    (set-buffer tmp-buf)
    (beginning-of-buffer)
    (while (re-search-forward "^" nil t)
      (replace-match "#+LATEX_HEADER: " nil nil))
    (set-buffer org-buf)
    (insert-buffer-substring tmp-buf)))))
