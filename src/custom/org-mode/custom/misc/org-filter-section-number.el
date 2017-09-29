;; http://bastibe.de/2014-12-03-org-numbering.html
;; Numbers Headlines selectively
;; Add a property :NUMBERS: no

(defun headline-numbering-filter (data backend info)
  "No numbering in headlines that have a property :numbers: no"
  (let* ((beg (next-property-change 0 data))
         (headline (if beg (get-text-property beg :parent data))))
    (if (string= (org-element-property :NUMBERS headline) "no")
        (cond ((eq backend 'latex)
               (replace-regexp-in-string
                "\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)"
                "\\1*" data nil nil 1))
              ((eq backend 'html)
               (replace-regexp-in-string
                "\\(<h[1-6]\\)\\([^>]*>\\)"
                "\\1 class=\"nonumber\"\\2" data nil nil)))
      data)))

(setq org-export-filter-headline-functions '(headline-numbering-filter))
