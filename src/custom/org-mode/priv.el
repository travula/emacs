(org-add-link-type
 "slink:"
 ;;  follow function
 (lambda (path)
   (let* ((data (read (format "(%s)" path)))
          (head (car data))
          (plist (cdr data))
          (link (org-element-context))
          (begin (org-element-property :begin link))
          (end (org-element-property :end link)))
     (setq plist (plist-put plist :last-clicked (current-time-string)))
     (save-excursion
     (setf (buffer-substring begin end) "")
     (goto-char begin)
     (insert (format "[[vlink:%s %s]]" head
         (substring (format "%S" plist) 1 -1))))))
 ;; format function
 (lambda (path description backend)
   (let* ((data (read (concat "(" path ")")))
          (head (car data))
          (plist (cdr data)))
     (format "\\%s[%s][%s]{%s}"
             (plist-get plist :type)
             (plist-get plist :pre)
             (plist-get plist :post)
             head))))




;;; [[slink:http://www.google.com :id "spec" :class "foo"][special link]]

;;; choppell  [2016-10-18]

(org-add-link-type
 "ylink:"
 ;;  follow function
(lambda (path)
   (let* ((data (read (format "(%s)" path)))
          (head (car data))
          (plist (cdr data))
          (link (org-element-context))
          (begin (org-element-property :begin link))
          (end (org-element-property :end link)))
;     (setq plist (plist-put plist :last-clicked (current-time-string)))
     (save-excursion
     (setf (buffer-substring begin end) "")
     (goto-char begin)
     (insert (format "[[slink:%s %s]]" head
         (substring (format "%S" plist) 1 -1))))))

 ;; format function
 (lambda (path description backend)
   (let* ((data (read (concat "(" path ")")))
          (head (car data))
          (plist (cdr data)))
     (format "\\%s[%s][%s]{%s}"
             (plist-get plist :type)
             (plist-get plist :pre)
             (plist-get plist :post)
             head))))





