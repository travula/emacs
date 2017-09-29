(defun list-filter (pred ls)
  (let ((ans nil))
    (dolist (el ls (reverse ans)) (if (funcall pred el) (push el ans)))))

(defun list-prefix (ls1 ls2)
  
(provide 'list-operations)
