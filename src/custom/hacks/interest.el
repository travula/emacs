(defun sum ()
  (let* ((r 0.36) (r12 (/ r 12)))
  (let ((i 1))
    (while (<= i 10)
      (princ (format "%3d     %f    %f" 
                     i (expt (+ 1 r) i)
		       (expt (+ 1 r12) (* i 12))))
      (newline)
      (setq i (+ 1 i))))))

 
