(defun comment-out-region-1 (start end)
  "This function places lisp-style comments at the
   beginning of each line in the region"
  (save-excursion 
    (goto-char start);      
    (print end (get-buffer "*scratch*"))
    (catch 'commenting-at-end-of-buffer
      (while (<= (point) end)
	(insert comment-start)
	(setq end (+ 1 end))
	(if (= 1 (forward-line 1)) ;;; If end of buffer
	    (throw 'commenting-at-end-of-buffer nil))))))
(defun comment-out-region ()
  (interactive)
  (comment-out-region-1 (region-beginning) (region-end)))


