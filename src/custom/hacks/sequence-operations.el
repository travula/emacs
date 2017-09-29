(require 'cl)

(defun sequence-prefix-p (seq1 seq2)
  (eq 0 (search seq1 seq2)))

(provide 'sequence-operations)
