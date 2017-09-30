(defun right-mark (col str)
  "move to column number col, adding spaces if neccessary, 
   and insert string str"
  (move-to-column col 'force)
  (insert str))

(right-mark 25 "?")


















			 


