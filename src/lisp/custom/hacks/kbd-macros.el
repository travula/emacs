;; saves the buffer after removing extra line-feeds imported from the PC.
(fset 'pc-to-unix
   [?\M-% ?\C-q ?\C-m return return ?! ?\C-x ?\C-s])
