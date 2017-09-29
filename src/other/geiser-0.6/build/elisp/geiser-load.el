(defconst geiser-elisp-dir nil)
(setq geiser-elisp-dir (file-name-directory load-file-name))
(add-to-list 'load-path geiser-elisp-dir)

(require 'geiser)

(setq geiser-scheme-dir "/home/choppell/emacs/lisp/geiser-0.6/build/../scheme")

(provide 'geiser-load)
