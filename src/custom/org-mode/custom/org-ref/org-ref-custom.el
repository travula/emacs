;;; Org-ref customization Venkatesh Choppella [2015-09-20 Sat]

;;; see https://github.com/jkitchin/org-ref/blob/master/README.org

(setq reftex-default-bibliography
      '("~/venk/work/research/biblio/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/venk/work/research/biblio/notes.org"
      org-ref-default-bibliography '("~/venk/work/research/biblio/references.bib")
      org-ref-pdf-directory "~/venk/work/research/biblio/bibtex-pdfs/")


(setq helm-bibtex-bibliography "~/venk/work/research/biblio/references.bib")
(setq helm-bibtex-library-path "~/venk/work/research/biblio/bibtex-pdfs")

;; open pdf with system pdf viewer (works on mac)
(setq helm-bibtex-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq helm-bibtex-pdf-open-function 'org-open-file)

(setq helm-bibtex-notes-path "~/venk/work/research/biblio/helm-bibtex-notes")



(global-set-key [f10] 'org-ref-open-bibtex-notes)
(global-set-key [f11] 'org-ref-open-bibtex-pdf)
(global-set-key [f12] 'org-ref-open-in-browser)


(require 'doi-utils)
(require 'jmax-bibtex)
(require 'pubmed)
(require 'arxiv)
(require 'sci-id)

