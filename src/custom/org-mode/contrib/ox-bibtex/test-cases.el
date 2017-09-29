

(let ((keyword '(keyword (:key "BIBLIOGRAPHY" :value "venk plain option:-d option:-r limit:t" :begin 223 :end 278 :post-blank 0 :post-affiliated 223))))
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (match-string 1 value))))

(let ((value "venk,foo/bar/baz plain option:-d option:-r limit:t"))
  (and
   (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
   (match-string 1 value)))

(let ((value ""))
  (and
   (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
))



(pcsv-parse-string nil)








(pcsv-test-get "venk,foo/bar/baz
 x,y")
(("venk" "foo/bar/baz") (" x" "y"))












