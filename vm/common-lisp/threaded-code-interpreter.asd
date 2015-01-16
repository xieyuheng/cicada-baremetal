(defsystem #:threaded-code-interpreter
  :description "threaded-code interpreter of cicada language"
  :author "XIE Yuheng <xyheme@gmail.com>"
  :depends-on (#:xyh-lib
               #:name-table)
  :serial t
  :components ((:file "threaded-code-interpreter--package-header")
               (:file "threaded-code-interpreter")))
