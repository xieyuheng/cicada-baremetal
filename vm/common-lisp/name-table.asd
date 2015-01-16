(defsystem #:name-table
  :description "cicada language's hash table of string"
  :author "XIE Yuheng <xyheme@gmail.com>"
  :depends-on (#:xyh-lib)
  :serial t
  :components ((:file "name-table--package-header")
               (:file "name-table")))
