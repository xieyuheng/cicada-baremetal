(defsystem #:cicada-vm
  :description "virtual machine of cicada language"
  :author "XIE Yuheng <xyheme@gmail.com>"
  :depends-on ()
  :serial t
  :components ((:file "cicada-vm--package-header")
               (:file "cicada-vm")))
