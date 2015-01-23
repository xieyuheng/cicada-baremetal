(push :with-little-test *features*)

(defsystem :cicada-vm
  :description "virtual machine of cicada language"
  :author "XIE Yuheng <xyheme@gmail.com>"
  :depends-on ()
  :serial t
  :components
  ((:module :package-header
            :components
            ((:file "package-header")))
   (:module :basic
            :components
            ((:file "basic")
             (:file "basic.test"
                    :if-feature :with-little-test)))
   (:module :ghost-in-shell
            :components
            ((:file "ghost-in-shell")
             (:file "ghost-in-shell.test"
                    :if-feature :with-little-test)))
   (:file "cicada-vm")
   (:file "cicada-vm.test"
          :if-feature :with-little-test)))
