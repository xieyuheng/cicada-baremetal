(push :with-little-test *features*)

(defsystem :cicada-vm
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
   (:file "cicada-vm")
   (:file "cicada-vm.test"
          :if-feature :with-little-test)))
