(push :with-little-test *features*)

;; new common-lisp hacker 
;; please view module as directory
;;   which you can override by :pathname
;; and system as top-level directory
(defsystem :cicada-vm
  :description "virtual machine of cicada language"
  :author "XIE Yuheng <xyheme@gmail.com>"
  :depends-on ()
  :serial t
  :components
  ((:module "package-header"
            :components
            ((:file "package-header")))
   (:module "basic"
            :components
            ((:file "basic")))
   (:module "little-tester"
            :components
            ((:file "little-tester")))
   (:module "basic.test" :pathname "basic"
            :components
            ((:file "basic.test"
                    :if-feature :with-little-test)))
   (:module "cicada-rhapsody"
            :components
            ((:file "cicada-rhapsody")
             (:file "cicada-rhapsody.test"
                    :if-feature :with-little-test)))
   ;; (:module ghost-in-shell
   ;;          :components
   ;;          ((:file "ghost-in-shell")
   ;;           (:file "ghost-in-shell.test"
   ;;                  :if-feature :with-little-test)))
   (:file "cicada-vm")
   (:file "cicada-vm.test"
          :if-feature :with-little-test)
   ))
