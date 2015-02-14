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
   (:module "ytool"
            :components
            ((:file "ytool")))
   (:module "regular-common-lisp"
            :components
            ((:file "regular-common-lisp")))
   (:module "little-tester"
            :components
            ((:file "little-tester")))
   (:module "ytool.test" :pathname "ytool"
            :components
            ((:file "ytool.test"
                    :if-feature :with-little-test)))
   (:module "regular-common-lisp.test" :pathname "regular-common-lisp"
            :components
            ((:file "regular-common-lisp.test"
                    :if-feature :with-little-test)))
   (:module "cicada-rhapsody"
            :components
            ((:file "cicada-rhapsody")
             (:file "cicada-rhapsody.test"
                    :if-feature :with-little-test)))
   (:module "architecture"
            :components
            ((:file "architecture")
             (:file "architecture.test"
                    :if-feature :with-little-test)))
   (:module "instruction-set"
            :components
            ((:file "instruction-set")
             (:file "instruction-set.test"
                    :if-feature :with-little-test)))
   (:module "compiler"
            :components
            ((:file "compiler")
             (:file "compiler.test"
                    :if-feature :with-little-test)))
   ;; (:module ghost-in-shell
   ;;          :components
   ;;          ((:file "ghost-in-shell")
   ;;           (:file "ghost-in-shell.test"
   ;;                  :if-feature :with-little-test)))
   ))
