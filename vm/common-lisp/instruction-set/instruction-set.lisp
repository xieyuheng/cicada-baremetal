(in-package :cicada-vm)
(define-primitive-function "fixnum" "add1"
    (@ <fixnum> -- <fixnum> @)
  (save#argument-stack
   :index 0
   :value (add1 (fetch#argument-stack
                 :index 0
                 :field :value))))
