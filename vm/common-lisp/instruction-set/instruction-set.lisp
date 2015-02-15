(in-package :cicada-vm)
(define-primitive-function "fixnum" "neg"
    (@ <fixnum> -- <fixnum> @)
  (with (pop#argument-stack)
    (push#argument-stack
     :title .title
     :value (neg .value))))

(define-primitive-function "fixnum" "add"
  (@ <fixnum> <fixnum> -- <fixnum> @)
  (with (pop#argument-stack)
    (let ((value2 .value))
      (with (pop#argument-stack)
        (push#argument-stack
         :title .title
         :value (add .value value2))))))

(define-primitive-function "fixnum" "sub"
    (@ <fixnum> <fixnum> -- <fixnum> @)
  (with (pop#argument-stack)
    (let ((value2 .value))
      (with (pop#argument-stack)
        (push#argument-stack
         :title .title
         :value (sub .value value2))))))

(define-primitive-function "fixnum" "mul"
    (@ <fixnum> <fixnum> -- <fixnum> @)
  (with (pop#argument-stack)
    (let ((value2 .value))
      (with (pop#argument-stack)
        (push#argument-stack
         :title .title
         :value (mul .value value2))))))

;; (define-primitive-function "fixnum" "moddiv"
;;     (@ <fixnum> <fixnum> -- <fixnum> <fixnum> @)
;;   (@ dividend, divisor -- remainder, quotient @))

;; (define-primitive-function "fixnum" "divmod"
;;     (@ <fixnum> <fixnum> -- <fixnum> <fixnum> @)
;;   (@ a, b -- quotient, a mod b @))

(define-primitive-function "fixnum" "div"
    (@ <fixnum> <fixnum> -- <fixnum> @)
  (with (pop#argument-stack)
    (let ((value2 .value))
      (with (pop#argument-stack)
        (push#argument-stack
         :title .title
         :value (div .value value2))))))

(define-primitive-function "fixnum" "mod"
    (@ <fixnum> <fixnum> -- <fixnum> @)
  (with (pop#argument-stack)
    (let ((value2 .value))
      (with (pop#argument-stack)
        (push#argument-stack
         :title .title
         :value (mod .value value2))))))

(define-primitive-instruction "primitive-function" "call"
    ;; ><><>< should do title check
    ;; 还有声明副作用的语法 如何
    ;; 比如 对 return-stack 的副作用
    (@ -- @)
  (with (fetch#cicada-memory
         :address (add *cicada-object-size*
                       (with (tos#return-stack)
                         .value)))
    ;; this means only primitive-instruction is handled now
    (funcall (primitive-function->host-function .value))))
