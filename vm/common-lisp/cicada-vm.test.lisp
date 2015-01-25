(in-package :cicada-vm)
(deftest title?
    (cicada-vm)
  (ensure
      (list (title? #(<title> 0))

            (title? (string->title "testing#title?")))
      ==>
      (list t
            t)))
(deftest title->index
    (cicada-vm)
  (ensure
      (let ((test1 (title->index (string->title "testing#1#title->index")))
            (test2 (title->index (string->title "testing#2#title->index"))))
        (- test2 test1))
      ==>
      1))

(deftest print-title
    (cicada-vm)
  (ensure
      (print-title (string->title "kkk")
                   :stream nil)
      ==>
      "[kkk]"))
(deftest be--and--ask
    (cicada-vm)
  (ensure
      (list (be :title (string->title "kkk")
                :name (string->name "took")
                :cicada-object (make-cicada-object
                                :title (string->title "my")
                                :value 666))
            (multiple-value-list
             (be :title (string->title "kkk")
                 :name (string->name "took")
                 :cicada-object (make-cicada-object
                                 :title (string->title "my")
                                 :value 666)))
            (multiple-value-list
             (ask :title (string->title "kkk")
                  :name (string->name "took"))))
      ==>
      (list `1
            `(1
              :updated!!!)
            `(,(vector '<cicada-object>
                      (cicada-object->cicada-byte-vector
                       (make-cicada-object
                        :title (string->title "my")
                        :value 666)))
              :found!!!))))
(deftest name?
    (cicada-vm)
  (ensure
      (name? #(<name> 0))
      ==>
      t))
(deftest string->natural-number
    (cicada-vm)
  (ensure
      (list (string->natural-number "")
            (string->natural-number "@")
            (string->natural-number "@@@"))
      ==>
      (list 0
            64
            448)))

(deftest natural-number->index
    (cicada-vm)
  (ensure
      (list (natural-number->index 0)
            (natural-number->index 123)
            (natural-number->index *size#name-hash-table*))
      ==>
      (list 0
            123
            0)))
(deftest name->string
    (cicada-vm)
  (ensure
      (name->string (string->name "kkk took my baby away!"))
      ==>
      "kkk took my baby away!"))

(deftest print-name
    (cicada-vm)
  (ensure
      (print-name (string->name "kkk took my baby away!")
                  :stream nil)
      ==>
      "[kkk took my baby away!]"))
(deftest cicada-object?
    (cicada-vm)
  (ensure
      (cicada-object?
       (make-cicada-object :title (string->title "kkk")
                           :value 666))
      ==>
      t))
(deftest make-cicada-object
    (cicada-vm)
  (ensure
      (list (fetch#byte-vector
             :byte-vector
             (cicada-object->cicada-byte-vector
              (make-cicada-object :title (string->title "kkk")
                                  :value 666))
             :size *cell-unit*
             :index *cell-unit*)
            (equal? (array-element-type
                     (cicada-object->cicada-byte-vector
                      (make-cicada-object :title (string->title "kkk")
                                          :value 666)))
                    '(unsigned-byte 8)))
      ==>
      (list 666
            t)))
(deftest push#return-stack
    (cicada-vm)
  (ensure
      (multiple-value-bind
            (pointer-index#1
             cicada-object#1)
          (push#return-stack
           (cicada-object->cicada-byte-vector
            (make-cicada-object :title (string->title "kkk")
                                :value 666)))
        (multiple-value-bind
              (pointer-index#2
               cicada-object#2)
            (push#return-stack
             (cicada-object->cicada-byte-vector
              (make-cicada-object :title (string->title "kkk")
                                  :value 666)))
          (list (- pointer-index#2
                   pointer-index#1)
                (every (function equal?)
                       cicada-object#1
                       cicada-object#2)
                (every (function equal?)
                       cicada-object#1
                       (cicada-object->cicada-byte-vector
                        (make-cicada-object :title (string->title "kkk")
                                            :value 666)))
                (equal? (car (cdr (multiple-value-list (pop#return-stack)))) (sub1 pointer-index#2))
                (equal? (car (cdr (multiple-value-list (tos#return-stack)))) (sub1 pointer-index#1))
                (every (function equal?)
                       (pop#return-stack)
                       (cicada-object->cicada-byte-vector
                        (make-cicada-object :title (string->title "kkk")
                                            :value 666)))
                )))
      ==>
      (list 1
            t
            t
            t
            t
            t)))
