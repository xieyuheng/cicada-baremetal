(in-package :cicada-vm)
(deftest host-object?
    (cicada-vm)
  (ensure
      (host-object? #(<host-object>
                      #(<title> 0)
                      #(<name> 0)))
      ==>
      T))
(deftest make-cicada-object
    (cicada-vm)
  (ensure
      (list (fetch#byte-vector
             :byte-vector (make-cicada-object :title (string->title "kkk")
                                              :value 666)
             :size *cell-unit*
             :index *cell-unit*)
            (equal? (array-element-type
                     (make-cicada-object :title (string->title "kkk")
                                         :value 666))
                    '(unsigned-byte 8)))
      ==>
      (list 666
            t)))
(deftest cicada-object?
    (cicada-vm)
  (ensure
      (cicada-object?
       (make-cicada-object :title (string->title "kkk")
                           :value 666))
      ==>
      t))
(deftest host-object->cicada-object
    (cicada-vm)
  (ensure
      (multiple-value-list
       (host-object->cicada-object
        `#(<host-object>
           ,(string->title "testing#host-object->cicada-object")
           #b10000000)))
      ==>
      (list `#(,(title->index
                 (string->title
                  "testing#host-object->cicada-object"))
               0 0 0
               128 0 0 0)
            `128)))
(deftest cicada-object->host-object
    (cicada-vm)
  (ensure
      (cicada-object->host-object
       (host-object->cicada-object
        `#(<host-object>
           ,(string->title "testing#host-object->cicada-object")
           #b10000000)))
      ==>
      `#(<HOST-OBJECT>
         ,(string->title "testing#host-object->cicada-object")
         128)))
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
            (natural-number->index *size#name-table*))
      ==>
      (list 0
            123
            0)))
(deftest name?
    (cicada-vm)
  (ensure
      (name? #(<name> 0))
      ==>
      t))
(deftest name->string
    (cicada-vm)
  (ensure
      (name->string (string->name "kkk took my baby away!"))
      ==>
      "kkk took my baby away!"))
(deftest print-name
    (cicada-vm)
  (ensure
      ;; (let ((test-stream (make-string-output-stream)))
      ;;   (print-name (string->name "kkk took my baby away!")
      ;;               :stream test-stream)
      ;;   (get-output-stream-string test-stream))
      (print-name (string->name "kkk took my baby away!")
                  :stream nil)
      ==>
      "[kkk took my baby away!]"))
(deftest be--and--explain
    (cicada-vm)
  (ensure
      (list (be :name (string->name "kkk")
                 :as (string->name "took")
                 :mean "my baby away!")
            (multiple-value-list
             (be :name (string->name "kkk")
                 :as (string->name "took")
                 :mean "my baby away!"))
            (multiple-value-list
             (explain :name (string->name "kkk")
                      :as (string->name "took"))))
      ==>
      (list 1
            `(2
              :UPDATED!!!
              "my baby away!")
            `("my baby away!"
              :found!!!))
    ;; (list 2
    ;;         `(2
    ;;           :UPDATED!!!
    ;;           "my baby away!")
    ;;         `("my baby away!"
    ;;           :found!!!))
    ))
(deftest meaningful?
    (cicada-vm)
  (ensure
      (meaningful? :name (string->name "kkk")
                   :as (string->name "took"))
      ==>
      :found!!!))
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
(deftest entitle--and--ask
    (cicada-vm)
  (ensure
      (list (entitle :title (string->title "kkk")
                     :name (string->name "took")
                     :object `#(<host-object>
                                ,(string->title "my")
                                "baby away!"))
            (multiple-value-list
             (entitle :title (string->title "kkk")
                      :name (string->name "took")
                      :object `#(<host-object>
                                 ,(string->title "my")
                                 "baby away!")))
            (multiple-value-list
             (ask :title (string->title "kkk")
                  :name (string->name "took"))))
      ==>
      (list `1
            `(1
              :updated!!!
              #(<host-object> ,(string->title "my") "baby away!"))
            `(#(<host-object> ,(string->title "my") "baby away!")
               :found!!!))))
(deftest entitled?
    (cicada-vm)
  (ensure
      (entitled? :title (string->title "kkk")
                 :name (string->name "took"))
      ==>
      :found!!!))
(deftest print-title
    (cicada-vm)
  (ensure
      ;; (let ((test-stream (make-string-output-stream)))
      ;;   (print-title (string->title "kkk")
      ;;                :stream test-stream)
      ;;   (get-output-stream-string test-stream))
      (print-title (string->title "kkk")
                   :stream nil)
      ==>
      "[title]"))
(deftest push#return-stack
    (cicada-vm)
  (ensure
      (list (multiple-value-list
             (push#return-stack
              (make-cicada-object :title (string->title "kkk")
                                  :value 666)))
            (multiple-value-list
             (push#return-stack
              (make-cicada-object :title (string->title "kkk")
                                  :value 666)))
            (multiple-value-list
             (pop#return-stack))
            (multiple-value-list
             (pop#return-stack)))
      ==>
      (list `(1 ,(make-cicada-object :title (string->title "kkk")
                                     :value 666))
            `(2 ,(make-cicada-object :title (string->title "kkk")
                                     :value 666))
            `(,(title->index (string->title "kkk"))
              1)
            `(,(title->index (string->title "kkk"))
              0))))
