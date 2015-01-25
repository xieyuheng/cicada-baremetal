(in-package :cicada-vm)
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
                :title#object (string->title "my")
                :value#object 666)
            (multiple-value-list
             (be :title (string->title "kkk")
                 :name (string->name "took")
                 :title#object (string->title "my")
                 :value#object 666))
            (multiple-value-list
             (ask :title (string->title "kkk")
                  :name (string->name "took"))))
      ==>
      `(1

        (1
         :UPDATED!!!)

        (,(string->title "my")
          666
          :FOUND!!!)        
        )))
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
(deftest return-stack
    (cicada-vm)
  (ensure
      (list (push#return-stack
             :title (string->title "return-stack--push--test#1")
             :value 147)

            (push#return-stack
             :title (string->title "return-stack--push--test#2")
             :value 258)
         
            (push#return-stack
             :title (string->title "return-stack--push--test#3")
             :value 369)

            (multiple-value-bind
                  (title
                   value
                   current-pointer)
                (tos#return-stack)
              value)
            (multiple-value-bind
                  (title
                   value
                   current-pointer)
                (pop#return-stack)
              value)

            (multiple-value-bind
                  (title
                   value
                   current-pointer)
                (tos#return-stack)
              value)
            (multiple-value-bind
                  (title
                   value
                   current-pointer)
                (pop#return-stack)
              value)

            (multiple-value-bind
                  (title
                   value
                   current-pointer)
                (tos#return-stack)
              value)
            (multiple-value-bind
                  (title
                   value
                   current-pointer)
                (pop#return-stack)
              value))
      ==>
      (list 1
            2
            3
            369
            369
            258
            258
            147
            147
            )))
