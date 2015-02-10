(in-package :cicada-vm)
(deftest print#title
    (cicada-vm)
  (ensure
      (print#title (string->title "kkk")
                   :to nil)
      ==>
      "kkk"))
(deftest be--and--ask
    (cicada-vm)
  (ensure
      (list (be :title (string->title "kkk")
                :name (string->name "took")
                :title#object (string->title "my")
                :value#object 666)
            (with (be :title (string->title "kkk")
                      :name (string->name "took")
                      :title#object (string->title "my")
                      :value#object 666)
              (list .field .update?))
            (with (ask :title (string->title "kkk")
                       :name (string->name "took"))
                  (list .title .value .found?)))
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

(deftest print#name
    (cicada-vm)
  (ensure
      (print#name (string->name "kkk took my baby away!")
                  :to nil)
      ==>
      "kkk took my baby away!"))
(deftest return-stack
    (cicada-vm)
  (ensure
      (let* ((push1 (push#return-stack
                     :title (string->title "return-stack--push--test#1")
                     :value 147))
             (push2 (push#return-stack
                     :title (string->title "return-stack--push--test#2")
                     :value 258))
             (push3 (push#return-stack
                     :title (string->title "return-stack--push--test#3")
                     :value 369)))
        (list (sub push3 push2)
              (sub push2 push1)
              (with (tos#return-stack)
                .value)
              (with (pop#return-stack)
                .value)

              (with (tos#return-stack)
                .value)
              (with (pop#return-stack)
                .value)

              (with (tos#return-stack)
                .value)
              (with (pop#return-stack)
                .value)))
      ==>
      (list 1
            1

            369
            369

            258
            258

            147
            147)))
(deftest argument-stack
    (cicada-vm)
  (ensure
      (list (push#argument-stack
             :title (string->title "argument-stack--push--test#1")
             :value 147)

            (push#argument-stack
             :title (string->title "argument-stack--push--test#2")
             :value 258)

            (push#argument-stack
             :title (string->title "argument-stack--push--test#3")
             :value 369)

            (with (tos#argument-stack)
              .value)
            (with (pop#argument-stack)
              .value)

            (with (tos#argument-stack)
              .value)
            (with (pop#argument-stack)
              .value)

            (with (tos#argument-stack)
              .value)
            (with (pop#argument-stack)
              .value))
      ==>
      (list 1
            2
            3

            369
            369

            258
            258

            147
            147)))
(deftest frame-stack
    (cicada-vm)
  (ensure
      (list (push#frame-stack
             :title (string->title "frame-stack--push--test#1")
             :value 147)

            (push#frame-stack
             :title (string->title "frame-stack--push--test#2")
             :value 258)

            (push#frame-stack
             :title (string->title "frame-stack--push--test#3")
             :value 369)

            (with (tos#frame-stack)
              .value)
            (with (pop#frame-stack)
              .value)

            (with (tos#frame-stack)
              .value)
            (with (pop#frame-stack)
              .value)

            (with (tos#frame-stack)
              .value)
            (with (pop#frame-stack)
              .value))
      ==>
      (list 1
            2
            3

            369
            369

            258
            258

            147
            147)))
