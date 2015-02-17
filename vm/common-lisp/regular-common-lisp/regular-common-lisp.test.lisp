(in-package :cicada-vm)
(deftest natural-number?
    (regular-common-lisp)
  (ensure
      (list (natural-number? 0)
            (natural-number? 1)
            (natural-number? -1)
            (natural-number? 1.1))
      ==>
      (list t
            t
            nil
            nil)))
(deftest shift#left
    (regular-common-lisp)
  (ensure
      (list (shift#left :number 10)
            (shift#left :step 2
                        :number 10))
      ==>
      (list 20
            40)))

(deftest shift#right
    (regular-common-lisp)
  (ensure
      (list (shift#right :number 64)
            (shift#right :step 2
                         :number 64))
      ==>
      (list 32
            16)))
(deftest fetch#bits
    (regular-common-lisp)
  (ensure
      (fetch#bits :bits #b0010
                  :size 1
                  :index 1)
      ==>
      1))
(deftest fetch#bytes
    (regular-common-lisp)
  (ensure
      (list (fetch#bytes :bytes #xff  :index 0)
            (fetch#bytes :bytes #xff  :index 1)
            (fetch#bytes :bytes #x100 :index 0)
            (fetch#bytes :bytes #x100 :index 1))
      ==>
      `(255
        0
        0
        1)))
(deftest fetch#array
    (regular-common-lisp)
  (ensure
      (fetch#array
       :array (make-array '(1 1 1) :initial-element 666)
       :index-vector '#(0 0 0))
      ==>
      666))

(deftest save#array
    (regular-common-lisp)
  (ensure
      (fetch#array
       :array (save#array
               :value 258
               :array (make-array '(1 1 1) :initial-element 666)
               :index-vector '#(0 0 0))
       :index-vector '#(0 0 0))
      ==>
      258))
(deftest map#vector--sub-vector
    (regular-common-lisp)
  (ensure
      (map#vector
       :width 2
       :number 2
       :function (lambda (&key sub-vector) sub-vector)
       :vector #(0 0 1 1 2 2))
      ==>
      (list #(0 0) #(1 1))))

(deftest map#vector--element
    (regular-common-lisp)
  (ensure
      (map#vector
       :width 1
       :number 2
       :function (lambda (&key element) element)
       :vector #(0 0 1 1 2 2))
      ==>
      (list 0 0)))
(deftest fetch#byte-array
    (regular-common-lisp)
  (ensure
      (let ((k (make-array `(4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 1)))
        (fetch#byte-array :byte-array k
                          :size 2
                          :index-vector #(0)))
      ==>
      257))

(deftest fetch#byte-array--big-endian
    (regular-common-lisp)
  (ensure
      (let ((k (make-array `(4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 1)))
        (fetch#byte-array :byte-array k
                          :size 2
                          :index-vector #(0)
                          :endian 'big))
      ==>
      257))

(deftest save#byte-array
    (regular-common-lisp)
  (ensure
      (let ((k (make-array `(4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 1)))
        (save#byte-array :value 1234
                         :byte-array k
                         :size 2
                         :index-vector '#(0))
        (fetch#byte-array :byte-array k
                          :size 2
                          :index-vector '#(0)))
      ==>
      1234))

(deftest save#byte-array--big-endian
    (regular-common-lisp)
  (ensure
      (let ((k (make-array `(4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 1)))
        (save#byte-array :value 1234
                         :byte-array k
                         :size 2
                         :index-vector #(0)
                         :endian 'big)
        (fetch#byte-array :byte-array k
                          :size 2
                          :index-vector #(0)
                          :endian 'big))
      ==>
      1234))
(deftest fetch#byte-vector
    (regular-common-lisp)
  (ensure
      (let ((k (make-array `(4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 1)))
        (fetch#byte-vector :byte-vector k
                           :size 2
                           :index 0))
      ==>
      257))

(deftest save#byte-vector
    (regular-common-lisp)
  (ensure
      (let ((k (make-array `(4)
                           :element-type '(unsigned-byte 8)
                           :initial-element 1)))
        (save#byte-vector :value 1234
                          :byte-vector k
                          :size 2
                          :index 0)
        (fetch#byte-vector :byte-vector k
                           :size 2
                           :index 0))
      ==>
      1234))
(deftest cat
    (regular-common-lisp)
  (ensure
      (cat ()
        ("~A" 123)
        ("~A" 456))
      ==>
      "123456"))

;; (cat ()
;;   ("~A" 123)
;;   ("~A" 456))

;; (cat (:to *standard-output*)
;;   ("~%")
;;   ("~A~%" 123)
;;   ("~A~%" 456))

;; (let ((x 123))
;;   (cat (:to *standard-output*)
;;     ("~A~%" x)))
(deftest char#space?
    (regular-common-lisp)
  (ensure
      (list (char#space? #\newline)
            (char#space? #\space))
      ==>
      (list t
            t)))

(deftest string#space?
    (regular-common-lisp)
  (ensure
      (list (string#space? " 123 ")
            (string#space? "  ")
            (string#space? ""))
      ==>
      (list nil
            t
            t)))
(deftest string->head#word
    (regular-common-lisp)
  (and (ensure
           (list (multiple-value-list (string->head#word " kkk took my baby away! "))
                 (multiple-value-list (string->head#word "k"))
                 (multiple-value-list (string->head#word " k"))
                 (multiple-value-list (string->head#word "k ")))
           ==>
           (list `("kkk" 4 1 " kkk took my baby away! ")
                 `("k" nil 0 "k")
                 `("k" nil 1 " k")
                 `("k" 1 0 "k ")))

       ;; the argument applied to string->head#word
       ;; must not be space-string
       ;; one should use string#space? to ensure this

       ;; just do not handle the error
       ;; let the debuger do its job
       (ensure
           (string->head#word " ")
           signals
           type-error)
       ))


(deftest string->tail#word
    (regular-common-lisp)
  (ensure
      (list (string->tail#word " kkk took my baby away! ")
            (string->tail#word "just-kkk"))
      ==>
      (list " took my baby away! "
            nil)))


(deftest string->list#word
    (regular-common-lisp)
  (ensure
      (list (string->list#word " kkk took my baby away! ")
            (string->list#word " kkk")
            (string->list#word "kkk ")
            (string->list#word " ")
            (string->list#word ""))
      ==>
      (list `("kkk" "took" "my" "baby" "away!")
            `("kkk")
            `("kkk")
            `nil
            `nil)))
(deftest string->head#line
    (regular-common-lisp)
  (ensure
      (list (string->head#line "123")
            (string->head#line (format nil "~%123"))
            (string->head#line (format nil "123~%")))
      ==>
      `("123"
        ""
        "123")))


(deftest string->tail#line
    (regular-common-lisp)
  (ensure
      (list (string->tail#line "123")
            (string->tail#line (format nil "~%123"))
            (string->tail#line (format nil "123~%")))
      ==>
      `(nil
        "123"
        "")))


(deftest string->list#line
    (regular-common-lisp)
  (ensure
      (string->list#line
       (cat (:postfix (cat () ("~%")))
         ("kkk")
         ("took")
         ("")
         ("my baby")
         ("")
         ("away!")
         ("")))
      ==>
      `("kkk"
        "took"
        ""
        "my baby"
        ""
        "away!"
        ""
        "")))
(deftest string->head#char
    (regular-common-lisp)
  (and (ensure
           (list (multiple-value-list (string->head#char " kkk took my baby away! "))
                 (multiple-value-list (string->head#char "k"))
                 (multiple-value-list (string->head#char " k"))
                 (multiple-value-list (string->head#char "k ")))
           ==>
           (list `(#\  "kkk took my baby away! " " kkk took my baby away! ")
                 `(#\k "" "k")
                 `(#\  "k" " k")
                 `(#\k " " "k ")))

       ;; the argument applied to string->head#char
       ;; must not be ""
       ;; one should use string#empty? to ensure this

       ;; just do not handle the error
       ;; let the debuger do its job
       (ensure
           (string->head#char "")
           signals
           type-error)
       ))

(deftest string->tail#char
    (regular-common-lisp)
  (and (ensure
           (string->tail#char " kkk took my baby away! ")
           ==>
           "kkk took my baby away! ")

       ;; just do not handle the error
       ;; let the debuger do its job
       (ensure
           (string->tail#char "")
           signals
           type-error)
       ))

(deftest string->list#char
    (regular-common-lisp)
  (ensure
      (list (string->list#char " kkk took my baby away! ")
            (string->list#char " kkk")
            (string->list#char "kkk ")
            (string->list#char " ")
            (string->list#char ""))
      ==>
      (list `(#\  #\k #\k #\k #\  #\t #\o #\o #\k #\  #\m #\y #\  #\b #\a #\b #\y #\  #\a
                  #\w #\a #\y #\! #\ )
            `(#\  #\k #\k #\k)
            `(#\k #\k #\k #\ )
            `(#\ )
            `nil)))
(deftest next-word!
    (regular-common-lisp)
  (ensure
      (let* ((*string#cicada-interpreter* " 1 2 3 ")
             (*cursor#cicada-interpreter* 0))
        (list (next-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)
              (next-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)
              (next-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)
              (next-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)))
      ==>
      (list "1" "2" "3" :no-more-word)))

(deftest back-word!
    (regular-common-lisp)
  (ensure
      (let* ((*string#cicada-interpreter* " 1 2 3 ")
             (*cursor#cicada-interpreter*
              (length *string#cicada-interpreter*)))
        (list (back-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)
              (back-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)
              (back-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)
              (back-word!
               :string *string#cicada-interpreter*
               :cursor *cursor#cicada-interpreter*)))
      ==>
      (list "3" "2" "1" :no-more-word)))

(deftest find-word!
    (regular-common-lisp)
  (ensure
      (let* ((*string#cicada-interpreter* " 123 ; 456 ; ")
             (*cursor#cicada-interpreter* 0)
             (find-cursor-1
              (find-word! :word ";"
                          :string *string#cicada-interpreter*
                          :cursor *cursor#cicada-interpreter*))
             (next-word
              (next-word! :string *string#cicada-interpreter*
                          :cursor *cursor#cicada-interpreter*))
             (find-cursor-2
              (find-word! :word ";"
                          :string *string#cicada-interpreter*
                          :cursor *cursor#cicada-interpreter*)))
        (list find-cursor-1
              next-word
              find-cursor-2))
      ==>
      (list 5 ";" 11)))
(defun pair? (x)
  (consp x))

(defun list? (x)
  (listp x))
(deftest end-of-list
    (regular-common-lisp)
  (and (ensure
           (end-of-list '(1 2 3))
           ==>
           3)
       (ensure
           (end-of-list '(1 2 . 3))
           signals
           simple-error)
       (ensure
           (end-of-list 3)
           signals
           simple-error)))
(deftest set-end-cdr!
    (regular-common-lisp)
  (ensure
      (let ((list '(1 2 3)))
        (set-end-cdr! 666 list)
        list)
      ==>
      '(1 2 3 . 666)))

(deftest set-end-car!
    (regular-common-lisp)
  (ensure
      (let ((list '(1 2 3)))
        (set-end-car! 666 list)
        list)
      ==>
      '(1 2 666)))
(deftest find#record
    (regular-common-lisp)
  (ensure (find#record :two 666
                       '((:one 111 :two 222 :three 333)
                         "not-pair"
                         (:one 666 :two 666 :three 666)))
      ==>
      '(:one 666 :two 666 :three 666)))
(deftest edit#line-list
    (regular-common-lisp)
  (ensure
      (edit#line-list
       :indent 2
       :prefix "* "
       :postfix "|^-^"
       :function-list
       `(,(lambda (string) (string-trim '(#\space) string)))
       :line-list
       `("  123"
         "456  "))
      ==>
      `("  * 123|^-^"
        "  * 456|^-^")))

;; (edit#line-list
;;  :indent 2
;;  :print-to *standard-output*
;;  :prefix "* "
;;  :postfix "|^-^"
;;  :function-list
;;  `(,(lambda (string) (string-trim '(#\space) string)))
;;  :line-list
;;  `("  123"
;;    "456  "))
