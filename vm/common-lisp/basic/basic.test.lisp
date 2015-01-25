(in-package :cicada-vm)
(deftest natural-number?
    (basic)
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
    (basic)
  (ensure
      (list (shift#left :number 10)
            (shift#left :step 2
                        :number 10))
      ==>
      (list 20
            40)))

(deftest shift#right
    (basic)
  (ensure
      (list (shift#right :number 64)
            (shift#right :step 2
                         :number 64))
      ==>
      (list 32
            16)))
(deftest fetch#bits
    (basic)
  (ensure
      (fetch#bits :bits #b0010
                  :size 1
                  :index 1)
      ==>
      1))
(deftest fetch#bytes
    (basic)
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
    (basic)
  (ensure
      (fetch#array
       :array (make-array '(1 1 1) :initial-element 666)
       :index-vector '#(0 0 0))
      ==>
      666))

(deftest save#array
    (basic)
  (ensure
      (fetch#array
       :array (save#array
               :value 258
               :array (make-array '(1 1 1) :initial-element 666)
               :index-vector '#(0 0 0))
       :index-vector '#(0 0 0))
      ==>
      258))
(deftest fetch#byte-array
    (basic)
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
    (basic)
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
    (basic)
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
    (basic)
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
    (basic)
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
    (basic)
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
    (basic)
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
    (basic)
  (ensure
      (list (char#space? #\newline)
            (char#space? #\space))
      ==>
      (list t
            t)))

(deftest string#space?
    (basic)
  (ensure
      (list (string#space? " 123 ")
            (string#space? "  ")
            (string#space? ""))
      ==>
      (list nil
            t
            t)))
(deftest string->head#word
    (basic)
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
    (basic)
  (ensure
      (list (string->tail#word " kkk took my baby away! ")
            (string->tail#word "just-kkk"))
      ==>
      (list " took my baby away! "
            nil)))


(deftest string->list#word
    (basic)
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
    (basic)
  (ensure
      (list (string->head#line "123")
            (string->head#line (format nil "~%123"))
            (string->head#line (format nil "123~%")))
      ==>
      `("123"
        ""
        "123")))


(deftest string->tail#line
    (basic)
  (ensure
      (list (string->tail#line "123")
            (string->tail#line (format nil "~%123"))
            (string->tail#line (format nil "123~%")))
      ==>
      `(nil
        "123"
        "")))


(deftest string->list#line
    (basic)
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
    (basic)
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
    (basic)
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
    (basic)
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
(defun pair? (x)
  (consp x))

(defun list? (x)
  (listp x))
(deftest end-of-list
    (basic)
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
(deftest edit#line-list
    (basic)
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
