(in-package #:threaded-code-interpreter)
(defun nil? (x)
  (null x))

(defun integer? (x)
  (integerp x))

(defun natural-number? (x)
  (and (integerp x)
       (<= 0 x)))

;; (natural-number? 0)
;; (natural-number? 1)
;; (natural-number? -1)
;; (natural-number? 1.1)


(defun array? (x)
  (arrayp x))
(defun add1 (x)
  (+ x 1))

(defun sub1 (x)
  (- x 1))
(defun read#line (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof))
  (read-line from eof-as-error? read-eof-as))

(defun read#char (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof))
  (read-char from eof-as-error? read-eof-as))

(defun newline (&key (many 1))
  (cond ((= 0 many) :nothing)
        ((= 1 many) (format t "~%"))
        ((< 1 many) (format t "~%")
         (newline :many (sub1 many)))
        (:else :nothing)))
(defun char->code (char)
  (char-code char))

(defun code->char (code)
  (code-char code))
(defun char#space? (char)
  (let ((code (char->code char)))
    (cond ((= code 32) t)
          ((= code 10) t)
          (:else nil))))

;; (char#space? #\newline)
;; (char#space? #\space)

(defun string#empty? (string)
  (equal string ""))

(defun string#space? (string)
  (not (position-if
        (lambda (char) (not (char#space? char)))
        string)))

;; (string#space? " 123 ")
;; (string#space? "  ")
;; (string#space? "")
(defun string->head#word (string)
  ;; interface:
  ;; (multiple-value-bind
  ;;       (head#word
  ;;        index-end
  ;;        index-start
  ;;        string)
  ;;     (string->head#word string)
  ;;   ><><><)
  (let* ((index-start (position-if
                       (lambda (char) (not (char#space? char)))
                       string))
         (index-end (position-if
                     (lambda (char) (char#space? char))
                     string
                     :start index-start)))
    (values (subseq string
                    index-start
                    index-end)
            index-end
            index-start
            string)))

;; (string->head#word " kkk took my baby away! ")
;; (string->head#word "k")
;; (string->head#word " k")
;; (string->head#word "k ")

;; the argument applied to string->head#word
;; must not be space-string
;; one should use string#space? to ensure this

;; just do not handle the error
;; let the debuger do its job
;; (string->head#word " ")



(defun string->tail#word (string)
  (multiple-value-bind
        (head#word
         index-end
         index-start
         string)
      (string->head#word string)
    (if (nil? index-end)
        ""
        (subseq string index-end))))

;; (string->tail#word " kkk took my baby away! ")




(defun string->list#word (string &key (base-list '()))
  (cond
    ((string#space? string) base-list)
    (:else
     (cons (string->head#word string)
           (string->list#word (string->tail#word string))))))

;; (string->list#word " kkk took my baby away! ")
;; (string->list#word " kkk")
;; (string->list#word "kkk ")
;; (string->list#word " ")
;; (string->list#word "")
(defun string->head#char (string)
  ;; interface:
  ;; (multiple-value-bind
  ;;       (head#char
  ;;        tail#char
  ;;        string)
  ;;     (string->head#char string)
  ;;   ><><><)
  (values (char string 0)
          (subseq string
                  1)
          string))

;; (string->head#char " kkk took my baby away! ")
;; (string->head#char "k")
;; (string->head#char " k")
;; (string->head#char "k ")

;; the argument applied to string->head#char
;; must not be ""
;; one should use string#empty? to ensure this

;; just do not handle the error
;; let the debuger do its job
;; (string->head#char "")



(defun string->tail#char (string)
  (multiple-value-bind
        (head#char
         tail#char
         string)
      (string->head#char string)
    tail#char))

;; (string->tail#char " kkk took my baby away! ")
;; (string->tail#char "")



(defun string->list#char (string &key (base-list '()))
  (cond
    ((string#empty? string) base-list)
    (:else
     (cons (string->head#char string)
           (string->list#char (string->tail#char string))))))

;; (string->list#char " kkk took my baby away! ")
;; (string->list#char " kkk")
;; (string->list#char "kkk ")
;; (string->list#char " ")
;; (string->list#char "")
(defun shift#left (&key
                     (step 1)
                     number)
  (* number
     (expt 2 step)))

;; (shift#left
;;  :step 2
;;  :number 10)
;; (shift#left
;;  :number 10)


(defun shift#right (&key
                      (step 1)
                      number)
  (/ number
     (expt 2 step)))

;; (shift#right
;;  :step 2
;;  :number 64)
;; (shift#right
;;  :number 64)
(defun group (list
              &key
                (number 2)
                ;; (pattern '())
                (base-list '()))
  (cond ((< (length list) 2) base-list)
        (:else
         (cons (list (first list) (second list))
               (group (cddr list)
                      :number number)))))
;; (defun help#group ())
;; must be a prime number

;; 1000003  ;; about 976 k
;; 1000033
;; 1000333
;; 100003   ;; about 97 k
;; 100333   
;; 997
;; 499
;; 230      ;; for a special test

(defparameter *size#name-table*
  100333)

(defparameter *size#entry#name-table*
  100)

(defparameter *name-table*
  (make-array
   (list *size#name-table* *size#entry#name-table*)
   :initial-element nil))
(defparameter *max-carry-position* 22)

(defun string->natural-number (string
                               &key
                                 (counter 0)
                                 (sum 0))
  (if (string#empty? string)
      sum
      (multiple-value-bind
            (head#char
             tail#char
             string)
          (string->head#char string)
        (string->natural-number
         tail#char
         :counter (if (< counter *max-carry-position*)
                      (add1 counter)
                      0)
         :sum (+ sum
                 (shift#left
                  :step counter
                  :number (char->code head#char)))))))

;; (string->natural-number "")
;; (string->natural-number "@")
;; (string->natural-number "@@@")
(defun natural-number->index (natural-number)
  (if (not (natural-number? natural-number))
      (error "argument of natural-number->index must be a natural-number")
      (mod natural-number *size#name-table*)))

;; (natural-number->index 0)
;; (natural-number->index 123)
;; (natural-number->index 123.123)
(defun name? (x)
  (and (array? x)
       (= 1 (array-rank x))
       (= 2 (array-dimension x
                             0))
       (equal :<name>
              (aref x
                    0))))

(name? #(:<name> "123"))

(defun print-name (name
                   &key (stream t))
  (format stream
          "#name: ~A"
          '><))


(defun help#string->symbol#find-old-or-creat-new
    ())

(defun string->name (string)
  (let ((index
         (natural-number->index
          (string->natural-number string))))
    ()
    ))


(defun symbol->string (index)
  ())

