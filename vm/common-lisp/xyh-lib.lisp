(in-package #:xyh-lib)
(defun nil? (x)
  (null x))

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
(defun char#space? (char)
  (let ((code (char-code char)))
    (cond ((= code 32) t)
          ((= code 10) t)
          (:else nil))))

;; (char#space? #\newline)
;; (char#space? #\space)



(defun string->head#word (string)
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

;; (multiple-value-bind
;;       (head#word
;;        index-end
;;        index-start
;;        string)
;;     (string->head#word string)
;;   ><><><)

;; (string->head#word " kkk took my baby away! ")
;; (string->head#word "k")
;; (string->head#word " k")
;; (string->head#word "k ")

;; the argument applied to string->head#word
;; must not be space-string

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



(defun string#space? (string)
  (not (position-if
        (lambda (char) (not (char#space? char)))
        string)))

;; (string#space? " 123 ")
;; (string#space? "  ")
;; (string#space? "")



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
