(in-package :cicada-vm)
(defparameter *string#cicada-compiler* "")
(defparameter *cursor#cicada-compiler* 0)


(defparameter *string#define-function* "")
(defparameter *cursor#define-function* 0)


;; the following function is to be used in (: cicada :)
(defun cicada-compiler (string)
  (set! *string#cicada-compiler* string)
  (set! *cursor#cicada-compiler* 0)
  (help#cicada-compiler))

(defun help#cicada-compiler ()
  (let ((next-word (next-word!
                    :string *string#cicada-compiler*
                    :cursor *cursor#cicada-compiler*)))
    (cond ((equal? ":" next-word)
           (let* ((start-index *cursor#cicada-compiler*)
                  (end-index (let ((find-cursor
                                    (find-word!
                                     :word ";"
                                     :string *string#cicada-compiler*
                                     :cursor *cursor#cicada-compiler*)))
                               (if (nil? find-cursor)
                                   (orz ()
                                     ("when calling (cicada-compiler)~%")
                                     (": and ; as bar-ket must be balance~%")))))
                  (string (subseq string
                                  start-index
                                  end-index))
                  (next-word (progn (next-word!
                                     ;; over ";"
                                     :string *string#cicada-compiler*
                                     :cursor *cursor#cicada-compiler*)
                                    (next-word!
                                     ;; the next-word to return
                                     :string *string#cicada-compiler*
                                     :cursor *cursor#cicada-compiler*))))
             (cond ((equal? "(define-function)" next-word)
                    (define-function string))
                   (:else
                    (orz ()
                      ("when calling (cicada-compiler)~%")
                      ("the word after ; must be (define-function) but not ~A ~%" next-word)))))
           (help#cicada-compiler))
          ((equal? :no-more-word next-word)
           :ok)
          (:else
           (orz ()
             ("when calling (cicada-compiler)~%")
             ("the first word must be : but not ~A ~%" next-word))))))
(defparameter *buffer#define-function*
  (make#vector :length (mul 1024 1024)
               :element-type '(unsigned-byte 8)
               :initial-element 0))

(defparameter *current-free-address#buffer#define-function* 0)

(defun define-function (string)
  (set! *string#define-function* string)
  (set! *cursor#define-function* 0)
  (help#define-function))

(defun help#define-function ()
  (let ((next-word (next-word!
                    :string *string#define-function*
                    :cursor *cursor#define-function*)))
    (cond ((function-call?#word next-word)
           (let ((function-name
                  (function-call->function-name#word next-word)))
             ()))
          ((equal? :no-more-word next-word)
           :ok)
          (:else
           (orz ()
             ("when calling (define-function)~%")
             ("the word in the body must be a function call but not ~A ~%" next-word))))))

(defun function-call?#word (word)
  (and (equal? "(" (string->head#char word))
       (equal? ")" (string->end#char word))))

(defun function-call->function-name#word (word)
  (subseq word 1 (sub1 (length word))))
(defun fetch#vector-function-body ())
(defun save#vector-function-body ())
