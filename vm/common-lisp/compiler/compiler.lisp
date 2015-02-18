(in-package :cicada-vm)
(be :title (string->title "vector-function-heap")
    :name (string->name "current-free-address")
    :title#object (string->title "fixnum")
    :value#object 0)
(defun fetch#vector-function-body ())
(defun save#vector-function-body ())
(defparameter *string#cicada-compiler* "")
(defparameter *cursor#cicada-compiler* 0)


(defparameter *string#define-function* "")
(defparameter *cursor#define-function* 0)


;; the following function is to be used in (: cicada-compiler :)
(defun cicada-compiler (string)
  (set! *string#cicada-compiler* string)
  (set! *cursor#cicada-compiler* 0)
  (help#cicada-compiler))

(defun help#cicada-compiler ()
  (let ((next-word* (next-word*!
                     :string *string#cicada-compiler*
                     :cursor *cursor#cicada-compiler*)))
    (cond ((equal? ":" next-word*)
           (let* ((start-index *cursor#cicada-compiler*)
                  (end-index (let ((find-cursor
                                    (find-word!
                                     :word ";"
                                     :string *string#cicada-compiler*
                                     :cursor *cursor#cicada-compiler*)))
                               (if (nil? find-cursor)
                                   (orz ()
                                     ("when calling (cicada-compiler)~%")
                                     (": and ; as bar-ket must be balanced~%")))))
                  (string (subseq *string#cicada-compiler*
                                  start-index
                                  end-index))
                  (next-word*-1 (progn (next-word*!
                                        ;; over ";"
                                        :string *string#cicada-compiler*
                                        :cursor *cursor#cicada-compiler*)
                                       (next-word*!                              
                                        :string *string#cicada-compiler*
                                        :cursor *cursor#cicada-compiler*)))
                  (next-word*-2 (next-word*!
                                 :string *string#cicada-compiler*
                                 :cursor *cursor#cicada-compiler*))
                  (next-word*-3 (next-word*!
                                 :string *string#cicada-compiler*
                                 :cursor *cursor#cicada-compiler*)))
             (cond ((and (equal? "(" next-word*-1)
                         (equal? "define-function" next-word*-2)
                         (equal? ")" next-word*-3))
                    (define-function string))
                   (:else
                    (orz ()
                      ("when calling (cicada-compiler)~%")
                      ("the word after ; must be (define-function) but not ~A~A~A ~%"
                       next-word*-1 next-word*-2 next-word*-3)))))
           (help#cicada-compiler))
          ((equal? :no-more-word next-word*)
           :ok)
          (:else
           (orz ()
             ("when calling (cicada-compiler)~%")
             ("the first word must be : but not ~A ~%" next-word*))))))
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
  (let ((next-word* (next-word*!
                     :string *string#define-function*
                     :cursor *cursor#define-function*)))
    (cond ((equal? "(" next-word*)
           (help#function-call#define-function))
          ((equal? :no-more-word next-word*)
           :ok)
          (:else
           (orz ()
             ("when calling (define-function)~%")
             ("the word in the body must be a function call but not ~A ~%" next-word*))))))

(defun help#function-call#define-function ()
  (let ((next-word* (next-word*!
                     :string *string#define-function*
                     :cursor *cursor#define-function*)))
    (cond ((equal? "<" next-word*)           
           (let* ((next-word*-1 (next-word*!
                                 :string *string#define-function*
                                 :cursor *cursor#define-function*))
                  (next-word*-2 (next-word*!
                                 :string *string#define-function*
                                 :cursor *cursor#define-function*))
                  (next-word*-3 (next-word*!
                                 :string *string#define-function*
                                 :cursor *cursor#define-function*))
                  (next-word*-4 (next-word*!
                                 :string *string#define-function*
                                 :cursor *cursor#define-function*))
                  (function-title next-word*-1)
                  (function-name next-word*-3)
                  (current-free-address
                   (with (ask :title (string->title "vector-function-heap")
                              :name (string->name "current-free-address"))
                     .value)))
             (cond ((not (equal? ">" next-word*-2))
                    (orz ()
                      ("when calling (define-function)~%")
                      ("when calling (help#function-call#define-function)~%")
                      ("un-handled syntax inside <>~%")
                      ("as follow: ~%~A" *string#define-function*)))
                   ((not (equal? ")" next-word*-4))
                    (orz ()
                      ("when calling (define-function)~%")
                      ("when calling (help#function-call#define-function)~%")
                      ("un-handled syntax inside ()~%")
                      ("as follow: ~%~A" *string#define-function*)))                   
                   (:else
                    (with (ask :title (string->title "primitive-function")
                               :name  (string->name  "call"))
                      (save#cicada-section
                       :section-name "vector-function-heap"
                       :address current-free-address
                       :title .title ;; (string->title "primitive-instruction")
                       :value .value))
                    (set! current-free-address
                        (add current-free-address *cicada-object-size*))
                    (with (ask :title (string->title function-title)
                               :name  (string->name  function-name))
                      (save#cicada-section
                       :section-name "vector-function-heap"
                       :address current-free-address
                       ;; (execute-next-instruction) does not check the following title
                       :title .title ;; (string->title "primitive-instruction-address")
                       :value .value))
                    (set! current-free-address
                        (add current-free-address *cicada-object-size*))))))
          ((equal? :no-more-word next-word*)
           (orz ()
             ("when calling (define-function)~%")
             ("when calling (help#function-call#define-function)~%")
             ("the () is un- balanced~%")
             ("too few )~%")
             ("as follow: ~%~A" *string#define-function*)))
          (:else
           (orz ()
             ("when calling (define-function)~%")
             ("when calling (help#function-call#define-function)~%")
             ("un-handled syntax inside ()~%")
             ("as follow: ~%~A" *string#define-function*))))))
