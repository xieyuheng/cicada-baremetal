(in-package :cicada-vm)
(be :title (string->title "vector-function-heap")
    :name (string->name "current-free-address")
    :title#object (string->title "fixnum")
    :value#object 0)
(defun fetch#vector-function-body ())
(defun save#vector-function-body ())
(defun $source->section!
    (&key
       source-name
       section-name)
  (let* ((string (file->string :filename source-name))
         (byte-vector ($string->byte-vector :string string)))
    (byte-vector->file! :filename section-name
                        :byte-vector byte-vector)))
(defparameter *string$string->byte-vector* "")
(defparameter *cursor$string->byte-vector* 0)

(defparameter *byte-vector$string->byte-vector*
  (make#vector :length (mul 1024 1024)
               :element-type '(unsigned-byte 8)
               :initial-element 0))
(defparameter *current-free-address$string->byte-vector* 0)


(defun $string->byte-vector
    (&key
       string)
  (set! *string$string->byte-vector* string)
  (set! *cursor$string->byte-vector* 0)
  (set! *current-free-address$string->byte-vector* 0)
  (help$string->byte-vector))

(defun help$string->byte-vector ()
  (let ((next-word* (next-word*!
                     :string *string$string->byte-vector*
                     :cursor *cursor$string->byte-vector*)))
    (cond ((equal? ":" next-word*)
           (let* ((start-index *cursor$string->byte-vector*)
                  (end-index (let ((find-cursor
                                    (find-word!
                                     :word ";"
                                     :string *string$string->byte-vector*
                                     :cursor *cursor$string->byte-vector*)))
                               (if (nil? find-cursor)
                                   (orz ()
                                     ("when calling ($string->byte-vector)~%")
                                     (": and ; as bar-ket must be balanced~%")))))
                  (string (subseq *string$string->byte-vector*
                                  start-index
                                  end-index))
                  (next-word*-1 (progn (next-word*!
                                        ;; over ";"
                                        :string *string$string->byte-vector*
                                        :cursor *cursor$string->byte-vector*)
                                       (next-word*!
                                        :string *string$string->byte-vector*
                                        :cursor *cursor$string->byte-vector*)))
                  (next-word*-2 (next-word*!
                                 :string *string$string->byte-vector*
                                 :cursor *cursor$string->byte-vector*))
                  (next-word*-3 (next-word*!
                                 :string *string$string->byte-vector*
                                 :cursor *cursor$string->byte-vector*)))
             (cond ((and (equal? "(" next-word*-1)
                         (equal? "define-function" next-word*-2)
                         (equal? ")" next-word*-3))
                    ($define-function :string string))
                   (:else
                    (orz ()
                      ("when calling ($string->byte-vector)~%")
                      ("the word after ; must be (define-function) but not ~A~A~A ~%"
                       next-word*-1 next-word*-2 next-word*-3)))))
           (help$string->byte-vector))
          ((equal? :no-more-word next-word*)
           (subseq *byte-vector$string->byte-vector*
                   0
                   *current-free-address$string->byte-vector*))
          (:else
           (orz ()
             ("when calling ($string->byte-vector)~%")
             ("the first word must be : but not ~A ~%" next-word*))))))
(defparameter *string$define-function* "")
(defparameter *cursor$define-function* 0)


(defun $define-function
    (&key
       string)
  (set! *string$define-function* string)
  (set! *cursor$define-function* 0)
  (help$define-function))

(defun help$define-function ()
  (let ((next-word* (next-word*!
                     :string *string$define-function*
                     :cursor *cursor$define-function*)))
    (cond ((equal? "(" next-word*)
           ($function-call))
          ((equal? :no-more-word next-word*)
           :help$define-function--ok)
          (:else
           (orz ()
             ("when calling ($define-function)~%")
             ("the word in the body must be a function call but not ~A ~%" next-word*))))))
(defun $function-call ()
  (let ((next-word* (next-word*!
                     :string *string$define-function*
                     :cursor *cursor$define-function*)))
    (cond ((equal? "<" next-word*)
           (let* ((next-word*-1 (next-word*!
                                 :string *string$define-function*
                                 :cursor *cursor$define-function*))
                  (next-word*-2 (next-word*!
                                 :string *string$define-function*
                                 :cursor *cursor$define-function*))
                  (next-word*-3 (next-word*!
                                 :string *string$define-function*
                                 :cursor *cursor$define-function*))
                  (next-word*-4 (next-word*!
                                 :string *string$define-function*
                                 :cursor *cursor$define-function*))
                  (function-title next-word*-1)
                  (function-name next-word*-3))
             (cond ((not (equal? ">" next-word*-2))
                    (orz ()
                      ("when calling ($define-function)~%")
                      ("when calling ($function-call)~%")
                      ("un-handled syntax inside <>~%")
                      ("as follow: ~%~A" *string$define-function*)))
                   ((not (equal? ")" next-word*-4))
                    (orz ()
                      ("when calling ($define-function)~%")
                      ("when calling ($function-call)~%")
                      ("un-handled syntax inside ()~%")
                      ("as follow: ~%~A" *string$define-function*)))
                   (:else
                    (with (ask :title (string->title "primitive-function")
                               :name  (string->name  "call"))
                      (save#cicada-object-vector
                       :cicada-object-vector *byte-vector$string->byte-vector*
                       :address *current-free-address$string->byte-vector*
                       :title .title
                       :value .value))
                    (set! *current-free-address$string->byte-vector*
                        (add *current-free-address$string->byte-vector*
                             *cicada-object-size*))
                    (with (ask :title (string->title function-title)
                               :name  (string->name  function-name))
                      (save#cicada-object-vector
                       :cicada-object-vector *byte-vector$string->byte-vector*
                       :address *current-free-address$string->byte-vector*
                       :title .title
                       :value .value))
                    (set! *current-free-address$string->byte-vector*
                        (add *current-free-address$string->byte-vector*
                             *cicada-object-size*))))))
          ((equal? :no-more-word next-word*)
           (orz ()
             ("when calling ($define-function)~%")
             ("when calling ($function-call)~%")
             ("the () is un- balanced~%")
             ("too few )~%")
             ("as follow: ~%~A" *string$define-function*)))
          (:else
           (orz ()
             ("when calling ($define-function)~%")
             ("when calling ($function-call)~%")
             ("un-handled syntax inside ()~%")
             ("as follow: ~%~A" *string$define-function*))))))
