(in-package :cicada-vm)
(defun fetch#vector-function-body ())
(defun save#vector-function-body ())
(defun $iaa->ccd!
    (&key
       iaa
       ccd)
  (let* ((string (file->string :filename iaa))
         (byte-vector ($string->byte-vector :string string)))
    (byte-vector->file! :filename ccd
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
  (loop$byte-vector))

(defun loop$byte-vector ()
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
                                     (": and ; as bar-ket must be balanced~%"))
                                   find-cursor)))
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
           (loop$byte-vector))
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
  (loop$function-head)
  (loop$function-body))
(defun loop$function-head ()
  )
(defun loop$function-body ()
  (let ((next-word* (next-word*!
                     :string *string$define-function*
                     :cursor *cursor$define-function*)))
    (cond ((equal? "(" next-word*)
           ($function-call)
           (loop$function-body))
          ((equal? :no-more-word next-word*)
           :loop$function-body--ok)
          (:else
           (orz ()
             ("when calling ($define-function)~%")
             ("the word in the body must be a function call but not ~A ~%" next-word*))))))
(defun $save-object
    (&key
       title
       name)
  (with (ask :title title
             :name  name)
    (save#cicada-object-vector
     :cicada-object-vector *byte-vector$string->byte-vector*
     :address *current-free-address$string->byte-vector*
     :title .title
     :value .value))
  (set! *current-free-address$string->byte-vector*
      (add *current-free-address$string->byte-vector*
           *cicada-object-size*)))
(defun $function-call ()
  (let ((next-word* (next-word*!
                     :string *string$define-function*
                     :cursor *cursor$define-function*)))
    (cond ((| string <a> ? | next-word*)
           (let* ((function-title (| string <a> -> a | next-word*))
                  (function-name
                   (next-word*!
                    :string *string$define-function*
                    :cursor *cursor$define-function*))
                  (end-ket
                   (next-word*!
                    :string *string$define-function*
                    :cursor *cursor$define-function*)))
             (cond ((not (equal? ")" end-ket))
                    (orz ()
                      ("when calling ($define-function)~%")
                      ("when calling ($function-call)~%")
                      ("un-handled syntax inside ()~%")
                      ("as follow: ~%~A" *string$define-function*)))
                   (:else
                    ($save-object
                     :title (string->title "primitive-function")
                     :name  (string->name  "call"))
                    ($save-object
                     :title (string->title function-title)
                     :name  (string->name  function-name))))))
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
