(in-package :cicada-vm)
(defmacro set! (variable-name value)
  `(setf ,variable-name ,value))
(defun nil? (x)
  (null x))

(defun ture? (x)
  (eq t x))

(defun false? (x)
  (eq nil x))
(defun eq? (x y)
  (eq x y))

(defun equal? (x y)
  (equal x y))
(defparameter true t)
(defparameter false nil)
(defparameter *size#fixnum* 32) ;; unit bit

(defun fixnum? (x)
  (and (natural-number? x)
       (< x
          (expt 2 *size#fixnum*))))

(defun one? (x)
  (and (integerp x)
       (= 1 x)))

(defun zero? (x)
  (and (integerp x)
       (zerop x)))

(defun integer? (x)
  (integerp x))

(defun natural-number? (x)
  (and (integerp x)
       (<= 0 x)))
(defmacro add (&body body) `(+ ,@body))
(defun sub (x y) (- x y))
(defmacro mul (&body body) `(* ,@body))
(defun div (x y) (/ x y))

(defun add1 (x) (+ x 1))
(defun sub1 (x) (- x 1))
(defun add2 (x) (+ x 2))
(defun sub2 (x) (- x 2))

(defmacro add1! (x) `(setf ,x (add1 ,x)))
(defmacro sub1! (x) `(setf ,x (sub1 ,x)))
(defmacro add2! (x) `(setf ,x (add2 ,x)))
(defmacro sub2! (x) `(setf ,x (sub2 ,x)))

(defun neg (x) (- x))

(defun power (x y) (expt x y))
(defun natural-number->keyword (number)
  (intern (format nil "~A" number)
          :keyword))
(defun shift#left (&key
                     (step 1)
                     number)
  (* number
     (expt 2 step)))


(defun shift#right (&key
                      (step 1)
                      number)
  (/ number
     (expt 2 step)))
(defun fetch#bits (&key
                     bits
                     (size 1)
                     index)
  (ldb (byte size index) bits))

(defun save#bits (&key
                    value
                    bits
                    (size 1)
                    index)
  (setf (ldb (byte size index) bits) value)
  (values bits
          value))
(defun fetch#bytes (&key
                      bytes
                      (size 1)
                      index)
  (fetch#bits :bits bytes
              :size (* 8 size)
              :index (* 8 index)))

(defun save#bytes (&key
                     value
                     bytes
                     (size 1)
                     index)
  (save#bits :value value
             :bits bytes
             :size (* 8 size)
             :index (* 8 index)))
(defun array? (x)
  (arrayp x))
(defun fetch#array (&key
                      array
                      index-vector)
  (let ((index-list (vector->list index-vector)))
    (apply (function aref)
           array index-list)))



(defun save#array (&key
                     value
                     array
                     index-vector)
  (let ((index-list (vector->list index-vector)))
    (setf
     (apply #'aref array index-list) value)
    (values array
            value)))
(defun vector? (x)
  (vectorp x))
(defun make#vector
    (&key
       length
       element-type
       initial-element
       initial-contents
       adjustable
       fill-pointer
       displaced-to
       displaced-index-offset)
  (cond ((nil? initial-contents)
         (make-array `(,length)
                     :element-type element-type
                     :initial-element initial-element
                     :adjustable adjustable
                     :fill-pointer fill-pointer
                     :displaced-to displaced-to
                     :displaced-index-offset displaced-index-offset))
        (:else
         (make-array `(,length)
                     :element-type element-type
                     :initial-contents initial-contents
                     :adjustable adjustable
                     :fill-pointer fill-pointer
                     :displaced-to displaced-to
                     :displaced-index-offset displaced-index-offset))))

(defun make#sub-vector (&key vector start end)
  (subseq vector start end))


(defun fetch#vector (&key
                       vector
                       index)
  (fetch#array :array vector
               :index-vector `#(,index)))



(defun save#vector (&key
                      value
                      vector
                      index)
  (save#array :value value
              :array vector
              :index-vector `#(,index)))



(defun copy-vector (vector)
  (if (not (vector? vector))
      (error "the argument of copy-vector must be a vector")
      (copy-seq vector)))
(defun map#vector
    (&key
       function
       vector
       (width 1)
       (offset 0)
       (number nil)
       (base-list '()))
  (let ((length (div (sub (length vector)
                          offset)
                     width)))
    (when (nil? number) (set! number length))
    (help ((defun loop-collect
               (&key
                  (cursor 0))
             (cond ((< cursor number)
                    (let ((value-to-collect
                           (if (equal? width 1)
                               (funcall function
                                 :element (get-element cursor))
                               (funcall function
                                 :sub-vector (get-sub-vector cursor)))))
                      (cons value-to-collect
                            (loop-collect :cursor (add1 cursor)))))
                   (:else
                    base-list))))
      (loop-collect)
      :where
      (defun get-element (cursor)
        (fetch#vector :vector (get-sub-vector cursor)
                      :index 0))
      (defun get-sub-vector (cursor)
        (let ((start (add offset (mul cursor width))))
          (subseq vector
                  start
                  (add start width)))))))
(defun list->vector (list)
  (if (not (list? list))
      (error "the argument of (list->vector) must be a list")
      (coerce list 'vector)))


(defun vector->list (vector)
  (if (not (vector? vector))
      (error "the argument of (vector->list) must be a vector")
      (coerce vector 'list)))
(defun fetch#byte-array
    (&key
       byte-array
       (size 1)
       index-vector
       (endian 'little))

  (cond
    ((not (<= (+ (fetch#vector :vector index-vector
                               :index (sub1 (array-rank byte-array)))
                 size)
              (array-dimension byte-array
                               (sub1 (array-rank byte-array)))))
     (error "the size of the value you wish to fetch is out of the index of the byte-array"))

    ((equal? endian 'little)
     ;; helper function will do side-effect on argument :index-vector
     ;; so copy it first
     (setf index-vector (copy-vector index-vector))
     (help#little-endian#fetch#byte-array
      :byte-array byte-array
      :size size
      :index-vector index-vector))

    ((equal? endian 'big)
     ;; helper function will do side-effect on argument :index-vector
     ;; so copy it first
     (setf index-vector (copy-vector index-vector))
     (help#big-endian#fetch#byte-array
      :byte-array byte-array
      :size size
      :index-vector index-vector))

    (:else
     (error "the argument :endian of (fetch#byte-array) must be 'little or 'big"))
    ))


(defun help#little-endian#fetch#byte-array
    (&key
       byte-array
       size
       index-vector
       (counter 0)
       (sum 0))
  (cond
    ((not (< counter
             size))
     sum)

    (:else
     (let* ((last-index (fetch#vector
                         :vector index-vector
                         :index (sub1 (array-rank byte-array))))
            (value-for-shift (fetch#array
                              :array byte-array
                              :index-vector index-vector))
            (value-for-sum (shift#left
                            :step (* 8 counter)
                            :number value-for-shift)))
       ;; update index-vector
       (save#vector :value (add1 last-index)
                    :vector index-vector
                    :index (sub1 (array-rank byte-array)))
       ;; loop
       (help#little-endian#fetch#byte-array
        :byte-array byte-array
        :size size
        :index-vector index-vector
        :counter (add1 counter)
        :sum (+ sum value-for-sum))))
    ))




;; (add1) change to (sub1)
;; new index-vector-for-fetch
(defun help#big-endian#fetch#byte-array
    (&key
       byte-array
       size
       index-vector
       (counter 0)
       (sum 0))
  (cond
    ((not (< counter
             size))
     sum)

    (:else
     (let* ((last-index (fetch#vector
                         :vector index-vector
                         :index (sub1 (array-rank byte-array))))
            ;; new index-vector-for-fetch
            (index-vector-for-fetch (save#vector
                                     :value (+ last-index
                                               (sub1 size))
                                     :vector (copy-vector index-vector)
                                     :index (sub1 (array-rank byte-array))))
            (value-for-shift (fetch#array
                              :array byte-array
                              :index-vector index-vector-for-fetch))
            (value-for-sum (shift#left
                            :step (* 8 counter)
                            :number value-for-shift)))
       ;; update index-vector
       ;; (add1) change to (sub1)
       (save#vector :value (sub1 last-index)
                    :vector index-vector
                    :index (sub1 (array-rank byte-array)))
       ;; loop
       (help#big-endian#fetch#byte-array
        :byte-array byte-array
        :size size
        :index-vector index-vector
        :counter (add1 counter)
        :sum (+ sum value-for-sum))))
    ))





(defun save#byte-array
    (&key
       value
       byte-array
       (size 1)
       index-vector
       (endian 'little))
  (cond
    ((not (<= (+ (fetch#vector :vector index-vector
                               :index (sub1 (array-rank byte-array)))
                 size)
              (array-dimension byte-array
                               (sub1 (array-rank byte-array)))))
     (error "the size of the value you wish to save is out of the index of the byte-array"))

    ((equal? endian 'little)
     ;; helper function will do side-effect on argument :index-vector
     ;; so copy it first
     (setf index-vector (copy-vector index-vector))
     (help#little-endian#save#byte-array
      :value value
      :byte-array byte-array
      :size size
      :index-vector index-vector))

    ((equal? endian 'big)
     ;; helper function will do side-effect on argument :index-vector
     ;; so copy it first
     (setf index-vector (copy-vector index-vector))
     (help#big-endian#save#byte-array
      :value value
      :byte-array byte-array
      :size size
      :index-vector index-vector))

    (:else
     (error "the argument :endian of (save#byte-array) must be 'little or 'big"))
    ))


(defun help#little-endian#save#byte-array
    (&key
       value
       byte-array
       size
       index-vector
       (counter 0))
  (cond
    ((not (< counter
             size))
     (values byte-array
             value))

    (:else
     (let* ((last-index (fetch#vector
                         :vector index-vector
                         :index (sub1 (array-rank byte-array)))))
       ;; save to byte-array
       (save#array :value (fetch#bytes :bytes value
                                       :size 1
                                       :index counter)
                   :array byte-array
                   :index-vector index-vector)
       ;; update index-vector
       (save#vector :value (add1 last-index)
                    :vector index-vector
                    :index (sub1 (array-rank byte-array)))
       ;; loop
       (help#little-endian#save#byte-array
        :value value
        :byte-array byte-array
        :size size
        :index-vector index-vector
        :counter (add1 counter))))
    ))





;; (add1) change to (sub1)
;; new index-vector-for-save
(defun help#big-endian#save#byte-array
    (&key
       value
       byte-array
       size
       index-vector
       (counter 0))
  (cond
    ((not (< counter
             size))
     (values byte-array
             value))

    (:else
     (let* ((last-index (fetch#vector
                         :vector index-vector
                         :index (sub1 (array-rank byte-array))))
            ;; new index-vector-for-save
            (index-vector-for-save (save#vector
                                    :value (+ last-index
                                              (sub1 size))
                                    :vector (copy-vector index-vector)
                                    :index (sub1 (array-rank byte-array)))))
       ;; save to byte-array
       (save#array :value (fetch#bytes :bytes value
                                       :size 1
                                       :index counter)
                   :array byte-array
                   :index-vector index-vector-for-save)
       ;; update index-vector
       ;; (add1) change to (sub1)
       (save#vector :value (sub1 last-index)
                    :vector index-vector
                    :index (sub1 (array-rank byte-array)))
       ;; loop
       (help#big-endian#save#byte-array
        :value value
        :byte-array byte-array
        :size size
        :index-vector index-vector
        :counter (add1 counter))))
    ))
(defun byte-vector? (x)
  (typep x '(vector (unsigned-byte 8))))
(defun fetch#byte-vector (&key
                            byte-vector
                            (size 1)
                            index
                            (endian 'little))
  (fetch#byte-array :byte-array byte-vector
                    :size size
                    :index-vector `#(,index)
                    :endian endian))



(defun save#byte-vector (&key
                           value
                           byte-vector
                           (size 1)
                           index
                           (endian 'little))
  (save#byte-array :value value
                   :byte-array byte-vector
                   :size size
                   :index-vector `#(,index)
                   :endian endian))


(defun copy#byte-vector (&key
                           from
                           from-index
                           to
                           to-index
                           size
                           (counter 0))
  (cond
    ((not (< counter
             size))
     (values to
             from
             counter))

    (:else
     (save#byte-vector
      :value (fetch#byte-vector
              :byte-vector from
              :size 1
              :index from-index)
      :byte-vector to
      :size 1
      :index to-index)
     (copy#byte-vector :from from
                       :from-index (add1 from-index)
                       :to to
                       :to-index (add1 to-index)
                       :size size
                       :counter (add1 counter)))))
(defun stream? (x)
  (streamp x))
(defun read#char (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof)
                    (recursive-call-to-reader? nil))
  (read-char from
             eof-as-error?
             read-eof-as
             recursive-call-to-reader?))

(defun read#line (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof)
                    (recursive-call-to-reader? nil))
  (read-line from
             eof-as-error?
             read-eof-as
             recursive-call-to-reader?))
;; (cat (:to *standard-output*)
;;   ("~A" 123)
;;   ("~A" 456))
;; ==>
;; (concatenate
;;  'string
;;  (format *standard-output* "~A" 123)
;;  (format *standard-output* "~A" 456))

;; (defmacro cat
;;     ((&key (to nil))
;;      &body form#list-of-list)
;;   (let* ((form#list-of-list#2
;;           (mapcar (lambda (list) (append `(format ,to) list))
;;                   form#list-of-list))
;;          (form#final (append '(concatenate (quote string))
;;                              form#list-of-list#2)))
;;     form#final))



(defmacro cat
    ((&key (to nil)
           (trim '())
           prefix
           postfix
           letter)
     &body form#list-of-list)
  (let* ((form#list-of-list#2
          (apply (function append)
                 (mapcar (lambda (list)
                           (list prefix
                                 (list 'string-trim trim
                                       (append '(format nil) list))
                                 postfix))
                         form#list-of-list)))
         (form#list-of-list#3
          (append '(concatenate (quote string))
                  form#list-of-list#2))
         (form#final
          (cond ((equal letter :big)
                 (list 'string-upcase form#list-of-list#3))
                ((equal letter :small)
                 (list 'string-downcase form#list-of-list#3))
                ((equal letter nil)
                 form#list-of-list#3)
                (:else
                 (error "the argument :letter of (cat) must be :big or :small or nil")))))
    `(let ((string-for-return ,form#final))
       (format ,to "~A" string-for-return)
       string-for-return)))

;; (cat (:to *standard-output*
;;           :trim '(#\Space)
;;           :prefix "* "
;;           :postfix (cat () ("~%")))
;;   ("~A" "      123   ")
;;   ("~A" "   456   "))
(defmacro orz
    ((&key (to nil)
           (trim '())
           prefix
           postfix
           letter)
     &body form#list-of-list)
  `(error (cat (:to ,to
                    :trim ,trim
                    :prefix ,prefix
                    :postfix ,postfix
                    :letter ,letter)
            ,@form#list-of-list)))
(defun file->byte-vector!
    (&key
       filename
       byte-vector
       (start 0)
       (end nil))
  (cond ((not (string? filename))
         (orz ()
           ("the argument :filename of (file->byte-vector!)~%")
           ("must be a string~%")))
        ((not (byte-vector? byte-vector))
         (orz ()
           ("the argument :byte-vector of (file->byte-vector!)~%")
           ("must be a byte-vector~%")))
        (:else
         (let* ((input-stream
                 (open filename
                       :element-type '(unsigned-byte 8)
                       :direction :input))
                (end-address
                 (read-sequence byte-vector
                                input-stream
                                :start start
                                :end end)))           
           (close input-stream)
           ;; return the index of the first byte of the byte-vector that was not updated           
           (values end-address)))))


(defun byte-vector->file!
    (&key
       filename
       byte-vector
       (start 0)
       (end nil))
  (cond ((not (string? filename))
         (orz ()
           ("the argument :filename of (byte-vector->file!)~%")
           ("must be a string~%")))
        ((not (byte-vector? byte-vector))
         (orz ()
           ("the argument :byte-vector of (byte-vector->file!)~%")
           ("must be a byte-vector~%")))
        (:else
         (let* ((output-stream
                 (open filename
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :supersede)))
           (write-sequence byte-vector
                           output-stream
                           :start start
                           :end end)
           (close output-stream)
           (values :byte-vector->file!--ok)))))


;; (defparameter *test-byte-vector*
;;   (make#vector :length 16
;;                :element-type '(unsigned-byte 8)
;;                :initial-element 33))
;; (byte-vector->file! :filename "home:test.org"
;;                     :byte-vector *test-byte-vector*)
;; (file->byte-vector! :filename "home:test.org"
;;                     :byte-vector *test-byte-vector*)
(defun file->string
    (&key
       filename
       (start 0)
       (end nil))
  (let ((char-vector (make#vector :length (mul 1024 1024)
                                  :element-type '(char)
                                  :initial-element *space#char*)))
    (cond ((not (string? filename))
           (orz ()
             ("the argument :filename of (file->string)~%")
             ("must be a string")))
          (:else
           (let* ((input-stream
                   (open filename
                         :direction :input))
                  (end-address
                   (read-sequence char-vector
                                  input-stream
                                  :start start
                                  :end end))
                  (sub-char-vector
                   (subseq char-vector
                           0
                           end-address)))
             (close input-stream)
             (values (coerce sub-char-vector 'string)
                     ;; return the index of the first byte of the char-vector that was not updated
                     ;; as length of the string
                     end-address))))))

(defun string->file!
    (&key
       filename
       string
       (start 0)
       (end nil))
  (cond ((not (string? filename))
         (orz ()
           ("the argument :filename of (string->file!)~%")
           ("must be a string~%")))
        (:else
         (let* ((output-stream
                 (open filename
                       :direction :output
                       :if-exists :supersede)))
           (write-sequence string
                           output-stream
                           :start start
                           :end end)
           (close output-stream)
           (values :string->file!--ok)))))

;; (file->string :filename "home:test.org")
;; (string->file! :filename "home:test.org" :string "666123")
(defun bind-char-to-reader
    (&key
       char
       reader
       (terminating? true)
       (readtable *readtable*))
  (set-macro-character char
                       reader
                       (not terminating?)
                       readtable))

(defun find-reader-from-char
    (&key
       char
       (readtable *readtable*))
  (get-macro-character char readtable))


(defun bind-two-char-to-reader
    (&key
       char1
       char2
       reader
       (readtable *readtable*))
  (set-dispatch-macro-character char1
                                char2
                                reader
                                readtable))

(defun find-reader-from-two-char (char1 char2)
  (get-dispatch-macro-character char1
                                char2
                                readtable))
(defun char? (x)
  (characterp x))

(defun char#space? (char)
  (if (not (char? char))
      (error "the argument of (char#space?) must be a char")
      (let ((code (char->code char)))
        (cond ((= code 32) t)
              ((= code 10) t)
              (:else nil)))))

(defun char#bar-ket? (char)
  (if (not (char? char))
      (error "the argument of (char#bar-ket?) must be a char")
      (or (equal? char #\()
          (equal? char #\))
          (equal? char #\[)
          (equal? char #\])
          (equal? char #\{)
          (equal? char #\})
          ;; <> are not viewed as bar-ket
          ;; for we need to use them in arrow -> & <-
          ;; (equal? char #\<)
          ;; (equal? char #\>)
          )))
(defun char->code (char)
  (char-code char))

(defun code->char (code)
  (code-char code))
(defparameter *bar#square#string* "[")
(defparameter *bar#square#char* (character *bar#square#string*))
(defparameter *ket#square#string* "]")
(defparameter *ket#square#char* (character *ket#square#string*))

(defparameter *bar#round#string* "(")
(defparameter *bar#round#char* (character *bar#round#string*))
(defparameter *ket#round#string* ")")
(defparameter *ket#round#char* (character *ket#round#string*))

(defparameter *bar#flower#string* "{")
(defparameter *bar#flower#char* (character *bar#flower#string*))
(defparameter *ket#flower#string* "}")
(defparameter *ket#flower#char* (character *ket#flower#string*))

(defparameter *space#string* " ")
(defparameter *space#char* (character *space#string*))

(defparameter *sharp#string* "#")
(defparameter *sharp#char* (character *sharp#string*))
(defun symbol? (x)
  (symbolp x))
(defun symbol->string (symbol)
  (symbol-name symbol))

(defun string->symbol (string)
  (intern string))
(defmacro put (symbol field-symbol value)
  `(setf (get ,symbol ,field-symbol) ,value))
(defun string? (x)
  (stringp x))

(defun string#empty? (string)
  (equal? string ""))

(defun string#space? (string)
  (if (not (string? string))
      (error "the argument of (string#space?) must be a string")
      (not (position-if
            (lambda (char) (not (char#space? char)))
            string))))
(defun make#string (&key
                      length
                      (initial-element *space#char*)
                      (element-type 'character))
  (make-string length
               :initial-element initial-element
               :element-type element-type))

(defun make#sub-string (&key string start end)
  (subseq string start end))


(defun fetch#string (&key
                       string
                       index)
  (fetch#vector :vector string
                :index index))


(defun save#string (&key
                      value
                      string
                      index)
  (save#vector :value value
               :vector string
               :index index))
(defun dup#string
    (&key
       (time 1)
       string)
  (if (not (string? string))
      (orz ()
        ("the argument :string of (dup#string) must be a string~%"))
      (help#dup#string :time time
                       :string string)))

(defun help#dup#string
    (&key
       time
       string)  
  (cond ((= 1 time)
         string)
        (:else
         (concatenate
          'string
          string
          (help#dup#string :time (sub1 time)
                           :string string)))))
;; interface:
;; (multiple-value-bind
;;        (head#word
;;         index-end-or-nil
;;         index-start
;;         string)
;;      (string->head#word string)
;;    ><><><)

(defun string->head#word (string)
  (let* ((index-start
          (position-if (lambda (char) (not (char#space? char)))
                       string))
         (index-end-or-nil
          (position-if (lambda (char) (char#space? char))
                       string
                       :start index-start)))
    (values (subseq string
                    index-start
                    index-end-or-nil)
            index-end-or-nil
            index-start
            string)))


(defun string->tail#word (string)
  (multiple-value-bind
        (head#word
         index-end-or-nil
         index-start
         string)
      (string->head#word string)
    (if (nil? index-end-or-nil)
        nil
        (subseq string index-end-or-nil))))


(defun string->list#word (string &key (base-list '()))
  (cond
    ((nil? string) base-list)
    ((string#space? string) base-list)
    (:else
     (cons (string->head#word string)
           (string->list#word (string->tail#word string))))))
;; interface:
;; (multiple-value-bind
;;       (head#line
;;        index-end-or-nil
;;        string)
;;     (string->head#line string)
;;   ><><><)

(defun string->head#line (string)
  (let* ((index-end-or-nil
          (position-if (lambda (char) (equal? #\Newline char))
                       string)))
    (values (subseq string
                    0
                    index-end-or-nil)
            index-end-or-nil
            string)))


(defun string->tail#line (string)
  (multiple-value-bind
        (head#line
         index-end-or-nil
         string)
      (string->head#line string)
    (if (nil? index-end-or-nil)
        nil
        (subseq string (add1 index-end-or-nil)))))


(defun string->list#line (string &key (base-list '()))
  (cond
    ((nil? string) base-list)
    (:else
     (cons (string->head#line string)
           (string->list#line (string->tail#line string))))))
;; interface:
;; (multiple-value-bind
;;       (head#char
;;        tail#char
;;        string)
;;     (string->head#char string)
;;   ><><><)

(defun string->head#char (string)
  (values (char string 0)
          (subseq string
                  1)
          string))

(defun string->end#char (string)
  (let ((length (length string)))
    (values (char string (sub1 length))
            length
            string)))

(defun string->tail#char (string)
  (multiple-value-bind
        (head#char
         tail#char
         string)
      (string->head#char string)
    tail#char))


(defun string->list#char (string &key (base-list '()))
  (cond
    ((string#empty? string) base-list)
    (:else
     (cons (string->head#char string)
           (string->list#char (string->tail#char string))))))
(defun next-word
    (&key
       string
       cursor)
  (let ((index-start
         (position-if (lambda (char) (not (char#space? char)))
                      string
                      :start cursor)))
    (cond ((nil? index-start)
           (values :no-more-word
                   cursor))
          (:else
           (let* ((index-end-or-nil
                   (position-if (lambda (char) (char#space? char))
                                string
                                :start index-start))
                  (index-end (if (nil? index-end-or-nil)
                                 (length string)
                                 index-end-or-nil)))
             (values (subseq string
                             index-start
                             index-end)
                     index-end))))))

(defmacro next-word!
    (&key
       string
       cursor)
  `(multiple-value-bind (next-word next-cursor)
       (next-word :string ,string
                  :cursor ,cursor)
     (set! ,cursor next-cursor)
     next-word))



(defun back-word
    (&key
       string
       cursor)
  (let ((pre-index-start
         (position-if (lambda (char) (not (char#space? char)))
                      string
                      :end cursor
                      :from-end t)))
    (cond ((nil? pre-index-start)
           (values :no-more-word
                   cursor))
          (:else
           (let* ((index-start (add1 pre-index-start))
                  (pre-index-end-or-nil
                   (position-if (lambda (char) (char#space? char))
                                string
                                :end index-start
                                :from-end t))
                  (index-end (if (nil? pre-index-end-or-nil)
                                 0
                                 (add1 pre-index-end-or-nil))))
             (values (subseq string
                             index-end
                             index-start)
                     index-end))))))

(defmacro back-word!
    (&key
       string
       cursor)
  `(multiple-value-bind (back-word back-cursor)
       (back-word :string ,string
                  :cursor ,cursor)
     (set! ,cursor back-cursor)
     back-word))



(defun find-word
    (&key
       word
       string
       cursor)
  (multiple-value-bind (next-word next-cursor)
      (next-word :string string
                 :cursor cursor)
   (cond ((equal? next-word word)
          (multiple-value-bind (back-word back-cursor)
              (back-word :string string
                         :cursor next-cursor)
            back-cursor))
         ((equal? next-word :no-more-word)
          nil)
         (:else
          (find-word :word word
                     :string string
                     :cursor next-cursor)))))

(defmacro find-word!
    (&key
       word
       string
       cursor)
  `(let ((find-cursor (find-word :word ,word
                                 :string ,string
                                 :cursor ,cursor)))
     (if (nil? find-cursor)
         nil
         (progn
           (set! ,cursor find-cursor)
           find-cursor))))
(defun next-word*
    (&key
       string
       cursor)
  (let ((index-start
         (position-if (lambda (char) (not (char#space? char)))
                      string
                      :start cursor)))
    (cond ((nil? index-start)
           (values :no-more-word
                   cursor))
          ((char#bar-ket? (char string index-start))
           (let ((index-end (add1 index-start)))
             (values (subseq string
                             index-start
                             index-end)
                     index-end)))
          (:else
           (let* ((index-end-or-nil
                   (position-if (lambda (char) (or (char#space? char)
                                                   (char#bar-ket? char)))
                                string
                                :start index-start))
                  (index-end (if (nil? index-end-or-nil)
                                 (length string)
                                 index-end-or-nil)))
             (values (subseq string
                             index-start
                             index-end)
                     index-end))))))

(defmacro next-word*!
    (&key
       string
       cursor)
  `(multiple-value-bind (next-word* next-cursor)
       (next-word* :string ,string
                   :cursor ,cursor)
     (set! ,cursor next-cursor)
     next-word*))



(defun back-word*
    (&key
       string
       cursor)
  (let ((pre-index-start
         (position-if (lambda (char) (not (char#space? char)))
                      string
                      :end cursor
                      :from-end t)))
    (cond ((nil? pre-index-start)
           (values :no-more-word
                   cursor))
          ((char#bar-ket? (char string pre-index-start))
           (let* ((index-start (add1 pre-index-start))
                  (index-end pre-index-start))
             (values (subseq string
                             index-end
                             index-start)
                     index-end)))
          (:else
           (let* ((index-start (add1 pre-index-start))
                  (pre-index-end-or-nil
                   (position-if (lambda (char) (or (char#space? char)
                                                   (char#bar-ket? char)))
                                string
                                :end index-start
                                :from-end t))
                  (index-end (if (nil? pre-index-end-or-nil)
                                 0
                                 (add1 pre-index-end-or-nil))))
             (values (subseq string
                             index-end
                             index-start)
                     index-end))))))

(defmacro back-word*!
    (&key
       string
       cursor)
  `(multiple-value-bind (back-word* back-cursor)
       (back-word* :string ,string
                   :cursor ,cursor)
     (set! ,cursor back-cursor)
     back-word*))



(defun find-word*
    (&key
       word
       string
       cursor)
  (multiple-value-bind (next-word* next-cursor)
      (next-word* :string string
                  :cursor cursor)
    (cond ((equal? next-word* word)
           (multiple-value-bind (back-word* back-cursor)
               (back-word* :string string
                           :cursor next-cursor)
             back-cursor))
          ((equal? next-word* :no-more-word)
           nil)
          (:else
           (find-word* :word word
                       :string string
                       :cursor next-cursor)))))

(defmacro find-word*!
    (&key
       word
       string
       cursor)
  `(let ((find-cursor (find-word* :word ,word
                                  :string ,string
                                  :cursor ,cursor)))
     (if (nil? find-cursor)
         nil
         (progn
           (set! ,cursor find-cursor)
           find-cursor))))
(defun pathname? (x)
  (pathnamep x))
(defun pathname->string (pathname)
  (if (not (pathname? pathname))
      (error "the argument of (pathname->string) must be a pathname")
      (namestring pathname)))

(defun string->pathname (string)
  (if (not (string? string))
      (error "the argument of (string->pathname) must be a string")
      (pathname string)))
(defun end-of-list (list)
  (cond
    ((not (pair? list))
     (error "the argument of (end-of-list) must be a list~%"))
    (:else
     (help#loop#end-of-list list))))

(defun help#loop#end-of-list (list)
  (let ((cdr#list (cdr list)))
    (cond
      ((nil? cdr#list)
       (car list))
      ((not (pair? cdr#list))
       (error (concatenate
               'string
               "the argument of (end-of-list) must be not only a list~%"
               "but also a proper-list~%")))
      (:else
       (help#loop#end-of-list cdr#list)))))
(defun set-car! (value list)
  (cond
    ((not (pair? list))
     (error "the argument of (set-car!) must be a list~%"))
    (:else
     (setf (car list) value))))

(defun set-cdr! (value list)
  (cond
    ((not (pair? list))
     (error "the argument of (set-cdr!) must be a list~%"))
    (:else
     (setf (cdr list) value))))


(defun set-end-cdr! (value list)
  (cond
    ((not (pair? list))
     (error "the argument of (set-end-cdr!) must be a list~%"))
    (:else
     (help#loop#set-end-cdr! value list))))

(defun help#loop#set-end-cdr! (value list)
  (let ((cdr#list (cdr list)))
    (cond
      ((not (pair? cdr#list))
       (set-cdr! value list))
      (:else
       (help#loop#set-end-cdr! value cdr#list)))))


(defun set-end-car! (value list)
  (cond
    ((not (pair? list))
     (error "the argument of (set-end-car!) must be a list~%"))
    (:else
     (help#loop#set-end-car! value list))))

(defun help#loop#set-end-car! (value list)
  (let ((cdr#list (cdr list)))
    (cond
      ((not (pair? cdr#list))
       (set-car! value list))
      (:else
       (help#loop#set-end-car! value cdr#list)))))
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
;; (cons-many 1 2 '(3 4))
;; ==>
;; (cons 1
;;       (cons 2
;;             '(3 4)))

(defmacro cons-many (&body form)
  (cond
    ((null (cdr form))
     (car form))
    (:else
     `(cons ,(car form)
            (cons-many . ,(cdr form))))))

;; (cons-many 1 2 (list 3 4))
;; (cons-many (car '(1 2)) (list 3 4))
;; (cons-many (list 3 4))

;; on error
;; (cons-many 1)
;; (getf `(:one 111 :two 222 :three 333) :two)
(defmacro find#key (key-word list)
  `(getf ,list ,key-word))


;; (destructuring-bind (&key one two three)
;;     `(:one 111 :two 222 :three 333)
;;   (list three two one))

(defun find#record (key-word value record)
  (cond ((nil? record)
         nil)
        ((not (pair? (car record)))
         (find#record key-word value (cdr record)))
        ((equal? (find#key key-word (car record))
                 value)
         (car record))
        (:else
         (find#record key-word value (cdr record)))))
(defun function? (x)
  (functionp x))
(defun map-composite-function (function-list list)
  (help#reverse#map-composite-function
   (reverse function-list)
   list))

(defun help#reverse#map-composite-function
    (reversed-function-list
     list)
  (cond
    ((nil? reversed-function-list)
     list)
    (:else
     (mapcar (car reversed-function-list)
             (help#reverse#map-composite-function
              (cdr reversed-function-list)
              list)))))
(defun return-zero-value ()
  (values))


(defparameter *record#defin* nil)

(defmacro defin ;; define-interface
    (function-symbol
     &body
       interface-list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set! *record#defin*
         (cons
          (append (list :function-symbol (quote ,function-symbol))
                  (list :interface-list (quote ,interface-list)))
          *record#defin*))))


(defmacro with
    ((function-symbol . function-body)
     &body
       form-body)
  (let* ((interface-list
          (find#key :interface-list
                    (find#record :function-symbol function-symbol
                                 *record#defin*))))
    (if (nil? interface-list)
        (error (cat ()
                 ("function: ~A have no interface" function-symbol)))
        `(multiple-value-bind
               ,interface-list
             (,function-symbol . ,function-body)
           ,@form-body))))
(defun string->function (string)
  (handler-case
      (symbol-function
       (string->symbol
        (string-upcase string)))
    (undefined-function (condition)
      nil)))

(defun symbol->function (symbol)
  (string->function
   (symbol->string symbol)))
(defun edit#line-list
    (&key
       line-list
       (print-to nil)
       (prefix "")
       (postfix "")
       (indent 0)
       (function-list '()))
  (let* ((line-list-for-return
          (map-composite-function function-list
                                  line-list))
         (line-list-for-return
          (mapcar (lambda (line) (concatenate 'string prefix line))
                  line-list-for-return))
         (line-list-for-return
          (mapcar (lambda (line) (concatenate 'string line postfix))
                  line-list-for-return))
         (line-list-for-return
          (cond ((zero? indent)
                 line-list-for-return)
                (:else
                 (mapcar (lambda (line) (concatenate 'string (dup#string :time indent :string " ") line))
                         line-list-for-return)))))
    (cond ((nil? print-to)
           line-list-for-return)
          ((stream? print-to)
           (mapcar (lambda (line) (format print-to "~A~%" line))
                   line-list-for-return))
          (:else
           (error "the argument :print-to of (edit#line-list) must be a output stream")))))
