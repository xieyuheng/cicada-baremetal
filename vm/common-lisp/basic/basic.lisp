(in-package :cicada-vm)
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
(defparameter *size#fixnum* 32) ;; unit bit

(defun fixnum? (x)
  (and (natural-number? x)
       (< x
          (expt 2 *size#fixnum*))))


(defun zero? (x)
  (and (integerp x)
       (zerop x)))

(defun integer? (x)
  (integerp x))

(defun natural-number? (x)
  (and (integerp x)
       (<= 0 x)))
(defun add1 (x)
  (+ x 1))

(defun sub1 (x)
  (- x 1))
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
(defun make-vector
    (&key
       length
       element-type
       initial-element
       adjustable
       fill-pointer
       displaced-to
       displaced-index-offset)
  (make-array `(,length)
              :element-type element-type
              :initial-element initial-element
              :adjustable adjustable
              :fill-pointer fill-pointer
              :displaced-to displaced-to
              :displaced-index-offset displaced-index-offset))
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
           postfix)
     &body form#list-of-list)
  (let* ((form#list-of-list#2
          (apply (function append)
                 (mapcar (lambda (list)
                           (list prefix
                                 (list 'string-trim trim
                                       (append '(format nil) list))
                                 postfix))
                         form#list-of-list)))
         (form#final (append '(concatenate (quote string))
                             form#list-of-list#2)))
    `(let ((string-for-return ,form#final))
       (format ,to "~A" string-for-return)
       string-for-return)))

;; (cat (:to *standard-output*
;;           :trim '(#\Space)
;;           :prefix "* "
;;           :postfix (cat () ("~%")))
;;   ("~A" "      123   ")
;;   ("~A" "   456   "))
(defun char? (x)
  (characterp x))

(defun char#space? (char)
  (if (not (char? char))
      (error "the argument of (char#space?) must be a char")
      (let ((code (char->code char)))
        (cond ((= code 32) t)
              ((= code 10) t)
              (:else nil)))))
(defun char->code (char)
  (char-code char))

(defun code->char (code)
  (code-char code))
(defun symbol->string (symbol)
  (symbol-name symbol))

(defun string->symbol (string)
  (intern string))
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
(defun dup#string (&key
                     (time 1)
                     string)
  (cond ((= 1 time)
         string)
        (:else
         (concatenate
          'string
          string
          (dup#string :time (sub1 time)
                      :string string)))))
;; interface:
;; (multiple-value-bind
;;       (head#word
;;        index-end-or-nil
;;        index-start
;;        string)
;;     (string->head#word string)
;;   ><><><)

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
(defun end-of-list (list)
  (cond
    ((not (pair? list))
     (error "the argument of (end-of-list) must be a list"))
    (:else
     (help#loop#end-of-list list))
    ))

(defun help#loop#end-of-list (list)
  (let ((cdr#list (cdr list)))
    (cond
      ((nil? cdr#list)
       (car list))
      ((not (pair? cdr#list))
       (error (concatenate
               'string
               "the argument of (end-of-list) must be not only a list~%"
               "but also a proper-list")))
      (:else
       (help#loop#end-of-list cdr#list))
      )))
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
;; note the order
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
