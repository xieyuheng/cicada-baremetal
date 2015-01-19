(in-package #:cicada-vm)
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


(defun zero? (x)
  (and (integerp x)
       (zerop x)))

(defun integer? (x)
  (integerp x))

(defun natural-number? (x)
  (and (integerp x)
       (<= 0 x)))

;; (natural-number? 0)
;; (natural-number? 1)
;; (natural-number? -1)
;; (natural-number? 1.1)


(defun list? (x)
  (listp x))

(defun array? (x)
  (arrayp x))

(defun vector? (x)
  (vectorp x))


(defun string? (x)
  (stringp x))

(defun pair? (x)
  (consp x))
(defun add1 (x)
  (+ x 1))

(defun sub1 (x)
  (- x 1))
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


;; (fetch#bits :bits #b0010
;;             :size 1
;;             :index 1)
;; ==> 1
(defun fetch#bytes (&key
                      bytes
                      (size 1)
                      index)
  (fetch#bits :bits bytes
              :size (* 8 size)
              :index (* 8 index)))

;; (fetch#byte :number #xff  :index 0) ;; 255
;; (fetch#byte :number #xff  :index 1) ;; 0
;; (fetch#byte :number #x100 :index 0) ;; 0
;; (fetch#byte :number #x100 :index 1) ;; 1


(defun save#bytes (&key
                     value
                     bytes
                     (size 1)
                     index)
  (save#bits :value value
             :bits bytes
             :size (* 8 size)
             :index (* 8 index)))
(defun list->vector (list)
  (if (not (list? list))
      (error "the argument of (list->vector) must be a list")
      (coerce list 'vector)))


(defun vector->list (vector)
  (if (not (vector? vector))
      (error "the argument of (vector->list) must be a vector")
      (coerce vector 'list)))
;; (make-array '(2 3 4) :initial-element nil)

;; (array-dimension
;;  (make-array '(2 3 4) :initial-element nil)
;;  2)

;; (array-rank
;;  (make-array '(2 3 4) :initial-element nil))

;; (aref (make-array '(2 3 4) :initial-element nil)
;;       0 0 0)



(defun fetch#array (&key
                      array
                      index-vector)
  (let ((index-list (vector->list index-vector)))
    (apply (function aref)
           array index-list)))


;; (fetch#array :array (make-array '(2 3 4) :initial-element nil)
;;              :index-vector '#(0 0 0))



(defun save#array (&key
                     value
                     array
                     index-vector)
  (let ((index-list (vector->list index-vector)))
    (setf
     (apply #'aref array index-list) value)
    (values array
            value)))

;; (save#array :value 1
;;             :array (make-array '(2 3 4) :initial-element nil)
;;             :index-vector '#(0 0 0))
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
     (help#little-endian#fetch#byte-array
      :byte-array byte-array
      :size size
      :index-vector index-vector))

    ((equal? endian 'big)
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

;; (let ((k (make-array `(,*cicada-object-size*)
;;                      :element-type '(unsigned-byte 8)
;;                      :initial-element 1)))
;;   (fetch#byte-array :byte-array k
;;                     :size 2
;;                     :index-vector #(0)))
;; ==> 257



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

;; (let ((k (make-array `(,*cicada-object-size*)
;;                      :element-type '(unsigned-byte 8)
;;                      :initial-element 1)))
;;   (fetch#byte-array :byte-array k
;;                     :size 2
;;                     :index-vector #(0)
;;                     :endian 'big))
;; ==> 257




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
     (help#little-endian#save#byte-array
      :value value
      :byte-array byte-array
      :size size
      :index-vector index-vector))

    ((equal? endian 'big)
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



;; (let ((k (make-array `(,*cicada-object-size*)
;;                      :element-type '(unsigned-byte 8)
;;                      :initial-element 1)))
;;   (save#byte-array :value 1234
;;                    :byte-array k
;;                    :size 2
;;                    :index-vector #(0))
;;   (fetch#byte-array :byte-array k
;;                     :size 2
;;                     :index-vector #(0)))
;; ==> 1234



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



;; (let ((k (make-array `(,*cicada-object-size*)
;;                      :element-type '(unsigned-byte 8)
;;                      :initial-element 1)))
;;   (save#byte-array :value 1234
;;                    :byte-array k
;;                    :size 2
;;                    :index-vector #(0)
;;                    :endian 'big)
;;   (fetch#byte-array :byte-array k
;;                     :size 2
;;                     :index-vector #(0)
;;                     :endian 'big))
;; ==> 1234
(defun fetch#byte-vector (&key
                            byte-vector
                            (size 1)
                            index
                            (endian 'little))
  (fetch#byte-array :byte-array byte-vector
                    :size size
                    :index-vector `#(,index)
                    :endian endian))

;; (let ((k (make-array `(,*cicada-object-size*)
;;                      :element-type '(unsigned-byte 8)
;;                      :initial-element 1)))
;;   (fetch#byte-vector :byte-vector k
;;                      :size 2
;;                      :index 0))
;; ==> 257


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

;; (let ((k (make-array `(,*cicada-object-size*)
;;                      :element-type '(unsigned-byte 8)
;;                      :initial-element 1)))
;;   (save#byte-vector :value 1234
;;                     :byte-vector k
;;                     :size 2
;;                     :index 0)
;;   (fetch#byte-vector :byte-vector k
;;                      :size 2
;;                      :index 0))
;; ==> 1234
(defun return-zero-value ()
  (values))
(defun read#line (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof)
                    (recursive-call-to-reader? nil))
  (read-line from
             eof-as-error?
             read-eof-as
             recursive-call-to-reader?))


(defun read#char (&key
                    (from *standard-input*)
                    (eof-as-error? t)
                    (read-eof-as 'eof)
                    (recursive-call-to-reader? nil))
  (read-char from
             eof-as-error?
             read-eof-as
             recursive-call-to-reader?))


(defun newline (&key (many 1))
  (cond ((= 0 many) :nothing)
        ((= 1 many) (format t "~%"))
        ((< 1 many) (format t "~%")
         (newline :many (sub1 many)))
        (:else :nothing)))
(defun bind-char-to-reader (char reader)
  (set-macro-character char reader))

(defun find-reader-from-char (char)
  (get-macro-character char))
;; (character "1")
;; (character "中")

;; error, length of string must be 1
;; (character "12")
(defun char->code (char)
  (char-code char))

(defun code->char (code)
  (code-char code))
(defun string#empty? (string)
  (equal? string ""))
(defun char#space? (char)
  (let ((code (char->code char)))
    (cond ((= code 32) t)
          ((= code 10) t)
          (:else nil))))

;; (char#space? #\newline)
;; (char#space? #\space)


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
(defun symbol->string (symbol)
  (symbol-name symbol))

(defun string->symbol (string)
  (intern string))
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

;; (end-of-list '(1 2 3))
;; (end-of-list '(1 2 . 3))
;; (end-of-list 3)
(defparameter *cell-unit* 4) ;; 4 bytes
(defparameter *cicada-object-size*
  (* 2 *cell-unit*))
(defun object? (x)
  (and (array? x)
       (= 1 (array-rank x))
       (= 3 (array-dimension x
                             0))
       (equal? '<object>
               (fetch#vector :vector x
                             :index 0))
       (title? (fetch#vector :vector x
                             :index 1))))

;; (object? #(<object>
;;            #(<title> 0)
;;            #(<name> 0)))
;; ==> T
(defun cicada-object->host-object (cicada-object)
  ())
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

(defun index-within-name-table? (index)
  (and (natural-number? index)
       (< index *size#name-table*)))
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
       (equal? '<name>
               (fetch#vector :vector x
                             :index 0))
       (index-within-name-table?
        (fetch#vector :vector x
                      :index 1))))

;; (name? #(<name> 0))
;; ==> T
(defun name->index (name)
  (cond ((not (name? name))
         (error "argument of name->index must be a name"))
        (:else
         (fetch#vector :vector name
                       :index 1))))
(defun string->name (string)
  (let ((index
         (natural-number->index
          (string->natural-number string))))
    (help#string->name#find-old-or-creat-new string
                                             index)))

(defun help#string->name#find-old-or-creat-new (string index)
  (cond
    ((not (name-table-index#used? index))
     (help#string->name#creat-new string
                                  index)
     `#(<name> ,index))

    ((equal? string
             (fetch#array :array *name-table*
                          :index-vector `#(,index 0)))
     `#(<name> ,index))

    (:else
     (help#string->name#find-old-or-creat-new
      string
      (name-table-index#next index)))
    ))


(defun help#string->name#creat-new (string index)
 (save#array :value string
             :array *name-table*
             :index-vector `#(,index 0)))


(defun name-table-index#used? (index)
  (string? (fetch#array :array *name-table*
                        :index-vector `#(,index 0))))

(defun name-table-index#next (index)
  (if (= index *size#name-table*)
      0
      (add1 index)))



(defun name->string (name)
  (cond ((not (name? name))
         (error "argument of name->string must be a name"))
        (:else
         (let ((index (name->index name)))
           (cond ((not (name-table-index#used? index))
                  (error "this name does not have a string"))
                 (:else
                  (fetch#array :array *name-table*
                               :index-vector `#(,index 0)))
                 )))
        ))


;; (name->string (string->name "kkk took my baby away!"))
(defun print-name (name
                   &key (stream t))
  (format stream
          "[~A]"
          (name->string name)))

;; (print-name (string->name "kkk took my baby away!"))
;; <name
;; <as
;; <mean
;; (be)

;; <name
;; <as
;; (explain)



;; interface:
;; (multiple-value-bind
;;       (field
;;        update?
;;        old-mean)
;;     (be :name
;;         :as
;;         :mean )
;;   ><><><)

(defun be (&key
             name
             as
             mean)
  (if (or (not (name? name))
          (not (name? as)))
      (error "the argument :name and :as of (be) must be checked by (name?)")
      (let ((name-index (name->index name))
            (as-index (name->index as)))
        (help#be :name-index name-index
                 :as-index as-index
                 :mean mean))))



(defun help#be (&key
                  name-index
                  as-index
                  mean
                  (field 1))
  (let ((content-of-field
         (fetch#array :array *name-table*
                      :index-vector `#(,name-index ,field))))
    (cond
      ((nil? content-of-field)
       (save#array :value (cons as-index mean)
                   :array *name-table*
                   :index-vector `#(,name-index ,field))
       (values field
               nil
               nil))

      ((equal? as-index
               (car content-of-field))
       (save#array :value (cons as-index mean)
                   :array *name-table*
                   :index-vector `#(,name-index ,field))
       (values field
               :updated!!!
               (cdr content-of-field)))

      ((< field *size#entry#name-table*)
       (help#be :name-index name-index
                :as-index as-index
                :mean mean
                :field (add1 field)))

      (:else
       (error "the meaning of this name is too filled"))
      )))



;; interface:
;; (multiple-value-bind
;;       (mean
;;        find?)
;;     (explain :name
;;              :as )
;;   ><><><)

(defun explain (&key
                  name
                  as)
  (if (or (not (name? name))
          (not (name? as)))
      (error "the argument :name and :as of (explain) must be checked by (name?)")
      (let ((name-index (name->index name))
            (as-index (name->index as)))
        (help#explain :name-index name-index
                      :as-index as-index))))



(defun help#explain (&key
                       name-index
                       as-index
                       (field 1))
  (let ((content-of-field
         (fetch#array :array *name-table*
                      :index-vector `#(,name-index ,field))))
    (cond
      ((nil? content-of-field)
       (values nil
               nil))

      ((equal? as-index
               (car content-of-field))
       (values (cdr content-of-field)
               :found!!!))

      ((< field *size#entry#name-table*)
       (help#explain :name-index name-index
                     :as-index as-index
                     :field (add1 field)))

      (:else
       (error (concatenate
               'string
               "can not explain the name as the way you wish~%"
               "and the meaning of this name is too filled")))
      )))



;; (be :name (string->name "kkk")
;;     :as (string->name "took")
;;     :mean "my baby away!")

;; (explain :name (string->name "kkk")
;;          :as (string->name "took"))



(defun meaningful? (&key
                      name
                      as)
    (multiple-value-bind
          (mean
           find?)
        (explain :name name
                 :as as)
      find?))

;; (meaningful? :name (string->name "kkk")
;;              :as (string->name "took"))
(defparameter *size#title-table*
  1000)

(defparameter *size#entry#title-table*
  100)

(defparameter *title-table*
  (make-array
   (list *size#title-table* *size#entry#title-table*)
   :initial-element nil))

(defun index-within-title-table? (index)
  (and (natural-number? index)
       (< index *size#title-table*)))

(defparameter *pointer#title-table* 0)
(defun string->title (string)
  (let ((name (string->name string))
        (name#title (string->name "title")))
    (cond
      ((meaningful? :name name
                    :as name#title)
       `#(<title>
          ,(explain :name name
                    :as name#title)))

      ((< *pointer#title-table*
          *size#title-table*)
       ;; to create a new title is
       ;; to allocate a new index in the title-table
       (be :name name
           :as name#title
           :mean *pointer#title-table*)
       (setf *pointer#title-table*
             (add1 *pointer#title-table*))
       `#(<title>
          ,(sub1 *pointer#title-table*)))

      (:else
       (error "title-table is filled, can not make new title")))))
(defun title? (x)
  (and (array? x)
       (= 1 (array-rank x))
       (= 2 (array-dimension x
                             0))
       (equal? '<title>
               (fetch#vector :vector x
                             :index 0))
       (index-within-title-table?
        (fetch#vector :vector x
                      :index 1))))

;; (title? #(<title> 0))
;; ==> T
;; (title? (string->title "testing#title?"))
;; ==> T
(defun title->index (title)
  (cond ((not (title? title))
         (error "argument of title->index must be a title"))
        (:else
         (fetch#vector :vector title
                       :index 1))))

;; (title->index (string->title "testing#1#title->index"))
;; (title->index (string->title "testing#2#title->index"))
;; <title
;; <name
;; <object
;; (entitle)

;; <title
;; <name
;; (ask)



;; interface:
;; (multiple-value-bind
;;       (field
;;        update?
;;        old-object)
;;     (entitle :title
;;              :name
;;              :object )
;;   ><><><)

(defun entitle (&key
                  title
                  name
                  object)
  (if (or (not (title? title))
          (not (name? name))
          (not (object? object)))
      (error "one or more the arguments of (entitle) is of wrong type")
      (let ((title-index (title->index title))
            (name-index (name->index name)))
        (help#entitle :title-index title-index
                      :name-index name-index
                      :object object))))



(defun help#entitle (&key
                       title-index
                       name-index
                       object
                       (field 1))
  (let ((content-of-field
         (fetch#array :array *title-table*
                      :index-vector `#(,title-index ,field))))
    (cond
      ((nil? content-of-field)
       (save#array :value (cons name-index object)
                   :array *title-table*
                   :index-vector `#(,title-index ,field))
       (values field
               nil
               nil))

      ((equal? name-index
               (car content-of-field))
       (save#array :value (cons name-index object)
                   :array *title-table*
                   :index-vector `#(,title-index ,field))
       (values field
               :updated!!!
               (cdr content-of-field)))

      ((< field *size#entry#title-table*)
       (help#entitle :title-index title-index
                     :name-index name-index
                     :object object
                     :field (add1 field)))

      (:else
       (error "the names under this title is too filled"))
      )))



;; interface:
;; (multiple-value-bind
;;       (object
;;        find?)
;;     (ask :title
;;          :name )
;;   ><><><)


(defun ask (&key
              title
              name)
  (if (or (not (title? title))
          (not (name? name)))
      (error "one or more the arguments of (ask) is of wrong type")
      (let ((title-index (title->index title))
            (name-index (name->index name)))
        (help#ask :title-index title-index
                  :name-index name-index))))



(defun help#ask (&key
                   title-index
                   name-index
                   (field 1))
  (let ((content-of-field
         (fetch#array :array *title-table*
                      :index-vector `#(,title-index ,field))))
    (cond
      ((nil? content-of-field)
       (values nil
               nil))

      ((equal? name-index
               (car content-of-field))
       (values (cdr content-of-field)
               :found!!!))

      ((< field *size#entry#title-table*)
       (help#ask :title-index title-index
                 :name-index name-index
                 :field (add1 field)))

      (:else
       (error (concatenate
               'string
               "can not ask for the object under the name as you wish~%"
               "and the names under this title is too filled")))
      )))



;; (entitle :title (string->title "kkk")
;;          :name (string->name "took")
;;          :object `#(<object>
;;                    ,(string->title "my")
;;                    "baby away!"))

;; (ask :title (string->title "kkk")
;;      :name (string->name "took"))



(defun entitled? (&key
                    title
                    name)
  (multiple-value-bind
        (object
         find?)
      (ask :title title
           :name name)
    find?))

;; (entitled? :title (string->title "kkk")
;;            :name (string->name "took"))
(string->title "title")
(string->title "return-stack")
(defparameter *size#return-stack* 1024)

(defparameter *return-stack*
  (make-array `(,*size#return-stack*) :initial-element nil))

(defparameter *pointer#return-stack* 0)

(defun push#return-stack (object)
  (if (not (< *pointer#return-stack*
              *size#return-stack*))
      (error "can not push anymore *return-stack* is filled")
      (let ()
        (save#vector :value object
                     :vector *return-stack*
                     :index *pointer#return-stack*)
        (setf *pointer#return-stack*
              (add1 *pointer#return-stack*))
        (values *pointer#return-stack*
                object))))

(defun pop#return-stack ()
  (if (zero? *pointer#return-stack*)
      (error "can not pop anymore *return-stack* is empty")
      (let ()
        (setf *pointer#return-stack*
              (sub1 *pointer#return-stack*))
        (values (fetch#vector :vector *return-stack*
                              :index *pointer#return-stack*)
                *pointer#return-stack*))))

;; (push#return-stack 123)
;; (pop#return-stack)
(defparameter *size#argument-stack* 1024)

(defparameter *argument-stack*
  (make-array `(,*size#argument-stack*) :initial-element nil))

(defparameter *pointer#argument-stack* 0)

(defun push#argument-stack (object)
  (if (not (< *pointer#argument-stack*
              *size#argument-stack*))
      (error "can not push anymore *argument-stack* is filled")
      (let ()
        (save#vector :value object
                     :vector *argument-stack*
                     :index *pointer#argument-stack*)
        (setf *pointer#argument-stack*
              (add1 *pointer#argument-stack*))
        (values *pointer#argument-stack*
                object))))

(defun pop#argument-stack ()
  (if (zero? *pointer#argument-stack*)
      (error "can not pop anymore *argument-stack* is empty")
      (let ()
        (setf *pointer#argument-stack*
              (sub1 *pointer#argument-stack*))
        (values (fetch#vector :vector *argument-stack*
                              :index *pointer#argument-stack*)
                *pointer#argument-stack*))))

;; (push#argument-stack 123)
;; (pop#argument-stack)
(defparameter *size#frame-stack* 1024)

(defparameter *frame-stack*
  (make-array `(,*size#frame-stack*) :initial-element nil))

(defparameter *pointer#frame-stack* 0)

(defun push#frame-stack (object)
  (if (not (< *pointer#frame-stack*
              *size#frame-stack*))
      (error "can not push anymore *frame-stack* is filled")
      (let ()
        (save#vector :value object
                     :vector *frame-stack*
                     :index *pointer#frame-stack*)
        (setf *pointer#frame-stack*
              (add1 *pointer#frame-stack*))
        (values *pointer#frame-stack*
                object))))

(defun pop#frame-stack ()
  (if (zero? *pointer#frame-stack*)
      (error "can not pop anymore *frame-stack* is empty")
      (let ()
        (setf *pointer#frame-stack*
              (sub1 *pointer#frame-stack*))
        (values (fetch#vector :vector *frame-stack*
                              :index *pointer#frame-stack*)
                *pointer#frame-stack*))))

;; (push#frame-stack 123)
;; (pop#frame-stack)
(string->title "primitive-instruction")
(string->title "primitive-function")
;; call#primitive-function
;; tail-call#primitive-function#
