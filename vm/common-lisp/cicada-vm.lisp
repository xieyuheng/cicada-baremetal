(in-package :cicada-vm)
(defparameter *cell-unit* 4) ;; 4 bytes
(defparameter *cicada-object-size*
  (* 2 *cell-unit*))
(defun host-object? (x)
  (and (vector? x)
       (= 3 (array-dimension x
                             0))
       (equal? '<host-object>
               (fetch#vector :vector x
                             :index 0))
       (title? (fetch#vector :vector x
                             :index 1))))
(defun make-cicada-object (&key
                             title
                             value)
  (if (not (title? title))
      (error "the agument :title of (make-cicada-object) must be checked by title?")
      (let ((cicada-object (make-array `(,*cicada-object-size*)
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
        (save#byte-vector :value (title->index title)
                          :byte-vector cicada-object
                          :size *cell-unit*
                          :index 0)
        (save#byte-vector :value value
                          :byte-vector cicada-object
                          :size *cell-unit*
                          :index *cell-unit*))))
(defun cicada-object? (x)
  (and (vector? x)
       (equal? '(unsigned-byte 8)
               (array-element-type x))
       (= *cicada-object-size*
          (array-dimension x 0))
       (not
        (nil?
         (fetch#array
          :array *title-table*
          :index-vector (vector (fetch#byte-vector
                                 :byte-vector x
                                 :size *cell-unit*
                                 :index 0)
                                0))))
       ))
(defun host-object->cicada-object (host-object)
  (if (not (host-object? host-object))
      (error "the argument of (host-object->cicada-object) must be checked by host-object?")
      (make-cicada-object :title (fetch#vector :vector host-object
                                               :index 1)
                          :value (fetch#vector :vector host-object
                                               :index 2))))
(defun cicada-object->host-object (cicada-object)
  (cond ((not (cicada-object? cicada-object))
         (error "the argument of (cicada-object->host-object) must be checked by cicada-object?"))
        (:else
         `#(<host-object>
            ,(vector '<title>
                     (fetch#byte-vector :byte-vector cicada-object
                                        :size *cell-unit*
                                        :index 0))
            ,(fetch#byte-vector :byte-vector cicada-object
                                :size *cell-unit*
                                :index *cell-unit*)))
        ))
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
(defun natural-number->index (natural-number)
  (if (not (natural-number? natural-number))
      (error "argument of natural-number->index must be a natural-number")
      (mod natural-number *size#name-table*)))
(defun name? (x)
  (and (vector? x)
       (= 2 (array-dimension x
                             0))
       (equal? '<name>
               (fetch#vector :vector x
                             :index 0))
       (index-within-name-table?
        (fetch#vector :vector x
                      :index 1))))
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
(defun print-name (name
                   &key (stream t))
  (format stream
          "[~A]"
          (name->string name)))
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
(defun meaningful? (&key
                      name
                      as)
    (multiple-value-bind
          (mean
           find?)
        (explain :name name
                 :as as)
      find?))
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
       ;; and save the name#title to the field number 0 of the entry
       (be :name name
           :as name#title
           :mean *pointer#title-table*)
       (save#array :value name#title
                   :array *title-table*
                   :index-vector (vector *pointer#title-table* 0))
       ;; update *pointer#title-table*
       (setf *pointer#title-table*
             (add1 *pointer#title-table*))
       `#(<title>
          ,(sub1 *pointer#title-table*)))

      (:else
       (error "title-table is filled, can not make new title")))))
(defun title? (x)
  (and (vector? x)
       (= 2 (array-dimension x
                             0))
       (equal? '<title>
               (fetch#vector :vector x
                             :index 0))
       (index-within-title-table?
        (fetch#vector :vector x
                      :index 1))))
(defun title->index (title)
  (cond ((not (title? title))
         (error "argument of title->index must be a title"))
        (:else
         (fetch#vector :vector title
                       :index 1))))
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
          (not (host-object? object)))
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
(defun entitled? (&key
                    title
                    name)
  (multiple-value-bind
        (object
         find?)
      (ask :title title
           :name name)
    find?))
(defun print-title (title &key (stream t))
  (if (not (title? title))
      (error "the argument of (print-title) must be checked by title?")
      (print-name (fetch#array :array *title-table*
                               :index-vector `#(,(title->index title) 0))
                  :stream stream)))
(string->title "title")
(string->title "return-stack")
(defparameter *size#return-stack* 1024)

(defparameter *return-stack*
  (make-array `(,(*  *cicada-object-size*
                     *size#return-stack*))
              :element-type '(unsigned-byte 8)
              :initial-element 0))

;; pointer is an index into *return-stack*
;; one step of push pop is *cicada-object-size*
(defparameter *pointer#return-stack* 0)

(defun push#return-stack (cicada-object)
  (cond
    ((not (cicada-object? cicada-object))
     (error "the argument of (push#return-stack) must be checked by cicada-object?"))

    ((not (<  (*  *pointer#return-stack*
                  *cicada-object-size*)
              *size#return-stack*))
     (error "can not push anymore *return-stack* is filled"))

    (:else
     (let ()
       (copy#byte-vector :from cicada-object
                         :from-index 0
                         :to *return-stack*
                         :to-index (*  *pointer#return-stack*
                                       *cicada-object-size*)
                         :size *cicada-object-size*)
       (setf *pointer#return-stack*
             (add1 *pointer#return-stack*))
       (values *pointer#return-stack*
               cicada-object)))))

(defun pop#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error "can not pop anymore *return-stack* is empty"))

    (:else
     (setf *pointer#return-stack*
           (sub1 *pointer#return-stack*))
     (values (fetch#byte-vector :byte-vector *return-stack*
                                :index (*  *pointer#return-stack*
                                           *cicada-object-size*))
             *pointer#return-stack*))))
;; (string->title "primitive-instruction")
;; (string->title "primitive-function")
;; call#primitive-function
;; tail-call#primitive-function
