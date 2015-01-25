(in-package :cicada-vm)
(defparameter *cell-unit* (/ *size#fixnum* 8)) ;; unit byte
(defparameter *cicada-object-size*
  (* 2 *cell-unit*))

(defun cicada-object-vector? (cicada-object-vector)
  (and (equal? '(unsigned-byte 8)
               (array-element-type cicada-object-vector))
       (zero? (mod (length cicada-object-vector)
                   *cicada-object-size*))))
(defun save#title#cicada-object-vector
    (&key
       title
       cicada-object-vector
       index)  
  (save#byte-vector :value title
                    :byte-vector cicada-object-vector
                    :size *cell-unit*
                    :index (mul *cicada-object-size*
                                index)))

(defun save#value#cicada-object-vector
    (&key
       value
       cicada-object-vector
       index) 
  (save#byte-vector :value value
                    :byte-vector cicada-object-vector
                    :size *cell-unit*
                    :index (add *cell-unit*
                                (mul *cicada-object-size*
                                     index))))


(defun fetch#title#cicada-object-vector
    (&key       
       cicada-object-vector
       index)  
  (fetch#byte-vector :byte-vector cicada-object-vector
                     :size *cell-unit*
                     :index (mul *cicada-object-size*
                                 index)))

(defun fetch#value#cicada-object-vector
    (&key
       cicada-object-vector
       index) 
  (fetch#byte-vector :byte-vector cicada-object-vector
                     :size *cell-unit*
                     :index (add *cell-unit*
                                 (mul *cicada-object-size*
                                      index))))
(defparameter *size#title.name-table* 1000)

(defparameter *size#entry#title.name-table* 100)

(defparameter *title.name-table*
  ;; should be a byte-vector in assembly version
  (make-array
   `(,*size#title.name-table* ,*size#entry#title.name-table*)
   ;; note that
   ;; this table's element can be of any type
   ;; but actually
   ;; (i 0) must be an name[index] to name-hash-table
   ;; (i n) must be a vector of
   ;; #( name[index] title[index] value )
   :initial-element 0))

;; the first entry of *title.name-table* reserved
;; for *name-hash-table*
;; to test if a name in *name-hash-table*
;; is used as title or not
(defparameter *pointer#title.name-table* 1)
(defun title? (index)
  (and (natural-number? index)
       (< index *size#title.name-table*)))
(defun string->title (string)
  (let* ((name (string->name string))
         (index-for-title
          (fetch#vector :vector *name-hash-table#index-for-title*
                        :index name)))
    (cond
      ((not (zero? index-for-title))
       index-for-title)

      ((< *pointer#title.name-table*
          *size#title.name-table*)
       ;; now
       ;; *pointer#title.name-table* is pointing to
       ;; the next free to use index
       ;; in the *title.name-table*

       ;; save title[index] to *name-hash-table#index-for-title*
       (save#vector :value *pointer#title.name-table*
                    :vector *name-hash-table#index-for-title*
                    :index name)

       ;; save name[index] to *title.name-table*
       (save#array :value name
                   :array *title.name-table*
                   :index-vector (vector *pointer#title.name-table* 0))

       ;; to update *pointer#title.name-table*
       ;; is to allocate a new index in the *title.name-table*
       (add1! *pointer#title.name-table*)

       ;; return value
       (sub1 *pointer#title.name-table*))

      (:else
       (error (cat ()
                ("title.name-table is filled~%")
                ("(string->title) can not make new title~%")))))))
(defun title->name (title)
  (if (not (title? title))
      (error "the argument of (title->name) must be a title")
      (fetch#array
       :array *title.name-table*
       :index-vector
       (vector title 0))))
(defun title->string (title)
  (if (not (title? title))
      (error "the argument of (title->string) must be a title")
      (name->string (title->name title))))
(defun print-title (title &key (stream t))
  (if (not (title? title))
      (error "the argument of (print-title) must be a title")
      (print-name (title->name title)
                  :stream stream)))
(defun be
    (&key
       title
       name
       title#object
       value#object)
  (cond
    ((not (title? title))
     (error "the argument :title of (be) must be a title"))
    ((not (name? name))
     (error "the argument :name of (be) must be a name"))
    ((not (title? title#object))
     (error "the argument :title#object of (be) must be a title"))
    (:else
     (help#be
      :title title
      :name name
      :title#object title#object
      :value#object value#object))))


(defun help#be
    (&key
       title
       name
       title#object
       value#object
       (field 1))
  (let ((content-of-field
         (fetch#array
          :array *title.name-table*
          :index-vector `#(,title ,field))))
    (cond
      ;; creat new
      ((zero? content-of-field)
       (save#array
        :value (vector name
                       title#object
                       value#object)
        :array *title.name-table*
        :index-vector `#(,title ,field))
       (values field
               nil))
      ;; update
      ((equal? name
               (fetch#vector
                :vector content-of-field
                :index 0))
       (save#array
        :value (vector name
                       title#object
                       value#object)
        :array *title.name-table*
        :index-vector `#(,title ,field))
       (values field
               :updated!!!))
      ;; next
      ((< field *size#entry#title.name-table*)
       (help#be :title title
                :name name
                :title#object title#object
                :value#object value#object
                :field (add1 field)))
      ;; filled
      (:else
       (error "the names under this title is too filled (be) can not do")))))
`interface
`(multiple-value-bind
       (title
        value
        found?)
     (ask :title
          :name )
   ><><><)
(defun ask
    (&key
       title
       name)
  (cond ((not (title? title))
         (error "the argument :title of (ask) must be a title"))
        ((not (name? name))
         (error "the argument :name of (ask) must be a name"))
        (:else
         (help#ask :title title
                   :name name))))

(defun help#ask
    (&key
       title
       name
       (field 1))
  (let ((content-of-field
         (fetch#array :array *title.name-table*
                      :index-vector `#(,title ,field))))
    (cond
      ;; not found
      ((zero? content-of-field)
       (values 0
               0
               nil))
      ;; found
      ((equal? name
               (fetch#vector :vector content-of-field
                             :index 0))
       (let ((vector#name-title-value
              (fetch#array :array *title.name-table*
                           :index-vector `#(,title ,field))))
         (values (fetch#vector :vector vector#name-title-value
                               :index 1)
                 (fetch#vector :vector vector#name-title-value
                               :index 2)
                 :found!!!)))
      ;; next
      ((< field *size#entry#title.name-table*)
       (help#ask :title title
                 :name name
                 :field (add1 field)))
      ;; filled
      (:else
       (error (cat ()
                ("can not ask for the object under the name as you wish~%")
                ("and the names under this title is too filled")))))))
;; must be a prime number

;; 1000003  ;; about 976 k
;; 1000033
;; 1000333
;; 100003   ;; about 97 k
;; 100333
;; 997
;; 499
;; 230      ;; for a special test

(defparameter *size#name-hash-table* 100333)

(defparameter *name-hash-table#string*
  (make-vector
   :length *size#name-hash-table*
   :initial-element 0))

;; to reverse index 0
;; the first entry of *name-hash-table* is reserved
;; for *title.name-table*
;; to test if a title name pair in *title.name-table*
;; is bound to any object or not
(save#vector :value ""
             :vector *name-hash-table#string*
             :index 0)

(defparameter *name-hash-table#index-for-title*
  (make-vector
   :length *size#name-hash-table*
   :element-type `(integer 0 ,*size#title.name-table*)
   :initial-element 0))
(defun name? (index)
  (and (natural-number? index)
       (< index *size#name-hash-table*)))
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
(defun string->name (string)
  (help#string->name#find-old-or-creat-new
   :string string
   :index (mod (string->natural-number string)
               *size#name-hash-table*)))

(defun help#string->name#find-old-or-creat-new
    (&key
       string
       index)
  (cond
    ((not (name-hash-table-index#used? index))
     (help#string->name#creat-new
      :string string
      :index index)
     index)

    ((equal?
      string
      (fetch#vector
       :vector *name-hash-table#string*
       :index index))
     index)

    (:else
     (help#string->name#find-old-or-creat-new
      :string string
      :index (name-hash-table-index#next :index index)))
    ))

(defun name-hash-table-index#used? (index)
  (not (zero? (fetch#vector
               :vector *name-hash-table#string*
               :index index))))

(defun help#string->name#creat-new
    (&key
       string
       index)
  (save#vector
   :value string
   :vector *name-hash-table#string*
   :index index))

(defun name-hash-table-index#next
    (&key index)
  (if (= index *size#name-hash-table*)
      0
      (add1 index)))
(defun name->string (name)
  (if (not (name? name))
      (error "the argument of (name->string) must be a name")
      (cond ((not (name-hash-table-index#used? name))
             (error "this name does not have a string"))
            (:else
             (fetch#vector :vector *name-hash-table#string*
                           :index name))
            )))
(defun print-name (name
                   &key (stream t))
  (format stream
          "[~A]"
          (name->string name)))
(defparameter *size#return-stack* 1024)

(defparameter *return-stack*
  (make-vector :length (mul *cicada-object-size* *size#return-stack*)
               :element-type '(unsigned-byte 8)
               :initial-element 0))

;; pointer is an index into *return-stack*
;; one step of push pop is *cicada-object-size*
(defparameter *pointer#return-stack* 0)

(defun push#return-stack
    (&key
       title
       value)
  (cond
    ;; type check
    ((not (title? title))
     (error "the argument :title of (push#return-stack) must a title"))
    ;; filled
    ((not (< (mul *pointer#return-stack*
                  *cicada-object-size*)
             *size#return-stack*))
     (error "can not push anymore *return-stack* is filled"))
    ;; side-effect
    ;; *pointer#return-stack* is always
    ;; a free to use index into cicada-object-vector
    (:else (save#title#cicada-object-vector
            :title title
            :cicada-object-vector *return-stack*
            :index *pointer#return-stack*)
           (save#value#cicada-object-vector
            :value value
            :cicada-object-vector *return-stack*
            :index *pointer#return-stack*)
           (add1! *pointer#return-stack*)
           ;; return current-pointer
           *pointer#return-stack*)))


`interface
`(multiple-value-bind
       (title
        value
        current-pointer)
     (pop#return-stack)
   ><><><)
(defun pop#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error (cat ()
              ("when call (pop#return-stack)~%")
              ("the *return-stack* must not be empty"))))
    (:else
     (sub1! *pointer#return-stack*)           
     (values (fetch#title#cicada-object-vector
              :cicada-object-vector *return-stack*
              :index *pointer#return-stack*)
             (fetch#value#cicada-object-vector
              :cicada-object-vector *return-stack*
              :index *pointer#return-stack*)
             *pointer#return-stack*))))



;; TOS denotes top of stack
`interface
`(multiple-value-bind
       (title
        value
        current-pointer)
     (tos#return-stack)
   ><><><)
(defun tos#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error (cat ()
              ("when call (tos#return-stack)~%")
              ("the *return-stack* must not be empty"))))
    (:else                
     (values (fetch#title#cicada-object-vector
              :cicada-object-vector *return-stack*
              :index (sub1 *pointer#return-stack*))
             (fetch#value#cicada-object-vector
              :cicada-object-vector *return-stack*
              :index (sub1 *pointer#return-stack*))
             (sub1 *pointer#return-stack*)))))
;; push#return-stack
