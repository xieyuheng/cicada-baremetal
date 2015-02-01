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
(defun print#title (title &key (stream t))
  (if (not (title? title))
      (error "the argument of (print#title) must be a title")
      (print#name (title->name title)
                  :stream stream)))
(defun map#title.name-table
    (&key
       function
       (title 1)
       (base-list '()))
  (cond ((not (< title *pointer#title.name-table*))
         base-list)
        (:else
         (cons (funcall function :title title)
               (map#title.name-table :function function
                                     :title (add1 title)
                                     :base-list base-list)))))

(defun map#entry#title.name-table
    (&key
       title
       function
       (field 1)
       (base-list '()))
  (let ((content-of-field
         (fetch#array :array *title.name-table*
                      :index-vector `#(,title ,field))))
    (cond ((not (vector? content-of-field))
           base-list)
          (:else
           (cons (funcall function
                   :name (fetch#vector
                          :vector content-of-field
                          :index 0)
                   :title#object (fetch#vector
                                  :vector content-of-field
                                  :index 1)
                   :value#object (fetch#vector
                                  :vector content-of-field
                                  :index 2))
                 (map#entry#title.name-table :title title
                                             :function function
                                             :field (add1 field)
                                             :base-list base-list))))))
;; can NOT return a string when :to == nil

(defun print#title.name-table
    (&key
       (to *standard-output*))
  (cat (:to to
            :postfix (cat () ("~%")))
    ("* title.name-table")
    ("  |------------+--------|")
    ("  | size       | ~6D |" *size#title.name-table*)
    ("  | size#entry | ~6D |" *size#entry#title.name-table*)
    ("  | title      | ~6D |" (sub1 *pointer#title.name-table*))
    ("  |------------+--------|"))
  (map#title.name-table
   :function
   (lambda (&key
              title)
     (cat (:to to
               :postfix (cat () ("~%")))
       ("  * ~A" (title->string title)))
     (map#entry#title.name-table
      :title title
      :function
      (lambda (&key
                 name
                 title#object
                 value#object)
        (cat (:to to
                  :postfix (cat () ("~%")))
          ("    * ~A" (name->string name))
          ("      ~A ~A" (title->string title#object) value#object)))))))

;; (be :title (string->title "k1")
;;     :name (string->name "took1")
;;     :title#object (string->title "my1")
;;     :value#object 666)
;; (be :title (string->title "k1")
;;     :name (string->name "took2")
;;     :title#object (string->title "my2")
;;     :value#object 666)
;; (print#title.name-table)
(defin be
  .field ;; index
  .update?)
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
(defin ask
  .title
  .value
  .found?)
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
  (make#vector
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
  (make#vector
   :length *size#name-hash-table*
   :element-type `(integer 0 ,*size#title.name-table*)
   :initial-element 0))

(defparameter *name-hash-table#name-counter* 0)
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
       index
       (collision-level 0))
  (cond
    ;; creat-new
    ((not (name-hash-table-index#used? index))
     (help#string->name#creat-new
      :string string
      :index index
      :collision-level collision-level)
     index)
    ;; find-old
    ((equal? string
             (fetch#vector
              :vector *name-hash-table#string*
              :index index))
     index)
    ;; collision
    (:else
     (help#string->name#find-old-or-creat-new
      :string string
      :index (name-hash-table-index#next :index index)
      :collision-level (add1 collision-level)))
    ))

(defun name-hash-table-index#used? (index)
  (not (zero? (fetch#vector
               :vector *name-hash-table#string*
               :index index))))

(defun name-hash-table-index#as-title? (index)
  (and (name-hash-table-index#used? index)
       (not (zero? (fetch#vector
                    :vector *name-hash-table#index-for-title*
                    :index index)))))

(defparameter *name-hash-table#collision-record* '())

(defun help#string->name#creat-new
    (&key
       string
       index
       collision-level)
  (add1! *name-hash-table#name-counter*)
  (if (not (zero? collision-level))
      (push (list :collision-level collision-level
                  :string string
                  :index index)
            *name-hash-table#collision-record*))
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
(defun print#name (name
                   &key (stream t))
  (format stream
          "~A"
          (name->string name)))
(defun map#name-hash-table
    (&key
       function
       (name 1)
       (base-list '()))
  (cond ((not (< name *size#name-hash-table*))
         base-list)
        ((not (name-hash-table-index#used? name))
         (map#name-hash-table :function function
                              :name (add1 name)
                              :base-list base-list))
        (:else
         (cons (funcall function :name name)
               (map#name-hash-table :function function
                                    :name (add1 name)
                                    :base-list base-list)))))

;; (map#name-hash-table
;;  :function
;;  (lambda (&key name)
;;    (name->string name)))
;; can NOT return a string when :to == nil

(defun print#name-hash-table
    (&key
       (to *standard-output*))
  (cat (:to to
            :postfix (cat () ("~%")))
    ("* name-hash-table")
    ("  |-----------+--------|")
    ("  | size      | ~6D |" *size#name-hash-table*)
    ("  | name      | ~6D |" *name-hash-table#name-counter*)
    ("  | collision | ~6D |" (length *name-hash-table#collision-record*))
    ("  |-----------+--------|"))
  (map#name-hash-table
   :function
   (lambda (&key name)
     (cat (:to to)
       ("  * ~A " (name->string name)))
     (cond
       ((name-hash-table-index#as-title? name)
        (cat (:to to)
          (" [as title] "))))
     (let ((collision-record-entry
            (find#record :index name
                         *name-hash-table#collision-record*)))
       (cond ((not (nil? collision-record-entry))
              (destructuring-bind
                    (&key collision-level
                          string
                          index)
                  collision-record-entry
                (cat (:to to)
                  (" [collision-level: ~A]" collision-level))))))
     (cat (:to to) ("~%")))))
(defparameter *size#cicada-image-buffer* 16)
(defparameter *cicada-image-filename* "test.image.iaa~")

(defparameter *cicada-image*
  (make#vector :length (mul *size#cicada-image-buffer* *cicada-object-size*)
               :element-type '(unsigned-byte 8)
               :initial-element 0))

(defparameter *pointer#cicada-image-buffer* 0)
(defun fetch-byte#cicada-image (&key address)
  (fetch#byte-vector :byte-vector *cicada-image*
                     :size 1
                     :index address))

(defun save-byte#cicada-image (&key address byte)
  (save#byte-vector :value byte
                    :byte-vector *cicada-image*
                    :size 1
                    :index address))

(defin fetch#cicada-image
  .title .value)
(defun fetch#cicada-image (&key address)
  (values (fetch#byte-vector :byte-vector *cicada-image*
                             :size *cell-unit*
                             :index address)
          (fetch#byte-vector :byte-vector *cicada-image*
                             :size *cell-unit*
                             :index (add *cell-unit*
                                         address))))

(defun save#cicada-image (&key address title value)
  (save#byte-vector :value title
                    :byte-vector *cicada-image*
                    :size *cell-unit*
                    :index address)
  (save#byte-vector :value value
                    :byte-vector *cicada-image*
                    :size *cell-unit*
                    :index (add *cell-unit*
                                address)))
(progn
  (setf stream
        (open (make-pathname :name *cicada-image-filename*)
              :direction :output
              :if-exists :supersede))
  (format stream "cicada test~%")
  (close stream))


(file->buffer :filename *cicada-image-filename*
              :buffer *cicada-image*)
(defun fetch#vector-function-body ())
(defun save#vector-function-body ())
(defparameter *size#return-stack* 1024)

(defparameter *return-stack*
  (make#vector :length (mul *cicada-object-size* *size#return-stack*)
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
(defin pop#return-stack
  .title
  .value
  .current-pointer)
(defun pop#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error (cat ()
              ("when calling (pop#return-stack)~%")
              ("the *return-stack* must NOT be empty"))))
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
(defin tos#return-stack
  .title
  .value
  .current-pointer)
(defun tos#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error (cat ()
              ("when calling (tos#return-stack)~%")
              ("the *return-stack* must NOT be empty"))))
    (:else
     (values (fetch#title#cicada-object-vector
              :cicada-object-vector *return-stack*
              :index (sub1 *pointer#return-stack*))
             (fetch#value#cicada-object-vector
              :cicada-object-vector *return-stack*
              :index (sub1 *pointer#return-stack*))
             (sub1 *pointer#return-stack*)))))
;; note that:
;; this function defines the interface of primitive-instruction
;; as:
;; 1. (<primitive-instruction> :title :value )
;;    the return-stack will likely be updated by primitive-instruction
;; 2. at the end of <primitive-instruction>
;;    the (execute-next-instruction) will likely be called again
;; [compare this to real CPU to understand it]

(defun execute-next-instruction ()
  (let* ((address#vector-function-body
          (with (tos#return-stack)
            .value))
         (primitive-instruction
          ;; this means only primitive-instruction is handled now
          (with (fetch#cicada-image
                 :address address#vector-function-body)
            .value)))
    (with (fetch#cicada-image
           :address (add *cicada-object-size*
                         address#vector-function-body))
      (funcall (primitive-instruction->host-function primitive-instruction)
               :title .title
               :value .value))))
(defparameter *size#primitive-instruction-table* 1000)

(defparameter *primitive-instruction-table*
  (make#vector
   :length *size#primitive-instruction-table*
   :initial-element 'function))

(defparameter *pointer#primitive-instruction-table* 1)
(defun primitive-instruction? (index)
  (and (natural-number? index)
       (< index *size#primitive-instruction-table*)))
(defun make-primitive-instruction (host-funciton)
  (cond ((not (function? host-funciton))
         (error "the argument of (make-primitive-instruction) must be a function"))
        ((< *pointer#primitive-instruction-table*
            *size#primitive-instruction-table*)
         (save#vector :value host-funciton
                      :vector *primitive-instruction-table*
                      :index *pointer#primitive-instruction-table*)
         (add1! *pointer#primitive-instruction-table*)
         ;; return the old pointer [the index]
         (sub1 *pointer#primitive-instruction-table*))
        (:else
         (error (cat ()
                  ("when calling (make-primitive-instruction)~%")
                  ("the *primitive-instruction-table* must NOT be filled"))))))
(defun primitive-instruction->host-function (primitive-instruction)
  (let ((host-function
         (fetch#vector :vector *primitive-instruction-table*
                       :index primitive-instruction)))
    (if (not (function? host-function))
        (error (cat ()
                 ("from an instruction[index]~%")
                 ("(primitive-instruction->host-function) can not find any host-function")))
        host-function)))

;; (defun primitive-instruction->host-function (primitive-instruction)
;;   (fetch#vector :vector *primitive-instruction-table*
;;                 :index primitive-instruction))
(defun map#primitive-instruction-table
    (&key
       function
       (primitive-instruction 1)
       (base-list '()))
  (cond ((not (< primitive-instruction
                 *pointer#primitive-instruction-table*))
         base-list)
        (:else
         (cons (funcall function
                 :primitive-instruction primitive-instruction)
               (map#primitive-instruction-table
                :function function
                :primitive-instruction (add1 primitive-instruction)
                :base-list base-list)))))
(defun print#primitive-instruction-table 
    (&key
       (to *standard-output*))
  (cat (:to to
            :postfix (cat () ("~%")))
    ("* primitive-instruction-table")
    ("  |-------------+--------|")
    ("  | size        | ~6D |" *size#primitive-instruction-table*)
    ("  | instruction | ~6D |" (sub1 *pointer#primitive-instruction-table*))
    ("  |-------------+--------|"))
  ;; (map#primitive-instruction-table
  ;;  :function
  ;;  (lambda (&key primitive-instruction)
  ;;    ))
  )
(defun &call-primitive-function (&key title value)
  ;; ><><>< should do title check ???
  (funcall (primitive-function->host-function value)))
(defparameter *size#primitive-function-table* 1000)

(defparameter *primitive-function-table*
  (make#vector
   :length *size#primitive-function-table*
   :initial-element 'function))

(defparameter *pointer#primitive-function-table* 1)
(defun primitive-function? (index)
  (and (natural-number? index)
       (< index *size#primitive-function-table*)))
(defun make-primitive-function (host-funciton)
  (cond ((not (function? host-funciton))
         (error "the argument of (make-primitive-function) must be a function"))
        ((< *pointer#primitive-function-table*
            *size#primitive-function-table*)
         (save#vector :value host-funciton
                      :vector *primitive-function-table*
                      :index *pointer#primitive-function-table*)
         (add1! *pointer#primitive-function-table*)
         ;; return the old pointer [the index]
         (sub1 *pointer#primitive-function-table*))
        (:else
         (error (cat ()
                  ("when calling (make-primitive-function)~%")
                  ("the *primitive-function-table* must NOT be filled"))))))
(defun primitive-function->host-function (primitive-function)
  (let ((host-function
         (fetch#vector :vector *primitive-function-table*
                       :index primitive-function)))
    (if (not (function? host-function))
        (error (cat ()
                 ("from an function[index]~%")
                 ("(primitive-function->host-function) can not find any host-function")))
        host-function)))

;; (defun primitive-function->host-function (primitive-function)
;;   (fetch#vector :vector *primitive-function-table*
;;                 :index primitive-function))
(defun &kkk ()
  (cat (:to *standard-output*)
    ("kkk took what away?")))
(defparameter *size#argument-stack* 1024)

(defparameter *argument-stack*
  (make#vector :length (mul *cicada-object-size* *size#argument-stack*)
               :element-type '(unsigned-byte 8)
               :initial-element 0))

;; pointer is an index into *argument-stack*
;; one step of push pop is *cicada-object-size*
(defparameter *pointer#argument-stack* 0)
(defun push#argument-stack
    (&key
       title
       value)
  (cond
    ;; type check
    ((not (title? title))
     (error "the argument :title of (push#argument-stack) must a title"))
    ;; filled
    ((not (< (mul *pointer#argument-stack*
                  *cicada-object-size*)
             *size#argument-stack*))
     (error "can not push anymore *argument-stack* is filled"))
    ;; side-effect
    ;; *pointer#argument-stack* is always
    ;; a free to use index into cicada-object-vector
    (:else (save#title#cicada-object-vector
            :title title
            :cicada-object-vector *argument-stack*
            :index *pointer#argument-stack*)
           (save#value#cicada-object-vector
            :value value
            :cicada-object-vector *argument-stack*
            :index *pointer#argument-stack*)
           (add1! *pointer#argument-stack*)
           ;; argument current-pointer
           *pointer#argument-stack*)))
(defin pop#argument-stack
  .title
  .value
  .current-pointer)
(defun pop#argument-stack ()
  (cond
    ((zero? *pointer#argument-stack*)
     (error (cat ()
              ("when calling (pop#argument-stack)~%")
              ("the *argument-stack* must NOT be empty"))))
    (:else
     (sub1! *pointer#argument-stack*)
     (values (fetch#title#cicada-object-vector
              :cicada-object-vector *argument-stack*
              :index *pointer#argument-stack*)
             (fetch#value#cicada-object-vector
              :cicada-object-vector *argument-stack*
              :index *pointer#argument-stack*)
             *pointer#argument-stack*))))
;; TOS denotes top of stack
(defin tos#argument-stack
  .title
  .value
  .current-pointer)
(defun tos#argument-stack ()
  (cond
    ((zero? *pointer#argument-stack*)
     (error (cat ()
              ("when calling (tos#argument-stack)~%")
              ("the *argument-stack* must NOT be empty"))))
    (:else
     (values (fetch#title#cicada-object-vector
              :cicada-object-vector *argument-stack*
              :index (sub1 *pointer#argument-stack*))
             (fetch#value#cicada-object-vector
              :cicada-object-vector *argument-stack*
              :index (sub1 *pointer#argument-stack*))
             (sub1 *pointer#argument-stack*)))))

