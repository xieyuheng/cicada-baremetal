(in-package :cicada-vm)
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
(defun index-within-title.name-table? (index)
  (and (natural-number? index)
       (< index *size#title.name-table*)))
(defun title? (vector)
  (and (vector? vector)
       (= 2 (length vector))
       (equal? '<title>
               (fetch#vector :vector vector
                             :index 0))
       (index-within-title.name-table?
        (fetch#vector :vector vector
                      :index 1))))
(defun string->title (string)
  (let* ((name-index
          (name->index (string->name string)))
         (index-for-title
          (fetch#vector :vector *name-hash-table#index-for-title*
                        :index name-index)))
    (cond
      ((not (zero? index-for-title))
       `#(<title> ,index-for-title))

      ((< *pointer#title.name-table*
          *size#title.name-table*)
       ;; now
       ;; *pointer#title.name-table* is pointing to
       ;; the next free to use index
       ;; in the *title.name-table*

       ;; save title[index] to *name-hash-table#index-for-title*
       (save#vector :value *pointer#title.name-table*
                    :vector *name-hash-table#index-for-title*
                    :index name-index)

       ;; save name[index] to *title.name-table*
       (save#array :value name-index
                   :array *title.name-table*
                   :index-vector (vector *pointer#title.name-table* 0))

       ;; to update *pointer#title.name-table*
       ;; is to allocate a new index in the *title.name-table*
       (setf *pointer#title.name-table*
             (add1 *pointer#title.name-table*))

       ;; return value
       `#(<title>
          ,(sub1 *pointer#title.name-table*)))

      (:else
       (error "title.name-table is filled, (string->title) can not make new title")))))
(defun title->index (title)
  (if (not (title? title))
      (error "argument of (title->index) must be a title")
      (fetch#vector :vector title
                    :index 1)))
(defun title->name (title)
  (if (not (title? title))
      (error "argument of (title->name) must be a title")
      `#(<name>
         ,(fetch#array
           :array *title.name-table*
           :index-vector
           (vector (title->index title) 0)))))
(defun title->string (title)
  (if (not (title? title))
      (error "argument of (title->string) must be a title")
      (name->string (title->name title))))
(defun print-title (title &key (stream t))
  (if (not (title? title))
      (error "the argument of (print-title) must be checked by title?")
      (print-name (title->name title)
                  :stream stream)))
(defun be (&key
             title
             name
             cicada-object)
  (cond
    ((not (title? title))
     (error "the argument :title of (be) must be a title"))
    ((not (name? name))
     (error "the argument :name of (be) must be a name"))
    ((not (cicada-object? cicada-object))
     (error "the argument :cicada-object of (be) must be a cicada-object?"))
    (:else
     (let ((title-index (title->index title))
           (name-index (name->index name)))
       (be#low-level :title-index title-index
                     :name-index name-index
                     :cicada-byte-vector (cicada-object->cicada-byte-vector
                                          cicada-object))))))


(defun be#low-level (&key
                       title-index
                       name-index
                       cicada-byte-vector
                       (field 1))
  (let ((content-of-field
         (fetch#array :array *title.name-table*
                      :index-vector `#(,title-index ,field))))
    (cond
      ;; creat new
      ((zero? content-of-field)
       (save#array :value (vector name-index
                                  (cicada-byte-vector->title-index cicada-byte-vector)
                                  (cicada-byte-vector->value cicada-byte-vector))
                   :array *title.name-table*
                   :index-vector `#(,title-index ,field))
       (values field
               nil))
      ;; update
      ((equal? name-index
               (fetch#vector :vector content-of-field
                             :index 0))
       (save#array :value (vector name-index
                                  (cicada-byte-vector->title-index cicada-byte-vector)
                                  (cicada-byte-vector->value cicada-byte-vector))
                   :array *title.name-table*
                   :index-vector `#(,title-index ,field))
       (values field
               :updated!!!))
      ;; next
      ((< field *size#entry#title.name-table*)
       (be#low-level :title-index title-index
                     :name-index name-index
                     :cicada-byte-vector cicada-byte-vector
                     :field (add1 field)))
      ;; filled
      (:else
       (error "the names under this title is too filled (be) can not do")))))
(defun ask (&key
              title
              name)
  (cond ((not (title? title))
         (error "the argument :title of (ask) must be a title"))
        ((not (name? name))
         (error "the argument :name of (ask) must be a name"))
        (:else
         (let ((title-index (title->index title))
               (name-index (name->index name)))
           (multiple-value-bind
                 (cicada-byte-vector
                  found?)
               (ask#low-level :title-index title-index
                              :name-index name-index)
             (values (cicada-byte-vector->cicada-object
                      cicada-byte-vector)
                     found?))))))


(defun ask#low-level (&key
                        title-index
                        name-index
                        (field 1))
  (let ((content-of-field
         (fetch#array :array *title.name-table*
                      :index-vector `#(,title-index ,field))))
    (cond
      ;; not found
      ((zero? content-of-field)
       (values nil
               nil))
      ;; found
      ((equal? name-index
               (fetch#vector :vector content-of-field
                             :index 0))
       (let ((vector-of-name-title-value
              (fetch#array :array *title.name-table*
                           :index-vector `#(,title-index ,field))))
         (values (make-cicada-byte-vector-with#title-index&value
                  :title-index (fetch#vector :vector vector-of-name-title-value
                                             :index 1)
                  :value (fetch#vector :vector vector-of-name-title-value
                                       :index 2)
                  ) ;; this bar-ket is to stress that the next key-word values
                 :found!!!)))
      ;; next
      ((< field *size#entry#title.name-table*)
       (ask#low-level :title-index title-index
                      :name-index name-index
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

(defparameter *name-hash-table#index-for-title*
  (make-vector
   :length *size#name-hash-table*
   :element-type `(integer 0 ,*size#title.name-table*)
   :initial-element 0))
(defun index-within-name-hash-table? (index)
  (and (natural-number? index)
       (< index *size#name-hash-table*)))
(defun name? (vector)
  (and (vector? vector)
       (= 2 (length vector))
       (equal? '<name>
               (fetch#vector :vector vector
                             :index 0))
       (index-within-name-hash-table?
        (fetch#vector :vector vector
                      :index 1))))
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
      (mod natural-number *size#name-hash-table*)))
(defun string->index (string)
  (natural-number->index
   (string->natural-number string)))
(defun string->name (string)
  (help#string->name#find-old-or-creat-new
   string
   (string->index string)))

(defun help#string->name#find-old-or-creat-new (string index)
  (cond
    ((not (name-hash-table-index#used? index))
     (help#string->name#creat-new string
                                  index)
     `#(<name> ,index))

    ((equal? string
             (fetch#vector :vector *name-hash-table#string*
                           :index index))
     `#(<name> ,index))

    (:else
     (help#string->name#find-old-or-creat-new
      string
      (name-hash-table-index#next index)))
    ))

(defun name-hash-table-index#used? (index)
  (not (equal? 0
               (fetch#vector :vector *name-hash-table#string*
                             :index index))))

(defun help#string->name#creat-new (string index)
  (save#vector :value string
               :vector *name-hash-table#string*
               :index index))

(defun name-hash-table-index#next (index)
  (if (= index *size#name-hash-table*)
      0
      (add1 index)))
;; (string->name "")
(defun name->index (name)
  (cond ((not (name? name))
         (error "argument of (name->index) must be a name"))
        (:else
         (fetch#vector :vector name
                       :index 1))))
(defun name->string (name)
  (if (not (name? name))
      (error "argument of name->string must be a name")
      (let ((index (name->index name)))
        (cond ((not (name-hash-table-index#used? index))
               (error "this name does not have a string"))
              (:else
               (fetch#vector :vector *name-hash-table#string*
                             :index index))
              ))))
(defun print-name (name
                   &key (stream t))
  (format stream
          "[~A]"
          (name->string name)))
(defparameter *cell-unit* (/ *size#fixnum* 8)) ;; unit byte
(defparameter *cicada-object-size*
  (* 2 *cell-unit*))

(defun cicada-byte-vector? (byte-vector)
  (and (equal? '(unsigned-byte 8)
               (array-element-type byte-vector))
       (fixnum? (fetch#byte-vector :byte-vector byte-vector
                                   :size *cell-unit*
                                   :index 2))
       (= *cicada-object-size*
          ;; (array-dimension byte-vector 0)
          (length byte-vector))
       (not (zero? (fetch#array
                    :array *title.name-table*
                    :index-vector `#(,(fetch#byte-vector
                                       :byte-vector byte-vector
                                       :size *cell-unit*
                                       :index 0)
                                     0))))))
(defun make-cicada-byte-vector-with#title-index&value
    (&key
       title-index
       value)
  (if (not (index-within-title.name-table? title-index))
      (error (cat ()
               ("the agument :title-index of~%")
               ("  (make-cicada-byte-vector-with#title-index&value)~%")
               ("must be an index-within-title.name-table")))
      (let ((cicada-object#byte-vector
             (make-vector :length *cicada-object-size*
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
        (save#byte-vector :value title-index
                          :byte-vector cicada-object#byte-vector
                          :size *cell-unit*
                          :index 0)
        ;; save#byte-vector returns the byte-vector
        (save#byte-vector :value value
                          :byte-vector cicada-object#byte-vector
                          :size *cell-unit*
                          :index *cell-unit*))))
(defun cicada-byte-vector->title-index (cicada-byte-vector)
  (cond ((not (cicada-byte-vector? cicada-byte-vector))
         (error "the argument of (cicada-byte-vector->title-index) must be cicada-byte-vector"))
        (:else
         (fetch#byte-vector :byte-vector cicada-byte-vector
                            :size *cell-unit*
                            :index 0))))
(defun cicada-byte-vector->value (cicada-byte-vector)
  (cond ((not (cicada-byte-vector? cicada-byte-vector))
         (error "the argument of (cicada-byte-vector->value) must be cicada-byte-vector"))
        (:else
         (fetch#byte-vector :byte-vector cicada-byte-vector
                            :size *cell-unit*
                            :index *cell-unit*))))
(defun cicada-object? (vector)
  (and (vector? vector)
       (equal? 2 (length vector))
       (equal? '<cicada-object>
               (fetch#vector :vector vector
                             :index 0))
       (cicada-byte-vector?
        (fetch#vector :vector vector
                      :index 1))))
(defun cicada-byte-vector->cicada-object (cicada-byte-vector)
  (if (not (cicada-byte-vector? cicada-byte-vector))
      (error "argument of (cicada-byte-vector->cicada-object) must be a cicada-byte-vector")
      `#(<cicada-object>
         ,cicada-byte-vector)))
(defun make-cicada-object
    (&key
       title
       value)
  (if (not (title? title))
      (error (cat ()
               ("the agument :title of~%")
               ("  (make-cicada-object)~%")
               ("must be an title")))
      `#(<cicada-object>
         ,(make-cicada-byte-vector-with#title-index&value
           :title-index (title->index title)
           :value value))))
(defun cicada-object->cicada-byte-vector (cicada-object)
  (if (not (cicada-object? cicada-object))
      (error "argument of (cicada-object->cicada-byte-vector) must be a cicada-object")
      (fetch#vector :vector cicada-object
                    :index 1)))
(defun cicada-object->cicada-byte-vector (cicada-object)
  (cond ((not (cicada-object? cicada-object))
         (error "the argument of (cicada-object->cicada-byte-vector) must be cicada-object"))
        (:else
         (fetch#vector :vector cicada-object
                       :index 1))))
(defun cicada-object->title-index (cicada-object)
  (cond ((not (cicada-object? cicada-object))
         (error "the argument of (cicada-object->title-index) must be cicada-object"))
        (:else
         (cicada-byte-vector->title-index
           (cicada-object->cicada-byte-vector cicada-object)))))
(defun cicada-object->title (cicada-object)
  (cond ((not (cicada-object? cicada-object))
         (error "the argument of (cicada-object->title) must be cicada-object"))
        (:else
         `#(<title>
            ,(cicada-object->title-index cicada-object)))))
(defun cicada-object->value (cicada-object)
  (cond ((not (cicada-object? cicada-object))
         (error "the argument of (cicada-object->value) must be cicada-object"))
        (:else
         (cicada-byte-vector->value
          (cicada-object->cicada-byte-vector cicada-object)))))
;; (string->title "return-stack")
(defparameter *size#return-stack* 1024)

(defparameter *return-stack*
  (make-vector :length (* *cicada-object-size* *size#return-stack*)
               :element-type '(unsigned-byte 8)
               :initial-element 0))

;; pointer is an index into *return-stack*
;; one step of push pop is *cicada-object-size*
(defparameter *pointer#return-stack* 0)

;; explicitly change value to cicada-byte-vector before push
(defun push#return-stack (cicada-byte-vector)
  (cond
    ((not (cicada-byte-vector? cicada-byte-vector))
     (error "the argument of (push#return-stack) must cicada-byte-vector?"))

    ((not (<  (*  *pointer#return-stack*
                  *cicada-object-size*)
              *size#return-stack*))
     (error "can not push anymore *return-stack* is filled"))

    (:else
     (copy#byte-vector :from cicada-byte-vector
                       :from-index 0
                       :to *return-stack*
                       :to-index (*  *pointer#return-stack*
                                     *cicada-object-size*)
                       :size *cicada-object-size*)
     (setf *pointer#return-stack*
           (add1 *pointer#return-stack*))
     (values *pointer#return-stack*
             cicada-byte-vector))))

(defun pop#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error "can not pop anymore *return-stack* is empty"))
    (:else
    (let ((cicada-byte-vector
            (make-cicada-byte-vector-with#title-index&value
             :title-index 0 ;; place holder
             :value 0)))
       (setf *pointer#return-stack*
             (sub1 *pointer#return-stack*))
       (copy#byte-vector :to cicada-byte-vector
                         :to-index 0
                         :from *return-stack*
                         :from-index (*  *pointer#return-stack*
                                         *cicada-object-size*)
                         :size *cicada-object-size*)
       (values cicada-byte-vector
               *pointer#return-stack*)))))


;; TOS denotes top of stack
(defun tos#return-stack ()
  (cond
    ((zero? *pointer#return-stack*)
     (error "can not pop anymore *return-stack* is empty"))
    (:else
     (let ((cicada-byte-vector
            (make-cicada-byte-vector-with#title-index&value
             :title-index 0 ;; place holder
             :value 0)))
       (copy#byte-vector :to cicada-byte-vector
                         :to-index 0
                         :from *return-stack*
                         :from-index (*  (sub1 *pointer#return-stack*)
                                         *cicada-object-size*)
                         :size *cicada-object-size*)
       (values cicada-byte-vector
               (sub1 *pointer#return-stack*))))))
(defun address->instruction (address)
  ;; ><><>< maybe not only the function in the table's entry
  (fetch#vector :vector *primitive-instruction-table*
                :index address))
;; (string->title "primitive-instruction")
(defparameter *size#primitive-instruction-table* 1000)

(defparameter *primitive-instruction-table*
  (make-vector
   :length *size#primitive-instruction-table*
   ;; note that
   ;; this table's element can be of any type
   :initial-element nil))

(defun index-within-primitive-instruction-table? (index)
  (and (natural-number? index)
       (< index *size#primitive-instruction-table*)))

(defparameter *pointer#primitive-instruction-table* 0)
(defparameter *size#cicada-image-buffer* 16)
(defparameter *cicada-image-file* "test.image.iaa~")

(defparameter *cicada-image-buffer*
  (make-array `(,(*  *size#cicada-image-buffer*
                     *cicada-object-size*))
              :element-type '(unsigned-byte 8)
              :initial-element 0))
(progn
  (setf stream (open (make-pathname :name *cicada-image-file*)
                     :direction ':output
                     :if-exists ':supersede))
  (format stream "cicada test~%")
  (close stream))


(defun load-file (&key
                    file
                    buffer
                    (buffer-boundary#lower 0)
                    (buffer-boundary#uper nil))
  (cond ((not (string? file))
         (error "the argument :file of (load-file) must be a string"))
        ((not (byte-vector? buffer))
         (error "the argument :buffer of (load-file) must be a byte-vector"))
        (:else
         ;; return the index of the first byte of the buffer that was not updated
         (read-sequence buffer
                        (open (make-pathname :name file)
                              :element-type '(unsigned-byte 8)
                              :direction ':input)
                        :start buffer-boundary#lower
                        :end buffer-boundary#uper))))

(load-file :file *cicada-image-file*
           :buffer *cicada-image-buffer*)
;; push#return-stack
