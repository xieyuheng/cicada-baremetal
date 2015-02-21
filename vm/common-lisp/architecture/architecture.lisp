(in-package :cicada-vm)
(defparameter *cell-unit* (/ *size#fixnum* 8)) ;; unit byte
(defparameter *cicada-object-size*
  (* 2 *cell-unit*))

(defun cicada-object-vector? (cicada-object-vector)
  (and (equal? '(unsigned-byte 8)
               (array-element-type cicada-object-vector))
       (zero? (mod (length cicada-object-vector)
                   *cicada-object-size*))))
(defun save#cicada-object-vector
    (&key
       (title nil)
       (value nil)
       cicada-object-vector
       (index nil)
       (address nil))
  (cond ((not (nil? index))
         (let ()
           (when (not (nil? title))
             (save#byte-vector :value title
                               :byte-vector cicada-object-vector
                               :size *cell-unit*
                               :index (mul *cicada-object-size*
                                           index)))
           (when (not (nil? value))
             (save#byte-vector :value value
                               :byte-vector cicada-object-vector
                               :size *cell-unit*
                               :index (add *cell-unit*
                                           (mul *cicada-object-size*
                                                index))))))
        ((not (nil? address))
         (let ()
           (when (not (nil? title))
             (save#byte-vector :value title
                               :byte-vector cicada-object-vector
                               :size *cell-unit*
                               :index address))
           (when (not (nil? value))
             (save#byte-vector :value value
                               :byte-vector cicada-object-vector
                               :size *cell-unit*
                               :index (add *cell-unit*
                                           address)))))
        (:else
         (orz ()
           ("when calling (save#cicada-object-vector)~%")
           ("the argument :index or :address must be not nil~%")))))


(defun fetch#cicada-object-vector
    (&key
       field
       cicada-object-vector
       (index nil)
       (address nil))
  (cond ((not (nil? index))
         (cond ((equal? field :title)
                (fetch#byte-vector :byte-vector cicada-object-vector
                                   :size *cell-unit*
                                   :index (mul *cicada-object-size*
                                               index)))
               ((equal? field :value)
                (fetch#byte-vector :byte-vector cicada-object-vector
                                   :size *cell-unit*
                                   :index (add *cell-unit*
                                               (mul *cicada-object-size*
                                                    index))))
               (:else
                (orz ()
                  ("when calling (fetch#cicada-object-vector)~%")
                  ("the argument :field must be :title or :value~%")
                  ("but not ~A ~%" field)))))
        ((not (nil? address))
         (cond ((equal? field :title)
                (fetch#byte-vector :byte-vector cicada-object-vector
                                   :size *cell-unit*
                                   :index address))
               ((equal? field :value)
                (fetch#byte-vector :byte-vector cicada-object-vector
                                   :size *cell-unit*
                                   :index (add *cell-unit*
                                               address)))
               (:else
                (orz ()
                  ("when calling (fetch#cicada-object-vector)~%")
                  ("the argument :field must be :title or :value~%")
                  ("but not ~A ~%" field)))))
        (:else
         (orz ()
           ("when calling (fetch#cicada-object-vector)~%")
           ("the argument :index or :address must be not nil~%")))))
(defparameter *size#title.name-table* 1000)

(defparameter *size#entry#title.name-table* 100)

;; the first entry of *title.name-table* reserved
;; for *name-hash-table*
;; to test if a name in *name-hash-table*
;; is used as title or not
(defparameter *pointer#title.name-table* 1)

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
       (orz ()
         ("can not ask for the object under the name as you wish~%")
         ("and the names under this title is too filled"))))))
(defun title? (index)
  (and (natural-number? index)
       (< index *size#title.name-table*)))
(defun string->title (string)
  (let* ((name (string->name string))
         (index-for-title
          (fetch#name-hash-table :field :index-for-title
                                 :name name)))
    (cond
      ;; find-old
      ((not (zero? index-for-title))
       index-for-title)

      ;; creat-new
      ((< *pointer#title.name-table*
          *size#title.name-table*)
       ;; now
       ;; *pointer#title.name-table* is pointing to
       ;; the next free to use index
       ;; in the *title.name-table*

       ;; save title[index] to :field :index-for-title of name-hash-table
       (save#name-hash-table :value *pointer#title.name-table*
                             :field :index-for-title
                             :name name)

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
       (orz ()
         ("title.name-table is filled~%")
         ("(string->title) can not make new title~%"))))))
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
(defun print#title (title &key (to t))
  (if (not (title? title))
      (error "the argument of (print#title) must be a title")
      (print#name (title->name title)
                  :to to)))
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

(defparameter *name-hash-table#name-counter* 0)

(defparameter *name-hash-table#string*
  (make#vector
   :length *size#name-hash-table*
   :initial-element 0))

(defparameter *name-hash-table#index-for-title*
  (make#vector
   :length *size#name-hash-table*
   :element-type `(integer 0 ,*size#title.name-table*)
   :initial-element 0))


(defun fetch#name-hash-table
    (&key
       name
       field)
  (cond ((equal? field :string)
         (fetch#vector
          :vector *name-hash-table#string*
          :index name))
        ((equal? field :index-for-title)
         (fetch#vector
          :vector *name-hash-table#index-for-title*
          :index name))
        (:else
         (orz ()
           ("the argument :field of (fetch#name-hash-table)~%")
           ("must be a valid field of the name-hash-table~%")
           ("but ~A is not~%" field)))))

(defun save#name-hash-table
    (&key
       value
       name
       field)
  (cond ((equal? field :string)
         (save#vector
          :value value
          :vector *name-hash-table#string*
          :index name))
        ((equal? field :index-for-title)
         (save#vector
          :value value
          :vector *name-hash-table#index-for-title*
          :index name))
        (:else
         (orz ()
           ("the argument :field of (save#name-hash-table)~%")
           ("must be a valid field of the name-hash-table~%")
           ("but ~A is not~%" field)))))

;; to reverse index 0
;; the first entry of *name-hash-table* is reserved
;; for *title.name-table*
;; to test if a title name pair in *title.name-table*
;; is bound to any object or not
(save#name-hash-table :value ""
                      :field :string
                      :name 0)
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
             (fetch#name-hash-table
              :field :string
              :name index))
     index)
    ;; collision
    (:else
     (help#string->name#find-old-or-creat-new
      :string string
      :index (name-hash-table-index#next :index index)
      :collision-level (add1 collision-level)))
    ))

(defun name-hash-table-index#used? (index)
  (not (zero? (fetch#name-hash-table
               :field :string
               :name index))))

(defun name-hash-table-index#as-title? (index)
  (and (name-hash-table-index#used? index)
       (not (zero? (fetch#name-hash-table
                    :field :index-for-title
                    :name index)))))

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
  (save#name-hash-table :value string
                        :field :string
                        :name index))

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
             (fetch#name-hash-table :field :string
                                    :name name)))))
(defun print#name (name
                   &key (to t))
  (format to (name->string name)))
(setf (logical-pathname-translations "cicada")
      `(("**;*.*" "home:cicada;**;*.*")))
(defun config#cicada-image (configuration-list)
  (help ((defun config-section (configuration)
           (let* ((image-name
                   (find#key :image-name configuration))
                  (section-name
                   (find#key :section-name configuration))
                  (section-meta
                   (find#key :section-meta configuration))
                  (stream-cicada-section
                   (open (cat ()
                           ("cicada:")
                           ("image;")
                           ("~A;" image-name)
                           ("~A;" section-name)
                           ("~A.cicada-section" section-name))
                         :direction :output
                         :if-exists :supersede))
                  (stream-meta
                   (open (cat ()
                           ("cicada:")
                           ("image;")
                           ("~A;" image-name)
                           ("~A;" section-name)
                           ("~A.cicada-section-meta" section-name))
                         :direction :output
                         :if-exists :supersede)))
             (cat (:to stream-meta) ("~W" section-meta))
             (close stream-cicada-section)
             (close stream-meta))))
    (mapcar (function config-section)
            configuration-list)))

(config#cicada-image
 `((:image-name "test-image"
                :section-name "test"
                :section-meta (:size 666))
   (:image-name "test-image"
                :section-name "vector-function-heap"
                :section-meta (:size ,(mul 6 1024)))))
(defparameter *size#cicada-memory* (mul 1024 1024))

(defparameter *current-free-address#cicada-memory* 0)

(defparameter *cicada-memory*
  (make#vector :length (mul *size#cicada-memory* *cicada-object-size*)
               :element-type '(unsigned-byte 8)
               :initial-element 0))
(defparameter *data-section-record#cicada-memory*
  `("data-section-record"))
;; `((:section-offset 0
;;    :section-name "vector-function-heap"
;;    :image-name "><"
;;    :section-meta (:size ,(mul 6 1024)))
;;   ...)

(defun load#cicada-section
    (&key
       image-name
       section-name)
  (let* ((section-meta
          (let* ((meta-stream
                  (open (cat ()
                          ("cicada:")
                          ("image;")
                          ("~A;" image-name)
                          ("~A;" section-name)
                          ("~A.cicada-section-meta" section-name))
                        :direction :input))
                 (section-meta (read meta-stream)))
            (close meta-stream)
            (values section-meta)))
         (section-size (find#key :size section-meta))
         (section-offset *current-free-address#cicada-memory*)
         (end-address
          (file->byte-vector!
           :filename (cat ()
                       ("cicada:")
                       ("image;")
                       ("~A;" image-name)
                       ("~A;" section-name)
                       ("~A.cicada-section" section-name))
           :byte-vector *cicada-memory*
           :start section-offset)))
    (set! *current-free-address#cicada-memory*
        (add *current-free-address#cicada-memory*
             section-size))
    (set-end-car! (append `(:section-offset ,section-offset)
                          `(:section-name   ,section-name)
                          `(:image-name     ,image-name)
                          `(:section-meta   ,section-meta))
                  *data-section-record#cicada-memory*)
    (be :title (string->title section-name)
        :name  (string->name "offset")
        :title#object (string->title "fixnum")
        :value#object section-offset)
    (be :title (string->title section-name)
        :name  (string->name "size")
        :title#object (string->title "fixnum")
        :value#object section-size)
    (be :title (string->title section-name)
        :name  (string->name "current-free-address")
        :title#object (string->title "fixnum")
        :value#object end-address)
    (values :load#cicada-section--ok)))
(defun cicada-section-name->meta (section-name)
  (find#key :section-meta
            (find#record :section-name section-name
                         *data-section-record#cicada-memory*)))

(defun cicada-section-name->offset (section-name)
  (find#key :section-offset
            (find#record :section-name section-name
                         *data-section-record#cicada-memory*)))
(defun fetch-byte#cicada-section
    (&key
       (section-offset nil)
       (section-name nil)
       address)
  (cond ((not (nil? section-offset))
         (fetch#byte-vector
          :byte-vector *cicada-memory*
          :size 1
          :index (add address
                      section-offset)))
     
        ((not (nil? section-name))
         (fetch#byte-vector
          :byte-vector *cicada-memory*
          :size 1
          :index (add address
                      (cicada-section-name->offset
                       section-name))))
     
        (:else
         (orz ()
           ("when calling (fetch-byte#cicada-section)~%")
           ("one of is argument :section-offset or :section-name must NOT be nil~%")))))



(defun save-byte#cicada-section
    (&key
       (section-offset nil)
       (section-name nil)
       address
       byte)
  (cond ((not (nil? section-offset))
         (save#byte-vector
          :value byte
          :byte-vector *cicada-memory*
          :size 1
          :index (add address
                      section-offset)))
     
        ((not (nil? section-name))
         (save#byte-vector
          :value byte
          :byte-vector *cicada-memory*
          :size 1
          :index (add address
                      (cicada-section-name->offset
                       section-name))))
     
        (:else
         (orz ()
           ("when calling (save-byte#cicada-section)~%")
           ("one of is argument :section-offset or :section-name must NOT be nil~%")))))
(defin fetch#cicada-section
  .title .value)
(defun fetch#cicada-section
    (&key
       (section-offset nil)
       (section-name nil)
       address)
  (cond ((not (nil? section-offset))
         (values (fetch#byte-vector
                  :byte-vector *cicada-memory*
                  :size *cell-unit*
                  :index (add address
                              section-offset))
                 (fetch#byte-vector
                  :byte-vector *cicada-memory*
                  :size *cell-unit*
                  :index (add *cell-unit*
                              address
                              section-offset))))

        ((not (nil? section-name))
         (values (fetch#byte-vector
                  :byte-vector *cicada-memory*
                  :size *cell-unit*
                  :index (add address
                              (cicada-section-name->offset
                               section-name)))
                 (fetch#byte-vector
                  :byte-vector *cicada-memory*
                  :size *cell-unit*
                  :index (add *cell-unit*
                              address
                              (cicada-section-name->offset
                               section-name)))))

        (:else
         (orz ()
           ("when calling (fetch#cicada-section)~%")
           ("one of is argument :section-offset or :section-name must NOT be nil~%")))))



(defun save#cicada-section
    (&key
       (section-offset nil)
       (section-name nil)
       address
       title
       value)
  (cond ((not (nil? section-offset))
         (save#byte-vector
          :value title
          :byte-vector *cicada-memory*
          :size *cell-unit*
          :index (add address
                      section-offset))
         (save#byte-vector
          :value value
          :byte-vector *cicada-memory*
          :size *cell-unit*
          :index (add *cell-unit*
                      address
                      section-offset)))

        ((not (nil? section-name))
         (save#byte-vector
          :value title
          :byte-vector *cicada-memory*
          :size *cell-unit*
          :index (add address
                      (cicada-section-name->offset
                       section-name)))
         (save#byte-vector
          :value value
          :byte-vector *cicada-memory*
          :size *cell-unit*
          :index (add *cell-unit*
                      address
                      (cicada-section-name->offset
                       section-name))))

        (:else
         (orz ()
           ("when calling (save#cicada-section)~%")
           ("one of is argument :section-offset or :section-name must NOT be nil~%")))))
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
    (:else (save#cicada-object-vector
            :title title
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
     (orz ()
       ("when calling (pop#return-stack)~%")
       ("the *return-stack* must NOT be empty")))
    (:else
     (sub1! *pointer#return-stack*)
     (values (fetch#cicada-object-vector
              :field :title
              :cicada-object-vector *return-stack*
              :index *pointer#return-stack*)
             (fetch#cicada-object-vector
              :field :value
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
     (orz ()
       ("when calling (tos#return-stack)~%")
       ("the *return-stack* must NOT be empty")))
    (:else
     (values (fetch#cicada-object-vector
              :field :title
              :cicada-object-vector *return-stack*
              :index (sub1 *pointer#return-stack*))
             (fetch#cicada-object-vector
              :field :value
              :cicada-object-vector *return-stack*
              :index (sub1 *pointer#return-stack*))
             (sub1 *pointer#return-stack*)))))
(defun empty?#return-stack ()
  (zero? *pointer#return-stack*))
(defun fetch#return-stack
    (&key
       index
       field)
  (cond ((not (< index *pointer#return-stack*))
         (orz ()
           ("when calling (fetch#return-stack)~%")
           ("the argument :index must be < then *pointer#return-stack*~%")
           ("but here: ~%")
           ("index = ~A ~%" index)
           ("*pointer#return-stack* = ~A ~%" *pointer#return-stack*)))
        ((equal? field :title)
         (fetch#cicada-object-vector
          :field :title
          :cicada-object-vector *return-stack*
          :index (sub (sub1 *pointer#return-stack*)
                      index)))
        ((equal? field :value)
         (fetch#cicada-object-vector
          :field :value
          :cicada-object-vector *return-stack*
          :index (sub (sub1 *pointer#return-stack*)
                      index)))
        (:else
         (orz ()
           ("when calling (fetch#return-stack)~%")
           ("the argument :field must be :title or :value~%")
           ("but not ~A ~%" field)))))

(defun save#return-stack
    (&key
       (title nil)
       (value nil)
       index)
  (cond ((not (< index *pointer#return-stack*))
         (orz ()
           ("when calling (save#return-stack)~%")
           ("the argument :index must be < then *pointer#return-stack*~%")
           ("but here: ~%")
           ("index = ~A ~%" index)
           ("*pointer#return-stack* = ~A ~%" *pointer#return-stack*)))
        (:else
         (save#cicada-object-vector
          :title title
          :value value
          :cicada-object-vector *return-stack*
          :index (sub (sub1 *pointer#return-stack*)
                      index)))))
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
    (:else (save#cicada-object-vector
            :title title
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
     (orz ()
       ("when calling (pop#argument-stack)~%")
       ("the *argument-stack* must NOT be empty")))
    (:else
     (sub1! *pointer#argument-stack*)
     (values (fetch#cicada-object-vector
              :field :title
              :cicada-object-vector *argument-stack*
              :index *pointer#argument-stack*)
             (fetch#cicada-object-vector
              :field :value
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
     (orz ()
       ("when calling (tos#argument-stack)~%")
       ("the *argument-stack* must NOT be empty")))
    (:else
     (values (fetch#cicada-object-vector
              :field :title
              :cicada-object-vector *argument-stack*
              :index (sub1 *pointer#argument-stack*))
             (fetch#cicada-object-vector
              :field :value
              :cicada-object-vector *argument-stack*
              :index (sub1 *pointer#argument-stack*))
             (sub1 *pointer#argument-stack*)))))
(defun empty?#argument-stack ()
  (zero? *pointer#argument-stack*))
(defun fetch#argument-stack
    (&key
       index
       field)
  (cond ((not (< index *pointer#argument-stack*))
         (orz ()
           ("when calling (fetch#argument-stack)~%")
           ("the argument :index must be < then *pointer#argument-stack*~%")
           ("but here: ~%")
           ("index = ~A ~%" index)
           ("*pointer#argument-stack* = ~A ~%" *pointer#argument-stack*)))
        ((equal? field :title)
         (fetch#cicada-object-vector
          :field :title
          :cicada-object-vector *argument-stack*
          :index (sub (sub1 *pointer#argument-stack*)
                      index)))
        ((equal? field :value)
         (fetch#cicada-object-vector
          :field :value
          :cicada-object-vector *argument-stack*
          :index (sub (sub1 *pointer#argument-stack*)
                      index)))
        (:else
         (orz ()
           ("when calling (fetch#argument-stack)~%")
           ("the argument :field must be :title or :value~%")
           ("but not ~A ~%" field)))))

(defun save#argument-stack
    (&key
       (title nil)
       (value nil)
       index)
  (cond ((not (< index *pointer#argument-stack*))
         (orz ()
           ("when calling (save#argument-stack)~%")
           ("the argument :index must be < then *pointer#argument-stack*~%")
           ("but here: ~%")
           ("index = ~A ~%" index)
           ("*pointer#argument-stack* = ~A ~%" *pointer#argument-stack*)))
        (:else
         (save#cicada-object-vector
          :title title
          :value value
          :cicada-object-vector *argument-stack*
          :index (sub (sub1 *pointer#argument-stack*)
                      index)))))
(defparameter *size#frame-stack* 1024)

(defparameter *frame-stack*
  (make#vector :length (mul *cicada-object-size* *size#frame-stack*)
               :element-type '(unsigned-byte 8)
               :initial-element 0))

;; pointer is an index into *frame-stack*
;; one step of push pop is *cicada-object-size*
(defparameter *pointer#frame-stack* 0)
(defun push#frame-stack
    (&key
       title
       value)
  (cond
    ;; type check
    ((not (title? title))
     (error "the frame :title of (push#frame-stack) must a title"))
    ;; filled
    ((not (< (mul *pointer#frame-stack*
                  *cicada-object-size*)
             *size#frame-stack*))
     (error "can not push anymore *frame-stack* is filled"))
    ;; side-effect
    ;; *pointer#frame-stack* is always
    ;; a free to use index into cicada-object-vector
    (:else (save#cicada-object-vector
            :value value
            :title title
            :cicada-object-vector *frame-stack*
            :index *pointer#frame-stack*)
           (add1! *pointer#frame-stack*)
           ;; frame current-pointer
           *pointer#frame-stack*)))

(defin pop#frame-stack
  .title
  .value
  .current-pointer)
(defun pop#frame-stack ()
  (cond
    ((zero? *pointer#frame-stack*)
     (orz ()
       ("when calling (pop#frame-stack)~%")
       ("the *frame-stack* must NOT be empty")))
    (:else
     (sub1! *pointer#frame-stack*)
     (values (fetch#cicada-object-vector
              :field :title
              :cicada-object-vector *frame-stack*
              :index *pointer#frame-stack*)
             (fetch#cicada-object-vector
              :field :value
              :cicada-object-vector *frame-stack*
              :index *pointer#frame-stack*)
             *pointer#frame-stack*))))

;; TOS denotes top of stack
(defin tos#frame-stack
  .title
  .value
  .current-pointer)
(defun tos#frame-stack ()
  (cond
    ((zero? *pointer#frame-stack*)
     (orz ()
       ("when calling (tos#frame-stack)~%")
       ("the *frame-stack* must NOT be empty")))
    (:else
     (values (fetch#cicada-object-vector
              :field :title
              :cicada-object-vector *frame-stack*
              :index (sub1 *pointer#frame-stack*))
             (fetch#cicada-object-vector
              :field :value
              :cicada-object-vector *frame-stack*
              :index (sub1 *pointer#frame-stack*))
             (sub1 *pointer#frame-stack*)))))
(defun empty?#frame-stack ()
  (zero? *pointer#frame-stack*))
(defun fetch#frame-stack
    (&key
       index
       field)
  (cond ((not (< index *pointer#frame-stack*))
         (orz ()
           ("when calling (fetch#frame-stack)~%")
           ("the argument :index must be < then *pointer#frame-stack*~%")
           ("but here: ~%")
           ("index = ~A ~%" index)
           ("*pointer#frame-stack* = ~A ~%" *pointer#frame-stack*)))
        ((equal? field :title)
         (fetch#cicada-object-vector
          :field :title
          :cicada-object-vector *frame-stack*
          :index (sub (sub1 *pointer#frame-stack*)
                      index)))
        ((equal? field :value)
         (fetch#cicada-object-vector
          :field :value
          :cicada-object-vector *frame-stack*
          :index (sub (sub1 *pointer#frame-stack*)
                      index)))
        (:else
         (orz ()
           ("when calling (fetch#frame-stack)~%")
           ("the argument :field must be :title or :value~%")
           ("but not ~A ~%" field)))))

(defun save#frame-stack
    (&key
       (title nil)
       (value nil)
       index)
  (cond ((not (< index *pointer#frame-stack*))
         (orz ()
           ("when calling (save#frame-stack)~%")
           ("the argument :index must be < then *pointer#frame-stack*~%")
           ("but here: ~%")
           ("index = ~A ~%" index)
           ("*pointer#frame-stack* = ~A ~%" *pointer#frame-stack*)))
        (:else
         (save#cicada-object-vector
          :title title
          :value value
          :cicada-object-vector *frame-stack*
          :index (sub (sub1 *pointer#frame-stack*)
                      index)))))
;; note that:
;; this function defines the interface of primitive-instruction

(defun execute-next-instruction ()
  (with (tos#return-stack)
    (with (fetch#cicada-section
           :section-offset
           (with (ask :title .title
                      :name (string->name "offset"))
             .value)
           :address .value)
      (funcall (primitive-instruction->host-function
                ;; primitive-instruction
                .value)))))
(defmacro @ (&body body)
    `(let* ((cute-comment#list (quote ,body))
            (length (length cute-comment#list)))
       (make#vector :length length
                    :initial-contents cute-comment#list)))
;; note that
;;   (symbol->string '<::a)
;;   ==>
;;   "A"

(defun | symbol <a> ? | (symbol)
  (if (not (symbol? symbol))
      false
      (let ((string (symbol->string symbol)))
        (and (>= (length string) 3)
             (equal? (string->head#char string) #\<)
             (equal? (string->end#char  string) #\>)))))

(defparameter *<-package* (find-package "<"))
(defun | symbol <:: ? | (symbol)
  (if (not (symbol? symbol))
      false
      (equal? *<-package*
              (symbol-package symbol))))
(defun | string <a> ? | (string)
  (if (not (string? string))
      false
      (and (>= (length string) 3)
           (equal? (string->head#char string) #\<)
           (equal? (string->end#char  string) #\>))))

(defun | string <a> -> a | (string)
  (if (not (| string <a> ? | string))
      (orz ()
        ("the argument of (| string <a> -> a |) must be a (| string <a> ? |)~%")
        ("but not ~A ~%" string))
      (subseq string 1 (sub1 (length string)))))

(defun | string <: ? | (string)
  (if (not (string? string))
      false
      (and (> (length string) 2)
           (equal? (subseq string 0 2) "<:"))))

(defun | string <:a -> a | (string)
  (if (not (| string <: ? | string))
      (orz ()
        ("the argument of (| string <:a -> a |) must be a (| string <: ? | string)~%")
        ("but not ~A ~%" string))
      (subseq string 2)))
(defun cute-comment->unnamed-local-variable (cute-comment)
  (let ((length (length cute-comment)))
    (help ((defun loop-collect (&key
                                  (cursor 0)
                                  (base-list '()))
             ;; (@ -- @) 特殊處理
             ;; 兩元並查
             ;; (因 雖可回頭看 但不可[不易]更改收集)
             ;; <a> <b>  則收 a 爲類型[姓]  並繼續
             ;; <a> <::  則斥 a 爲類型[姓]  並停止
             ;; <a> ***  則收 a 爲類型[姓]  並停止
             (cond ((not (< cursor (sub2 length)))
                    (orz ()
                      ("(cute-comment->unnamed-local-variable)~%")
                      ("meet ill formed (@ ... -- ... @) cute-comment~%")
                      ("the cute-comment as vector is ~A ~%" cute-comment)
                      ("the cursor is ~A ~%" cursor)))
                   ((| <a> <b> ? | cursor)
                    (cons (string->title
                           (| string <a> -> a |
                            (symbol->string
                             (fetch#vector :vector cute-comment
                                           :index cursor))))
                          (loop-collect
                             :cursor (add1 cursor)
                             :base-list base-list)))
                   ((| <a> <:: ? | cursor)
                    base-list)
                   ('| <a> *** |
                    (cons (string->title
                           (| string <a> -> a |
                            (symbol->string
                             (fetch#vector :vector cute-comment
                                           :index cursor))))
                          base-list)))))
      (let* ((list (if (| -- ? | 0)
                       '()
                       (loop-collect)))
             (list-length (length list))
             (vector-length (add1 list-length))
             (number list-length))
        (make#vector :length vector-length
                     :initial-contents (cons number list)))
      :where
      (defun | -- ? | (cursor)
        (let ((dash-dash#symbol
               (fetch#vector :vector cute-comment
                             :index cursor)))
          (and (symbol? dash-dash#symbol)
               (let ((dash-dash#string
                      (symbol->string dash-dash#symbol)))
                 (and (> (length dash-dash#string)
                         1)
                      (equal? (cat (:trim '(#\-))
                                (dash-dash#string))
                              ""))))))
      (defun | <a> <b> ? | (cursor)
        (and (| symbol <a> ? | (fetch#vector :vector cute-comment
                                             :index cursor))
             (| symbol <a> ? | (fetch#vector :vector cute-comment
                                             :index (add1 cursor)))))
      (defun | <a> <:: ? | (cursor)
        (and (| symbol <a> ? | (fetch#vector :vector cute-comment
                                             :index cursor))
             (| symbol <:: ? | (fetch#vector :vector cute-comment
                                             :index (add1 cursor))))))))


;; (cute-comment->unnamed-local-variable
;;  (@ <fixnum>
;;     <fixnum>
;;     <fixnum> <::var1
;;     1        <::var2
;;     <title>  <::var3
;;     fixnum (title) <::var4
;;     --
;;     <fixnum> @))

;; (cute-comment->unnamed-local-variable
;;  (@ <fixnum>
;;     <fixnum>
;;     --
;;     @))


;; (cute-comment->unnamed-local-variable
;;  (@ -- @))



(defun cute-comment->inited-local-variable (cute-comment)
  ;; 語義待定
  )


(defun cute-comment->named-local-variable (cute-comment)
  (let ((length (length cute-comment)))
    (help ((defun loop-collect (&key
                                  (cursor 0)
                                  (base-list '()))
             ;; 找 <::
             ;; 並 回頭看
             ;; 爲 <a>
             ;;    則 收 <:: 者 爲 有名約束變元之名
             ;;       收 a      爲 此約束變元的類型[姓]
             ;;       並 繼續
             ;;    否則 繼續
             ;; 見 --
             ;;    則止
             ;;    否則 繼續
             (cond ((not (< cursor (sub1 length)))
                    (orz ()
                      ("(cute-comment->named-local-variable)~%")
                      ("meet ill formed (@ ... -- ... @) cute-comment~%")
                      ("the cute-comment as vector is ~A ~%" cute-comment)
                      ("the cursor is ~A ~%" cursor)))
                   ((| <:: ? | cursor)
                    (cond ((zero? cursor)
                           (orz ()
                             ("(cute-comment->named-local-variable)~%")
                             ("meet ill formed (@ ... -- ... @) cute-comment~%")
                             ("a <:: is at the beginning~%")
                             ("the cute-comment as vector is ~A ~%" cute-comment)
                             ("the cursor is ~A ~%" cursor)))
                          ((| <a> ? | (sub1 cursor))
                           (cons-many (string->name
                                       (symbol->string
                                        (fetch#vector :vector cute-comment
                                                      :index cursor)))
                                      (string->title
                                       (| string <a> -> a |
                                        (symbol->string
                                         (fetch#vector :vector cute-comment
                                                       :index (sub1 cursor)))))
                                      (loop-collect
                                         :cursor (add1 cursor)
                                         :base-list base-list)))
                          (:else
                           (loop-collect
                              :cursor (add1 cursor)
                              :base-list base-list))))
                   ((| -- ? | cursor)
                    base-list)
                   (:else
                    (loop-collect
                       :cursor (add1 cursor)
                       :base-list base-list)))))
      (let* ((list (loop-collect))
             (list-length (length list))
             (vector-length (add1 list-length))
             (number (div list-length 2)))
        (make#vector :length vector-length
                     :initial-contents (cons number list)))
      :where
      (defun | <:: ? | (cursor)
        (| symbol <:: ? | (fetch#vector :vector cute-comment
                                        :index cursor)))
      (defun | <a> ? | (cursor)
        (| symbol <a> ? | (fetch#vector :vector cute-comment
                                        :index cursor)))
      (defun | -- ? | (cursor)
        (let ((dash-dash#symbol
               (fetch#vector :vector cute-comment
                             :index cursor)))
          (and (symbol? dash-dash#symbol)
               (let ((dash-dash#string
                      (symbol->string dash-dash#symbol)))
                 (and (> (length dash-dash#string)
                         1)
                      (equal? (cat (:trim '(#\-))
                                (dash-dash#string))
                              "")))))))))

;; (cute-comment->named-local-variable
;;  (@ <fixnum>
;;     <fixnum>
;;     <fixnum> <::var1
;;     1        <::var2
;;     <title>  <::var3
;;     fixnum (title) <::var4
;;     --
;;     <fixnum> @))



(defun cute-comment->return-object (cute-comment)
  (let ((length (length cute-comment)))
    (help ((defun find-dash-dash (&key
                                    (cursor 0))
             (cond ((not (< cursor length))
                    (orz ()
                      ("(cute-comment->return-object)~%")
                      ("meet ill formed (@ ... -- ... @) cute-comment~%")
                      ("can not find -- in it~%")
                      ("the cute-comment as vector is ~A ~%" cute-comment)
                      ("the cursor is ~A ~%" cursor)))
                   ((| -- ? | cursor)
                    cursor)
                   (:else
                    (find-dash-dash :cursor (add1 cursor)))))
           (defun loop-collect (&key
                                  (cursor 0)
                                  (base-list '()))
             ;; 找 -- 而後類 無名函數者
             ;; 但是此時無需 兩元並查
             ;; <a>  則收 a 爲類型[姓]  並繼續
             ;; 否則 誤
             ;; 遇 @ 則止
             (cond ((= cursor (sub1 length))
                    (if (| @ ? | cursor)
                        base-list
                        (orz ()
                          ("(cute-comment->return-object)~%")
                          ("meet ill formed (@ ... -- ... @) cute-comment~%")
                          ("the end of it is not @ ~%")
                          ("the cute-comment as vector is ~A ~%" cute-comment)
                          ("the cursor is ~A ~%" cursor))))
                   ((| <a> ? | cursor)
                    (cons (string->title
                           (| string <a> -> a |
                            (symbol->string
                             (fetch#vector :vector cute-comment
                                           :index cursor))))
                          (loop-collect
                             :cursor (add1 cursor)
                             :base-list base-list)))
                   (:else
                    (orz ()
                      ("(cute-comment->return-object)~%")
                      ("meet ill formed (@ ... -- ... @) cute-comment~%")
                      ("some thing other then <> occur after -- ~%")
                      ("the cute-comment as vector is ~A ~%" cute-comment)
                      ("the cursor is ~A ~%" cursor))))))
      (let* ((list (loop-collect :cursor (add1 (find-dash-dash))))
             (list-length (length list))
             (vector-length (add1 list-length))
             (number list-length))
        (make#vector :length vector-length
                     :initial-contents (cons number list)))
      :where
      (defun | @ ? | (cursor)
        (let ((dash-dash#symbol
               (fetch#vector :vector cute-comment
                             :index cursor)))
          (and (symbol? dash-dash#symbol)
               (equal? dash-dash#symbol
                       '@))))
      (defun | -- ? | (cursor)
        (let ((dash-dash#symbol
               (fetch#vector :vector cute-comment
                             :index cursor)))
          (and (symbol? dash-dash#symbol)
               (let ((dash-dash#string
                      (symbol->string dash-dash#symbol)))
                 (and (> (length dash-dash#string)
                         1)
                      (equal? (cat (:trim '(#\-))
                                (dash-dash#string))
                              ""))))))
      (defun | <a> ? | (cursor)
        (| symbol <a> ? | (fetch#vector :vector cute-comment
                                        :index cursor))))))

;; (cute-comment->return-object
;;  (@ <fixnum>
;;     <fixnum>
;;     <fixnum> <::var1
;;     1        <::var2
;;     <title>  <::var3
;;     fixnum (title) <::var4
;;     --
;;     <fixnum>
;;     <fixnum> @))

;; (cute-comment->return-object
;;  (@ <fixnum>
;;     --
;;     <fixnum>
;;     <fixnum>
;;     <fixnum>
;;     <fixnum> @))

;; (cute-comment->return-object
;;  (@ <fixnum>
;;     --
;;     @))
(defparameter *size#primitive-instruction-table* 1000)

(defparameter *pointer#primitive-instruction-table* 1)

(defparameter *primitive-instruction-table*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type 'function
   :initial-element 0))

(defparameter *primitive-instruction-table#title*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type `(integer 0 ,*size#title.name-table*)
   :initial-element 0))

(defparameter *primitive-instruction-table#name*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type `(integer 0 ,*size#name-hash-table*)
   :initial-element 0))


(defparameter *primitive-instruction-table#named-local-variable*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type `vector
   :initial-element 0))

(defparameter *primitive-instruction-table#inited-local-variable*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type `vector
   :initial-element 0))

(defparameter *primitive-instruction-table#unnamed-local-variable*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type `vector
   :initial-element 0))

(defparameter *primitive-instruction-table#return-object*
  (make#vector
   :length *size#primitive-instruction-table*
   :element-type `vector
   :initial-element 0))
(defun fetch#primitive-instruction-table
    (&key
       primitive-instruction
       field)
  (when (equal? primitive-instruction :currnet)
    (set! primitive-instruction *pointer#primitive-instruction-table*))
  (cond ((equal? field :instruction)
         (fetch#vector
          :vector *primitive-instruction-table*
          :index primitive-instruction))
        ((equal? field :title)
         (fetch#vector
          :vector *primitive-instruction-table#title*
          :index primitive-instruction))
        ((equal? field :name)
         (fetch#vector
          :vector *primitive-instruction-table#name*
          :index primitive-instruction))
        ((equal? field :named-local-variable)
         (fetch#vector
          :vector *primitive-instruction-table#named-local-variable*
          :index primitive-instruction))
        ;; ((equal? field :inited-local-variable)
        ;;  (fetch#vector
        ;;   :vector *primitive-instruction-table#inited-local-variable*
        ;;   :index primitive-instruction))
        ((equal? field :unnamed-local-variable)
         (fetch#vector
          :vector *primitive-instruction-table#unnamed-local-variable*
          :index primitive-instruction))
        ((equal? field :return-object)
         (fetch#vector
          :vector *primitive-instruction-table#return-object*
          :index primitive-instruction))
        (:else
         (orz ()
           ("the argument :field of (fetch#primitive-instruction-table)~%")
           ("must be a valid field of the primitive-instruction-table~%")
           ("but ~A is not~%" field)))))


(defun save#primitive-instruction-table
    (&key
       primitive-instruction
       field
       value)
  (when (equal? primitive-instruction :currnet)
    (set! primitive-instruction *pointer#primitive-instruction-table*))
  (cond ((equal? field :instruction)
         (save#vector
          :value value
          :vector *primitive-instruction-table*
          :index primitive-instruction))
        ((equal? field :title)
         (save#vector
          :value value
          :vector *primitive-instruction-table#title*
          :index primitive-instruction))
        ((equal? field :name)
         (save#vector
          :value value
          :vector *primitive-instruction-table#name*
          :index primitive-instruction))
        ((equal? field :named-local-variable)
         (save#vector
          :value value
          :vector *primitive-instruction-table#named-local-variable*
          :index primitive-instruction))
        ;; ((equal? field :inited-local-variable)
        ;;  (save#vector
        ;;   :value value
        ;;   :vector *primitive-instruction-table#inited-local-variable*
        ;;   :index primitive-instruction))
        ((equal? field :unnamed-local-variable)
         (save#vector
          :value value
          :vector *primitive-instruction-table#unnamed-local-variable*
          :index primitive-instruction))
        ((equal? field :return-object)
         (save#vector
          :value value
          :vector *primitive-instruction-table#return-object*
          :index primitive-instruction))
        (:else
         (orz ()
           ("the argument :field of (save#primitive-instruction-table)~%")
           ("must be a valid field of the primitive-instruction-table~%")
           ("but ~A is not~%" field)))))
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
                 :title
                 (fetch#primitive-instruction-table
                  :field :title
                  :primitive-instruction primitive-instruction)

                 :name
                 (fetch#primitive-instruction-table
                  :field :name
                  :primitive-instruction primitive-instruction)

                 :named-local-variable
                 (fetch#primitive-instruction-table
                  :field :named-local-variable
                  :primitive-instruction primitive-instruction)

                 ;; :inited-local-variable
                 ;; (fetch#primitive-instruction-table
                 ;;  :field :inited-local-variable
                 ;;  :primitive-instruction primitive-instruction)

                 :unnamed-local-variable
                 (fetch#primitive-instruction-table
                  :field :unnamed-local-variable
                  :primitive-instruction primitive-instruction)

                 :return-object
                 (fetch#primitive-instruction-table
                  :field :return-object
                  :primitive-instruction primitive-instruction)

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
  (map#primitive-instruction-table
   :function
   (lambda (&key
              title
              name
              named-local-variable
              unnamed-local-variable
              return-object
              primitive-instruction)
     (cat (:to to
               :postfix (cat () ("~%")))
       ("  * ~A ~A"
        (title->string title)
        (name->string name))
       ("    unnamed-local-variable: (~A) ~A"
        (fetch#vector :vector unnamed-local-variable
                      :index 0)
        (map#vector :vector unnamed-local-variable
                    :offset 1
                    :function
                    (lambda (&key element) (title->string element))))
       ("    named-local-variable:   (~A) ~A"
        (fetch#vector :vector named-local-variable
                      :index 0)
        (map#vector :vector named-local-variable
                    :width 2
                    :offset 1
                    :function
                    (lambda (&key sub-vector)
                      (list (title->string
                             (fetch#vector :vector sub-vector
                                           :index 1))
                            (name->string
                             (fetch#vector :vector sub-vector
                                           :index 0))))))
       ("    return-object:          (~A) ~A"
        (fetch#vector :vector return-object
                      :index 0)
        (map#vector :vector return-object
                    :offset 1
                    :function
                    (lambda (&key element) (title->string element)))))))
  (cat (:to to) ("~%")))
(defun primitive-instruction? (index)
  (and (natural-number? index)
       (< index *size#primitive-instruction-table*)))
(defun primitive-instruction->host-function (primitive-instruction)
  (let ((host-function
         (fetch#vector :vector *primitive-instruction-table*
                       :index primitive-instruction)))
    (if (not (function? host-function))
        (orz ()
          ("from an instruction[index]: ~A ~%" primitive-instruction)
          ("(primitive-instruction->host-function) can not find any host-function"))
        host-function)))
(defparameter *title#primitive-instruction*
  (string->title "primitive-instruction"))

(defmacro define-primitive-instruction
    (title-string
     name-string
     cute-comment
     &body body)
  `(let ((title (string->title ,title-string))
         (name (string->name ,name-string)))
     (be :title title
         :name name
         :title#object *title#primitive-instruction*
         :value#object
         (cond ((< *pointer#primitive-instruction-table*
                   *size#primitive-instruction-table*)
                (save#primitive-instruction-table
                 :value (lambda () ,@body)
                 :field :instruction
                 :primitive-instruction :currnet)
                (save#primitive-instruction-table
                 :value title
                 :field :title
                 :primitive-instruction :currnet)
                (save#primitive-instruction-table
                 :value name
                 :field :name
                 :primitive-instruction :currnet)
                (save#primitive-instruction-table
                 :value (cute-comment->unnamed-local-variable ,cute-comment)
                 :field :unnamed-local-variable
                 :primitive-instruction :currnet)
                (save#primitive-instruction-table
                 :value (cute-comment->named-local-variable ,cute-comment)
                 :field :named-local-variable
                 :primitive-instruction :currnet)
                (save#primitive-instruction-table
                 :value (cute-comment->return-object ,cute-comment)
                 :field :return-object
                 :primitive-instruction :currnet)
                (add1! *pointer#primitive-instruction-table*)
                ;; return the old pointer [the index]
                (sub1 *pointer#primitive-instruction-table*))
               (:else
                (orz ()
                  ("when using (define-primitive-instruction)~%")
                  ("the *primitive-instruction-table* must NOT be filled")))))))
(defparameter *size#primitive-function-table* 1000)

(defparameter *pointer#primitive-function-table* 1)

(defparameter *primitive-function-table*
  (make#vector
   :length *size#primitive-function-table*
   :element-type 'function
   :initial-element 0))

(defparameter *primitive-function-table#title*
  (make#vector
   :length *size#primitive-function-table*
   :element-type `(integer 0 ,*size#title.name-table*)
   :initial-element 0))

(defparameter *primitive-function-table#name*
  (make#vector
   :length *size#primitive-function-table*
   :element-type `(integer 0 ,*size#name-hash-table*)
   :initial-element 0))


(defparameter *primitive-function-table#named-local-variable*
  (make#vector
   :length *size#primitive-function-table*
   :element-type `vector
   :initial-element 0))

(defparameter *primitive-function-table#inited-local-variable*
  (make#vector
   :length *size#primitive-function-table*
   :element-type `vector
   :initial-element 0))

(defparameter *primitive-function-table#unnamed-local-variable*
  (make#vector
   :length *size#primitive-function-table*
   :element-type `vector
   :initial-element 0))

(defparameter *primitive-function-table#return-object*
  (make#vector
   :length *size#primitive-function-table*
   :element-type `vector
   :initial-element 0))
(defun fetch#primitive-function-table
    (&key
       primitive-function
       field)
  (when (equal? primitive-function :currnet)
    (set! primitive-function *pointer#primitive-function-table*))
  (cond ((equal? field :function)
         (fetch#vector
          :vector *primitive-function-table*
          :index primitive-function))
        ((equal? field :title)
         (fetch#vector
          :vector *primitive-function-table#title*
          :index primitive-function))
        ((equal? field :name)
         (fetch#vector
          :vector *primitive-function-table#name*
          :index primitive-function))
        ((equal? field :named-local-variable)
         (fetch#vector
          :vector *primitive-function-table#named-local-variable*
          :index primitive-function))
        ;; ((equal? field :inited-local-variable)
        ;;  (fetch#vector
        ;;   :vector *primitive-function-table#inited-local-variable*
        ;;   :index primitive-function))
        ((equal? field :unnamed-local-variable)
         (fetch#vector
          :vector *primitive-function-table#unnamed-local-variable*
          :index primitive-function))
        ((equal? field :return-object)
         (fetch#vector
          :vector *primitive-function-table#return-object*
          :index primitive-function))
        (:else
         (orz ()
           ("the argument :field of (fetch#primitive-function-table)~%")
           ("must be a valid field of the primitive-function-table~%")
           ("but ~A is not~%" field)))))


(defun save#primitive-function-table
    (&key
       primitive-function
       field
       value)
  (when (equal? primitive-function :currnet)
    (set! primitive-function *pointer#primitive-function-table*))
  (cond ((equal? field :function)
         (save#vector
          :value value
          :vector *primitive-function-table*
          :index primitive-function))
        ((equal? field :title)
         (save#vector
          :value value
          :vector *primitive-function-table#title*
          :index primitive-function))
        ((equal? field :name)
         (save#vector
          :value value
          :vector *primitive-function-table#name*
          :index primitive-function))
        ((equal? field :named-local-variable)
         (save#vector
          :value value
          :vector *primitive-function-table#named-local-variable*
          :index primitive-function))
        ;; ((equal? field :inited-local-variable)
        ;;  (save#vector
        ;;   :value value
        ;;   :vector *primitive-function-table#inited-local-variable*
        ;;   :index primitive-function))
        ((equal? field :unnamed-local-variable)
         (save#vector
          :value value
          :vector *primitive-function-table#unnamed-local-variable*
          :index primitive-function))
        ((equal? field :return-object)
         (save#vector
          :value value
          :vector *primitive-function-table#return-object*
          :index primitive-function))
        (:else
         (orz ()
           ("the argument :field of (save#primitive-function-table)~%")
           ("must be a valid field of the primitive-function-table~%")
           ("but ~A is not~%" field)))))
(defun map#primitive-function-table
    (&key
       function
       (primitive-function 1)
       (base-list '()))
  (cond ((not (< primitive-function
                 *pointer#primitive-function-table*))
         base-list)
        (:else
         (cons (funcall function
                 :title
                 (fetch#primitive-function-table
                  :field :title
                  :primitive-function primitive-function)

                 :name
                 (fetch#primitive-function-table
                  :field :name
                  :primitive-function primitive-function)

                 :named-local-variable
                 (fetch#primitive-function-table
                  :field :named-local-variable
                  :primitive-function primitive-function)

                 ;; :inited-local-variable
                 ;; (fetch#primitive-function-table
                 ;;  :field :inited-local-variable
                 ;;  :primitive-function primitive-function)

                 :unnamed-local-variable
                 (fetch#primitive-function-table
                  :field :unnamed-local-variable
                  :primitive-function primitive-function)

                 :return-object
                 (fetch#primitive-function-table
                  :field :return-object
                  :primitive-function primitive-function)

                 :primitive-function primitive-function)

               (map#primitive-function-table
                :function function
                :primitive-function (add1 primitive-function)
                :base-list base-list)))))
(defun print#primitive-function-table
    (&key
       (to *standard-output*))
  (cat (:to to
            :postfix (cat () ("~%")))
    ("* primitive-function-table")
    ("  |----------+--------|")
    ("  | size     | ~6D |" *size#primitive-function-table*)
    ("  | function | ~6D |" (sub1 *pointer#primitive-function-table*))
    ("  |----------+--------|"))
  (map#primitive-function-table
   :function
   (lambda (&key
              title
              name
              named-local-variable
              unnamed-local-variable
              return-object
              primitive-function)
     (cat (:to to
               :postfix (cat () ("~%")))
       ("  * ~A ~A"
        (title->string title)
        (name->string name))
       ("    unnamed-local-variable: (~A) ~A"
        (fetch#vector :vector unnamed-local-variable
                      :index 0)
        (map#vector :vector unnamed-local-variable
                    :offset 1
                    :function
                    (lambda (&key element) (title->string element))))
       ("    named-local-variable:   (~A) ~A"
        (fetch#vector :vector named-local-variable
                      :index 0)
        (map#vector :vector named-local-variable
                    :width 2
                    :offset 1
                    :function
                    (lambda (&key sub-vector)
                      (list (title->string
                             (fetch#vector :vector sub-vector
                                           :index 1))
                            (name->string
                             (fetch#vector :vector sub-vector
                                           :index 0))))))
       ("    return-object:          (~A) ~A"
        (fetch#vector :vector return-object
                      :index 0)
        (map#vector :vector return-object
                    :offset 1
                    :function
                    (lambda (&key element) (title->string element)))))))
  (cat (:to to) ("~%")))
(defun primitive-function? (index)
  (and (natural-number? index)
       (< index *size#primitive-function-table*)))
(defun primitive-function->host-function (primitive-function)
  (let ((host-function
         (fetch#primitive-function-table
          :field :function
          :primitive-function primitive-function)))
    (if (not (function? host-function))
        (orz ()
          ("from an index ~A ~%" primitive-function)
          ("(primitive-function->host-function) can not find any host-function"))
        host-function)))
(defparameter *title#primitive-function*
  (string->title "primitive-function"))

(defmacro define-primitive-function
    (title-string
     name-string
     cute-comment
     &body body)
  `(let ((title (string->title ,title-string))
         (name (string->name ,name-string)))
     (be :title title
         :name name
         :title#object *title#primitive-function*
         :value#object
         (cond ((< *pointer#primitive-function-table*
                   *size#primitive-function-table*)
                (save#primitive-function-table
                 :value (lambda () ,@body)
                 :field :function
                 :primitive-function :currnet)
                (save#primitive-function-table
                 :value title
                 :field :title
                 :primitive-function :currnet)
                (save#primitive-function-table
                 :value name
                 :field :name
                 :primitive-function :currnet)
                (save#primitive-function-table
                 :value (cute-comment->unnamed-local-variable ,cute-comment)
                 :field :unnamed-local-variable
                 :primitive-function :currnet)
                (save#primitive-function-table
                 :value (cute-comment->named-local-variable ,cute-comment)
                 :field :named-local-variable
                 :primitive-function :currnet)
                (save#primitive-function-table
                 :value (cute-comment->return-object ,cute-comment)
                 :field :return-object
                 :primitive-function :currnet)
                (add1! *pointer#primitive-function-table*)
                ;; return the old pointer [the index]
                (sub1 *pointer#primitive-function-table*))
               (:else
                (orz ()
                  ("when using (define-primitive-function)~%")
                  ("the *primitive-function-table* must NOT be filled")))))))
