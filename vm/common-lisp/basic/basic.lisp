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
(defmacro newline ()
  (cat () ("~%")))

;; (newline)

;; (cat ()
;;   ((newline)))

;; (defun newline (&key (many 1))
;;   (cond ((= 0 many) :nothing)
;;         ((= 1 many) (format t "~%"))
;;         ((< 1 many) (format t "~%")
;;          (newline :many (sub1 many)))
;;         (:else :nothing)))
(defun bind-char-to-reader (char reader)
  (set-macro-character char reader))

(defun find-reader-from-char (char)
  (get-macro-character char))
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
(defstruct (test-group
             (:constructor make-test-group (name &key
                                                 pre
                                                 post
                                                 docstring))
             (:print-function %print-test-group))
  (name (required-argument) :type symbol :read-only t)
  (docstring nil :type (or null simple-base-string) :read-only t)
  (tests (make-hash-table) :type hash-table :read-only t)
  (pre nil :type (or null function))
  (post nil :type (or null function)))

(defun %print-test-group (group stream depth)
  (declare (ignore depth))
  (print-unreadable-object (group stream :type t :identity t)
    (format stream "~S, ~D tests" (test-group-name group)
            (hash-table-count (test-group-tests group)))))


(defstruct (test
             (:constructor make-test (name fn
                                           &key
                                           after
                                           after-pass
                                           after-fail
                                           when
                                           unless
                                           priority
                                           docstring))
             (:print-function %print-test))
  (name (required-argument) :type symbol :read-only t)
  (docstring nil :type (or null simple-base-string) :read-only t)
  (fn (required-argument) :type function :read-only t)
  (priority 0 :type fixnum)
  (after '() :type list)
  (after-pass '() :type list)
  (after-fail '() :type list)
  (when nil :type (or null function))
  (unless nil :type (or null function)))

(defun %print-test (test stream depth)
  (declare (ignore depth))
  (print-unreadable-object (test stream :type t :identity t)
    (princ (test-name test) stream)))

(defun required-argument ()
  (error "A required argument was not supplied."))
(defun find-test-group (name &optional create)
  (if (test-group-p name)
      name
      (let ((group (get name 'tests)))
        (cond (group group)
              (create (setf (get name 'tests) (make-test-group name)))))))

(defmacro define-test-group (name &optional opts docstring)
  (let ((tmp (gensym "GROUP"))
        (pre (gensym))
        (post (gensym)))
    `(let ((,tmp (find-test-group ',name))
           (,pre ,(getf opts :before))
           (,post ,(getf opts :after)))
       (if (null ,tmp)
           (setf (get ',name 'tests) (make-test-group ',name
                                                      :pre ,pre
                                                      :post ,post
                                                      :docstring ',docstring))
           (progn
             (when ,pre (setf (test-group-pre ,tmp) ,pre))
             (when ,post (setf (test-group-post ,tmp) ,post))
             ,tmp)))))
(defmacro deftest
    (test-name
     (group &key
            after
            after-pass
            after-fail
            when unless
            priority)
     &body body)

  (multiple-value-bind
        (body
         decls
         doc)
      (help#parse-body#deftest body nil t)

    (let* ((test-function-name
            (intern (concatenate 'string
                                 #.(string '#:test-)
                                 (string test-name)
                                 "/"
                                 (string group))))
           (hash (gensym "HASH"))
           (keys (append
                  ;; symbol or list of symbol
                  (cond ((consp after)      `(:after       (quote ,after)))
                        (after              `(:after       (quote (,after)))))
                  (cond ((consp after-pass) `(:after-pass  (quote ,after-pass)))
                        (after-pass         `(:after-pass  (quote (,after-pass)))))
                  (cond ((consp after-fail) `(:after-fail  (quote ,after-fail)))
                        (after-fail         `(:after-fail  (quote (,after-fail)))))

                  (when when   `(:when   (lambda () ,when)))
                  (when unless `(:unless (lambda () ,unless)))
                  (when doc `(:docstring ,doc))
                  (if priority
                      `(:priority ,priority)
                      `(:priority (if (gethash (quote ,test-name) ,hash)
                                      (test-priority
                                       (gethash (quote ,test-name) ,hash))
                                      (hash-table-count ,hash)))))))



      `(progn
         (defun ,test-function-name ()
           ,doc
           ,@decls
           (block ,test-name
            ;; test-block ,test-name
             ,@body))

         (let ((,hash (test-group-tests
                       (find-test-group (quote ,group) t))))
           (when (gethash (quote ,test-name) ,hash) (warn "Redefining test ~A." (quote ,test-name)))
           (setf (gethash (quote ,test-name) ,hash)
                 (make-test (quote ,test-name)
                            (function ,test-function-name)
                            ,@keys)))

         (quote ,test-name)
         ))))


(defun help#parse-body#deftest (body env &optional doc-p)
  (declare (ignore env))
  (let ((decls '())
        (doc nil))
    (loop (cond
            ((and (consp (first body))
                  (eq (first (first body))
                      'declare))
             (push (pop body) decls))

            ((and doc-p
                  (null doc)
                  (stringp (first body)))
             (setq doc (pop body)))

            (:else
             (return (values body
                             (nreverse decls)
                             doc))
             )))))
(defmacro help#==>#ensure
    (&key
       actual-form
       expect-form)

  `(handler-case (values (multiple-value-list ,actual-form)
                         (multiple-value-list ,expect-form))

     ;; if a condition occur
     ;; :actual-form or :expect-form
     ;; match the condition's type to the following cases

     (simple-error (condition)
       (values nil
               (cat (:trim (cat () ("~%")))
                 ((cat (:postfix (cat () ("~%")))
                    ("#+begin_src lisp")
                    (";; [ACTUAL-FORM]")
                    ("~S" (quote ,actual-form))
                    ("")
                    (";; [EXPECT-FORM]")
                    ("~S" (quote ,expect-form))
                    ("")
                    (";; [ACTUAL-CONDITION when evaluating the forms]")
                    ("~A" (apply (function format) nil
                                 (simple-condition-format-control condition)
                                 (simple-condition-format-arguments condition)))
                    ("#+end_src"))))))

     (error (condition)
       (values nil
               (cat (:trim (cat () ("~%")))
                 ((cat (:postfix (cat () ("~%")))
                    ("#+begin_src lisp")
                    (";; [ACTUAL-FORM]")
                    ("~S" (quote ,actual-form))
                    ("")
                    (";; [EXPECT-FORM]")
                    ("~S" (quote ,expect-form))
                    ("")
                    (";; [ACTUAL-CONDITION when evaluating the forms]")
                    ("~A" condition)
                    ("#+end_src"))))))

     ;; the following names are bound by VALUES
     (:no-error (actual-value-list
                 expect-value-list)
       (cond ((not (and (= (length actual-value-list)
                           (length expect-value-list))
                        (every (function equalp)
                               actual-value-list
                               expect-value-list)))
              (values nil
                      (cat (:trim (cat () ("~%")))
                        ((cat (:postfix (cat () ("~%")))
                           ("#+begin_src lisp")
                           (";; [ACTUAL-FORM]")
                           ("~S" (quote ,actual-form))
                           ("")
                           (";; [EXPECT-VALUE]")
                           ("~{~S~^~%~17T~}" expect-value-list)
                           ("")
                           (";; [ACTUAL-VALUE]")
                           ("~{~S~^~%~15T~}" actual-value-list)
                           ("#+end_src"))))))
             (:else
              (values t
                      "ensure successed ^-^"))
             ))))

;; (ensure
;;     (values 1 2 3 4 5)
;;     ==>
;;     (values 1 2 3 4 5))

;; ><><><
;; (ensure
;;     (values 1 2 3 4 5)
;;     ==>
;;     (values 5 4 3 2 1))

;; (ensure
;;     (list (be :name (string->name "kkk")
;;               :as (string->name "took")
;;               :mean "my baby away!")
;;           (multiple-value-list
;;            (be :name (string->name "kkk")
;;                :as (string->name "took")
;;                :mean "my baby away!"))
;;           (multiple-value-list
;;            (explain :name (string->name "kkk")
;;                     :as (string->name "took"))))
;;     ==>
;;     (list 1
;;           `(2
;;             :UPDATED!!!
;;             "my baby away!")
;;           `("my baby away!"
;;             :found!!!)))

;; (ensure
;;     (string->head#char "")
;;     ==>
;;     '><><><)

;; (ensure
;;     (error "testing (ensure)")
;;     ==>
;;     '><><><)
(defmacro help#signals#ensure
    (&key
       actual-form
       expect-condition)

  `(handler-case (multiple-value-list ,actual-form)

     (,expect-condition () t)

     (simple-error (condition)
       (cat (:trim (cat () ("~%")))
         ((cat (:postfix (cat () ("~%")))
            ("#+begin_src lisp")
            (";; [ACTUAL-FORM]")
            ("~S" (quote ,actual-form))
            ("")
            (";; [EXPECT-CONDITION]")
            ("~S" (quote ,expect-condition))
            ("")
            (";; [ACTUAL-CONDITION]")
            ("~A" (apply (function format) nil
                         (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition)))
            ("#+end_src")))))

     (error (condition)
       (cat (:trim (cat () ("~%")))
         ((cat (:postfix (cat () ("~%")))
            ("#+begin_src lisp")
            (";; [ACTUAL-FORM]")
            ("~S" (quote ,actual-form))
            ("")
            (";; [EXPECT-CONDITION]")
            ("~S" (quote ,expect-condition))
            ("")
            (";; [ACTUAL-CONDITION]")
            ("~A" condition)
            ("#+end_src")))))

     (:no-error (actual-value-list)
       (cat (:trim (cat () ("~%")))
         ((cat (:postfix (cat () ("~%")))
            ("#+begin_src lisp")
            (";; [ACTUAL-FORM]")
            ("~S" (quote ,actual-form))
            ("")
            (";; [EXPECT-CONDITION]")
            ("~S" (quote ,expect-condition))
            ("")
            (";; [ACTUAL-VALUE]")
            ("~{~S~^~%~10T~}" actual-value-list)
            ("#+end_src")))))))

;; (ensure
;;     (string->head#char "")
;;     signals
;;     type-error)

;; (ensure
;;     (string->head#char "")
;;     signals
;;     error)

;; (ensure
;;     (string->head#char "")
;;     signals
;;     simple-error)
;; (multiple-value-bind
;;       (success?
;;        report-string)
;;     (ensure string)
;;   '><><><)

(defmacro ensure (left-expression
                  infix-notation
                  right-expression)
  (cond
    ((string-equal infix-notation '==>)
     `(help#==>#ensure :actual-form ,left-expression
                       :expect-form ,right-expression))

    ((string-equal infix-notation 'signals)
     `(help#signals#ensure :actual-form ,left-expression
                           :expect-condition ,right-expression))

    (:else
     (error "unknown infix-notation of the macro (ensure)"))
    ))
(defun all-tests (group)
  (let* ((group (find-test-group group))
         (tests (loop for x being the hash-values of (test-group-tests group)
                   collecting x))
         (constraints '()))
    (dolist (test tests)
      (dolist (val (test-after test))
        (push (cons val (test-name test)) constraints))
      (dolist (val (test-after-pass test))
        (push (cons val (test-name test)) constraints))
      (dolist (val (test-after-fail test))
        (push (cons val (test-name test)) constraints)))
    (help#topological-sort#all-tests (map-into tests #'test-name tests) constraints
                      (lambda (x y)
                        (declare (ignore y))
                        (first (stable-sort (copy-seq x) #'<
                                            :key (lambda (name)
                                                   (test-priority
                                                    (gethash name (test-group-tests group))))))))))

(defun help#topological-sort#all-tests (elements constraints tie-breaker)
  (let ((result '()))
    (loop
       (let* ((rhs (mapcar #'cdr constraints))
              (elts (remove-if (lambda (x) (member x rhs)) elements)))
         (when (null elts)
           (if elements
               (error "Inconsistent constraints in ~S" 'help#topological-sort#all-tests)
               (unless elements (return (nreverse result)))))
         (let ((elt
                (if (cdr elts) (funcall tie-breaker elts result) (car elts))))
           (push elt result)
           (setq elements (delete elt elements))
           (setq constraints (delete-if (lambda (x)
                                          (or (eq (car x) elt)
                                              (eq (cdr x) elt)))
                                        constraints)))))))
(defvar *break-on-fail* nil)
(defun run-unit
    (group &key
             (skip nil)
             (break-on-fail *break-on-fail*))
  (let ((group (find-test-group group))
        (passed '())
        (failed '()))
    (when (test-group-pre group)
      (funcall (test-group-pre group)))
    (unwind-protect
         (dolist (name (all-tests group))
           (tagbody
            try-again
              (let ((test (gethash name (test-group-tests group))))
                (unless (or (member name skip)
                            (and (test-when test)
                                 (not (funcall (test-when test))))
                            (and (test-unless test)
                                 (funcall (test-unless test)))
                            (set-difference (test-after-pass test) passed)
                            (set-difference (test-after-fail test) failed))

                  (multiple-value-bind
                        (pass?
                         report-string
                         time)
                      (help#do-test#run-unit test)

                    ;; about break-on-fail
                    (when (and break-on-fail (not pass?))
                      (restart-case
                          (break "Test ~A failed with BREAK-ON-FAIL set."
                                 name)
                        (try-again ()
                          :report "Try the test again."
                          (go try-again))))

                    ;; main report
                    (cond ((not pass?)
                           (push name failed)
                           (cat (:to *standard-output*
                                     :postfix (cat () ("~%")))
                             ("* >< ~A" name)
                             ("  * failed"))
                           (edit#line-list
                            :print-to *standard-output*
                            :indent 4
                            :line-list
                            (string->list#line (cat () ("~A" report-string)))))
                          (:else
                           (push name passed)
                           (cat (:to *standard-output*
                                     :postfix (cat () ("~%")))
                             ("* ~A" name))))

                    ;; about time used
                    ;; (multiple-value-bind (hours time) (floor time 3600)
                    ;;   (multiple-value-bind (minutes seconds) (floor time 60)
                    ;;     (format t "~47T[~2,'0D:~2,'0D:~5,2,,,'0F]~%"
                    ;;             hours minutes seconds)))

                    )))))

      (when (test-group-post group)
        (funcall (test-group-post group))))

    (let ((pass (length passed))
          (fail (length failed))
          (total (hash-table-count (test-group-tests group))))
      (format t "~2&Ran ~D of ~D test~:P in group ~S~%" (+ pass fail) total
              (test-group-name group))
      (when failed
        (format t "~&The following tests failed:~%  ~S~%" failed))
      (format t "~2&Totals -- Passed: ~D~25T~3D%~&~10TFailed: ~D~25T~3D%~%"
              pass (round (* 100 pass) total)
              fail (round (* 100 fail) total)))

    (null failed)
    ))



;; interface:
;; (multiple-value-bind
;;       (pass?
;;        report-string
;;        time)
;;     (help#do-test#run-unit test)
;;   '><><><)
(defun help#do-test#run-unit (test)
  (let ((time (get-internal-run-time)))
    (multiple-value-bind
          (success?
           report-string)
        (ignore-errors
          (funcall (test-fn test)))
      (values success?
              report-string
              (/ (float (- (get-internal-run-time) time) 1f0)
                 (float internal-time-units-per-second 1f0))))))
