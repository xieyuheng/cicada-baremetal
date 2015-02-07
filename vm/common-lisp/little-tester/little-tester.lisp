(in-package :cicada-vm)
(defun required-argument ()
  (error "A required argument was not supplied."))

(defstruct (test-group (:constructor make-test-group
                                     (name &key
                                           pre
                                           post
                                           docstring))
                       (:print-function %print-test-group))
  (name (required-argument)
        :type symbol
        :read-only t)
  (docstring nil
             :type (or null simple-base-string)
             :read-only t)
  (tests (make-hash-table)
         :type hash-table
         :read-only t)
  (pre nil
       :type (or null function))
  (post nil
        :type (or null function)))

(defun %print-test-group (group stream depth)
  (declare (ignore depth))
  (print-unreadable-object (group stream :type t :identity t)
    (format stream "~S, ~D tests" (test-group-name group)
            (hash-table-count (test-group-tests group)))))
(defun find-test-group (name &optional create)
  (if (test-group-p name)
      name
      (let ((group (get name 'tests)))
        (cond (group group)
              (create
               (setf (get name 'tests)
                     (make-test-group name)))))))

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
(defstruct (test (:constructor
                  make-test
                  (name fn
                        &key
                        after
                        after-pass
                        after-fail
                        when
                        unless
                        priority
                        docstring))
                 (:print-function %print-test))
  (name (required-argument)
        :type symbol
        :read-only t)
  (docstring nil
             :type (or null simple-base-string)
             :read-only t)
  (fn (required-argument)
      :type function
      :read-only t)
  (priority 0
            :type fixnum)
  (after '()
         :type list)
  (after-pass '()
              :type list)
  (after-fail '()
              :type list)
  (when  nil  :type (or null function))
  (unless nil :type (or null function))
  )

(defun %print-test (test stream depth)
  (declare (ignore depth))
  (print-unreadable-object (test stream :type t :identity t)
    (princ (test-name test) stream)))
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
             ,@body))
         (let ((,hash (test-group-tests (find-test-group (quote ,group) t))))
           (when (gethash (quote ,test-name) ,hash) (warn "Redefining test ~A." (quote ,test-name)))
           (setf (gethash (quote ,test-name) ,hash)
                 (make-test (quote ,test-name)
                            (function ,test-function-name)
                            ,@keys)))
         (quote ,test-name)))))


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
(defparameter *report-time?* nil)

(defun run-unit
    (group &key
             (skip nil))
  (let* ((group (find-test-group group))
         (passed '())
         (failed '())
         (report-pathname
          (cat (:letter :small)
            ("home:.unit-test-report-center;")
            ("~A-report.org" (test-group-name group)))
          ;; (merge-pathnames (make-pathname
          ;;                   :directory ".unit-test-report-center"
          ;;                   :name (cat (:letter :small)
          ;;                           ("~A" (test-group-name group))
          ;;                           (".unit-test-report.org")))
          ;;                  (user-homedir-pathname))
           )
         (report-stream (open report-pathname
                              :direction :output
                              :if-exists :supersede)))

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

                    ;; about time used
                    (let ((test-time-string
                           (if (not *report-time?*)
                               ""
                               (multiple-value-bind (hours time) (floor time 3600)
                                 (multiple-value-bind (minutes seconds) (floor time 60)
                                   (cat ()
                                     ("[~2,'0D:~2,'0D:~5,2,,,'0F]"
                                      hours minutes seconds)))))))
                      ;; main report
                      (cond ((not pass?)
                             (push name failed)
                             (cat (:to report-stream
                                       :postfix (cat () ("~%"))
                                       :letter :small)
                               ("* >< ~A  ~A" name test-time-string)
                               ("  * fail"))
                             (edit#line-list
                              :print-to report-stream
                              :indent 4
                              :line-list
                              (string->list#line (cat () ("~A" report-string)))))
                            (:else
                             (push name passed)
                             (cat (:to report-stream
                                       :postfix (cat () ("~%"))
                                       :letter :small)
                               ("* ~A  ~A" name test-time-string)))))
                    )))))

      (when (test-group-post group)
        (funcall (test-group-post group))))

    (let ((pass (length passed))
          (fail (length failed))
          (total (hash-table-count (test-group-tests group))))

      (cat (:to *standard-output*
                :postfix (cat () ("~%"))
                :letter :small)
        ("")
        ("* [unit] ~S" (test-group-name group))
        ("  * report overview")
        ("    |      | number | percent |")
        ("    | pass | ~6D |   ~3D % |" pass (round (* 100 pass) total))
        ("    | fail | ~6D |   ~3D % |" fail (round (* 100 fail) total))
        ("  * report detail write to")
        ("    ~A" report-pathname))        


      (when failed
        (cat (:to *standard-output*
                  :postfix (cat () ("~%"))
                  :letter :small)
          ("  * the following tests failed")
          ("    ~A" failed))))

    (close report-stream)

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
