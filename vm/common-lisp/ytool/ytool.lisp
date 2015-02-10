(in-package :cicada-vm)
(defun series (l &optional h i)
  (cond ((null i)
         (setf i 1)
         (cond ((null h) (setf h l) (setf l 1))   ))   )
  (cond ((> l h) nil)
        (t (make-series l h i))
        ))

(defun make-series (l h i)
  (declare (fixnum l h i))
  (let ((ans (list l)))
    (do ((tail ans)
         (l (+ l i) (+ l i)))
        ((> l h) ans)
      (setf (cdr tail) (list l))
      (setf tail (cdr tail))
      )))
(defun memq (x l) (member x l :test #'eq))
(defun assq (k al) (assoc k al :test #'eq))
(cl:eval-when (:compile-toplevel :load-toplevel :execute)

  (cl:defun ignore-smooth (args body)
    (multiple-value-bind (newargs to-be-ignored)
        (underscores-elim args)
      (let ((realbod (ignore-convert body)))
        (cond ((null to-be-ignored)
               (values args realbod))
              (t
               (values newargs `((declare (cl:ignore ,@to-be-ignored))
                                 ,@realbod)))))))

  (cl:defun underscores-elim (args)
    (let ((realargs '())
          (new-ignores '())
          (keyargs nil))
      (dolist (a args)
        (labels ((got-underscore ()
                   (let ((new (gensym)))
                     (push new new-ignores)
                     (push (subst new '_ a)
                           realargs))))
          (cond ((eq a '_)
                 (got-underscore))
                ((consp a)
                 (cond (keyargs
                        (cond ((or (eq (car a) '_)
                                   (and (consp (car a))
                                        (eq (cadr (car a))
                                            '_)))
                               (got-underscore))
                              (t
                               (push a realargs))))
                       ((eq (car a) '_)
                        (got-underscore))
                       (t
                        (push a realargs))))
                (t
                 (cond ((eq a '&key)
                        (setq keyargs t)))
                 (push a realargs)))))
      (values (reverse realargs)
              new-ignores)))

  (cl:defun ignore-convert (body)
    (cond ((and (not (null body))
                (consp (car body))
                (eq (caar body) 'ignore))
           `((declare (cl:ignore ,@(cdar body))) ,@(cdr body)))
          (t body)   ))

  )
;; rename multiple-value-bind
(defmacro multiple-value-let (vars e &rest l)
  (multiple-value-bind (vars l)
      (ignore-smooth vars l)
    `(multiple-value-bind ,vars ,e ,@l)))
;; prettier than LABELS
;; in addition to clauses at the front
;; allows :WHERE (local-fun1 ...) (local-fun2 ...)
(defmacro help (clauses &rest body)
  (let-fun-expand 'labels clauses body))

(defmacro let-fun-nonrec (clauses &body body)
  (let-fun-expand 'flet clauses body))

(defun let-fun-expand (binder clauses body)
  (multiple-value-let (clauses body _ _)
    (extract-where clauses body)
    (setq clauses
          (mapcar (lambda (c)
                    ;; (format t "c = ~s~%" c)
                    ;; use defun instead of :def
                    (cond ((eq (car c) 'defun)
                           (setq c (cdr c))))
                    (multiple-value-bind (args body)
                        (ignore-smooth (cadr c)
                                       (cddr c))
                      `(,(car c) ,args ,@body)))
                  clauses))
    `(,binder ,clauses
              ,@body)))

(defmacro let-var (clauses &rest body)
  (multiple-value-let (clauses body _ _)
    (extract-where clauses body)
    `(let ,@clauses
       ,@body)))

;;; returns < aug-bdgs, truncated-body, positions, wheres >
;;; aug-bdgs = bdgs + wheres, truncated-body = body-minus-wheres,
;;; positions = list of pairs (p1 p2) and numbers p
;;;    giving positions of all aug-bdgs; (p1 p2) is for bdgs, p's for
;;;    wheres.
;;; wheres = stuff starting with ':where' flag
(defun extract-where (bdgs body &key (offset 1))
  (let ((more (memq ':where body))
        (normal-bdgs-rels
         (mapcar (lambda (i) `(,offset ,i))
                 (series 0 (- (length bdgs) 1)))))
    (cond (more
           (let ((length#body (length body)))
             (values (append bdgs (cdr more))
                     (ldiff body more)
                     `(,@normal-bdgs-rels
                       ,@(mapcar (lambda (i) (+ offset
                                                1
                                                length#body
                                                (- (length (cdr more)))
                                                i))
                                 (series 0 (- (length (cdr more)) 1))))
                     more)))
          (:else
           (values bdgs body normal-bdgs-rels '())))))
