(in-package #:3b-walker)

(defgeneric filter-ast (node walker)
  (:documentation "Walk AST tree NODE, returning a new AST containing
values returned by filter-ast. (may share structure with original AST?)

 WALKER should be an instance of a user class to distinguish the
particular code walker (and if needed, store state)"))

(defmethod filter-ast :around (node (walker symbol))
  ;; shortcut to allow using a class name insteas of instance for walker
  ;; if it doesn't need initargs
  (filter-ast node (make-instance walker)))

(defmethod filter-ast (node walker)
  (if (or (keywordp node)
          (null node)
          (and (atom node)
               (not (symbolp node))))
      node
      (error "todo: walk ~s" node)))

(defun list-eql (a b)
  (loop for (x . am) on a
        for (y . ym) on b
        always (and (eql x y)
                    (or (and am ym)
                        (not (or am ym))))))
(defmacro define-filter-method (() class &rest slots)
  (let* ((next nil)
         (slots (loop for s in slots
                      collect
                      (cond
                        ((atom s)
                         (case s
                           ;; shortcut for call-next-method
                           ;; (possibly should require it to be last
                           ;; argument, since it always happens
                           ;; last?
                           (:next
                            (setf next t)
                            :next)
                           ;; add some shortcuts for common slots
                           ((args body bindings) (list :list s))
                           (t (list :walk s))))
                        ((= 1 (length s))
                         (error "missing walker in slot ~s?" s))
                        ;; otherwise keep what user supplied
                        (t s))))
         (walkers '(:walk filter-ast
                    :list filter-list))
         (tests '(:walk eql
                  :list list-eql)))
    `(defmethod filter-ast ((node ,class) walker)
       ,(if (null slots)
            'node
            `(let ((init nil))
               ,@(loop for s in slots
                       unless (eql s :next)
                       collect (destructuring-bind (f s) s
                                 `(let ((a (,(getf walkers f)
                                            (,s node) walker)))
                                    (unless (,(getf tests f) a (,s node))
                                      (push (list ',s a) init)))))
               (if init
                   (let ((new (copy-node node)))
                     (loop for (s v) in init
                           do (setf (slot-value new s) v))
                     (ast->sexp new)
                     ,(if next `(call-next-method new) `new))
                   ,(if next `(call-next-method) `node)))))))


(defmacro define-filter-methods (()
                                 (class &body slots)
                                 &body more)
  `(progn
     (define-filter-method () ,class ,@slots)
     ,@ (loop for (c . s) in more
              collect `(define-filter-method () ,c ,@s))))

(defun filter-list (list walker)
  (loop for a in list
        when (filter-ast a walker)
          collect it))

(define-filter-methods ()
    (initialized-variable-binding initform)
  (implicit-progn (:list body))
  (variable-reference)
  (constant-reference)
  (symbol-macro-reference expansion)
  (lambda-function (:list bindings) :next)
  (lambda-application (:list args) func)
  (function-application (:list args))
  ;; todo: compiler-macro-application
  (macro-application expansion)
  (special-form-block :next)
  (special-form-catch tag-ast :next)
  (special-form-eval-when :next)
  (special-form-flet (:list bindings) :next)
  (special-form-function)
  (special-form-go)
  (special-form-if test then else)
  (special-form-labels (:list bindings) :next)
  (special-form-let (:list bindings) :next)
  (special-form-let* (:list bindings) :next)
  (special-form-load-time-value form)
  (special-form-locally :next)
  (special-form-macrolet :next)
  (special-form-multiple-value-call function-form :next)
  (special-form-multiple-value-prog1 first-form :next)
  (special-form-progn :next)
  (special-form-progv symbols-ast values-ast :next)
  (special-form-quote)
  (special-form-return-from result)
  (special-form-setq (:list value-asts))
  (special-form-symbol-macrolet :next)
  (go-tag-binding)
  (unknown-go-tag-binding)
  (special-form-tagbody (:list body))
  (special-form-the form)
  (special-form-throw tag result)
  (special-form-unwind-protect protected-form :next))

(defmethod filter-ast :around ((node form-with-scope) walker)
  (let ((*lexical-environment* (cons (env node) *lexical-environment*)))
    (call-next-method)))

;;; utilities for modifying AST during filter-ast

;; todo: util for modifying let* bindings, since it needs to be
;; careful about scope if it adds? or maybe add special filter for
;; let* that builds up lexenv properly as it walks the bindings?


(defun make-variable-reference (value)
  (sexp->ast value))

(defun make-function-call (op &rest args)
  (labels ((unast (i)
             (if (typep i 'ast-node)
                 (or (whole i) (ast->sexp i))
                 i))
           (unast* (a)
             (loop for i in a collect (unast a)))
           (w ()
             (list* (unast op) (unast* args))))
    (etypecase op
      (lambda-function
       (make-instance 'lambda-application
                      :func op
                      :args (mapcar 'sexp->ast args)
                      :whole (w)))
      ((cons (eql lambda))
       (make-instance 'lambda-application
                      :func (sexp->ast op)
                      :args (mapcar 'sexp->ast args)
                      :whole (w)))
      (symbol
       (make-instance 'function-application
                      :binding (lookup-function op)
                      :args (mapcar 'sexp->ast args)
                      :whole (w)))
      (function-binding
       (make-instance 'function-application
                      :binding op
                      :args (mapcar 'sexp->ast args)
                      :whole (w))))))






