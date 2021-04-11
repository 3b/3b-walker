#++ (ql:quickload '3b-walker)
(in-package #:3b-walker)

;;; bindings in lexical environment

(defclass binding ()
  ((name :reader name :initarg :name)
   ;; link to environment containing this binding
   (context :Reader context :initarg :context :initform nil)))

(defclass variable-binding (binding)
  ())

(defclass free-variable (variable-binding)
  ())

(defclass host-variable (variable-binding)
  ;; variable defined in CL code, get it's value from host when accessed?
  ())

(defclass initialized-variable-binding (variable-binding)
  ((initform :accessor initform :initarg :initform :initform nil)))

;; for lambda-list vars, let, let*, etc
(defclass scoped-variable-binding (initialized-variable-binding)
  ((specialp :reader specialp :initform nil :initarg :special)))

;;; subclasses for lambda-list variables so we can try to expand them
;;; back to sexps
(defclass required-variable-binding (scoped-variable-binding)
  ())

;; bindings with (optional) associated foo-p var (optional, key)
(defclass scoped-variable-binding-with-p (scoped-variable-binding)
  ((pvar-binding :reader pvar-binding :initform nil :initarg :pvar)))

(defclass suppliedp-variable-binding (scoped-variable-binding)
  ())

(defclass optional-variable-binding (scoped-variable-binding-with-p)
  ())

(defclass rest-variable-binding (scoped-variable-binding)
  ())

(defclass key-variable-binding (scoped-variable-binding-with-p)
  ((keyname :reader keyname :initarg :keyname)))

(defclass aux-variable-binding (scoped-variable-binding)
  ())

(defclass constant-binding (variable-binding)
  ((value :accessor value :initarg :value)))

(defclass symbol-macro-binding (variable-binding)
  ((expansion :accessor expansion :initarg :expansion)))

(defclass function-binding (binding)
  ())

(defclass unknown-function (binding)
  ())

(defclass host-function (function-binding)
  ;; designator for host function, since it might not match name used
  ;; inside a dsl
  ((designator :Reader designator :initarg :designator)
   (cmacro :reader cmacro :initarg :compiler-macro)))

(defclass host-macro (function-binding)
  ((cmacro :reader cmacro :initarg :compiler-macro)
   (expander :reader expander :initarg :expander))
  (:default-initargs :context "host macro"))

(defclass implicit-progn ()
  ;; ast of body of an implicit (or explicit) progn
  ((body :reader body :initarg :body)))

(defclass form-with-declarations ()
  ((declarations :accessor declarations :initarg :declarations)))

(defclass form-with-docstring ()
  ((docstring :accessor docstring :initarg :docstring)))

(defclass lambda-function (implicit-progn
                           form-with-declarations
                           form-with-docstring
                           form-with-scope)
  ;; original lambda-list
  ((lambda-list :accessor lambda-list :initarg :lambda-list)
   ;; list of variable-binding objects
   (bindings :accessor bindings :initarg :bindings)))

;; not sure if we need to distinguish local and global functions?
;; possibly inline local functions if only used once?
(defclass local-function (function-binding lambda-function)
  ())

(defclass global-function (function-binding lambda-function)
  ())

(defclass macro-binding (function-binding)
  ((expander :accessor expander :initarg :expander)
   ;; original macro definition, for converting back to SEXPs
   (whole :reader whole :initarg :whole)))


;; binding in block namespace
(defclass block-binding (binding)
  ;; add link back to block ast?
  ())

(defclass go-tag-binding (binding)
  ;; add link back to tagbody ast?
  ())

(defclass unknown-go-tag-binding (go-tag-binding)
  ())

;;; lexical environment

(defclass environment ()
  ((variables :reader variables :initform (make-hash-table))
   (functions :reader functions :initform (make-hash-table))
   ;; possibly should combine with FUNCTIONS slot using an equal hash?
   (setf-functions :reader setf-functions :initform (make-hash-table))))

(defclass lexical-environment (environment)
  ((blocks :reader blocks :initform (make-hash-table))
   (lock :reader lock :initform nil :initarg :lock)
   ;; link to ast node defining this env
   (context :Reader context :initform nil :initarg :context)))

(defclass host-environment (environment)
  ;; optional env that just looks things up in (global) host env
  ())


(defvar *lexical-environment* nil)
;; not sure if this should be separate or not? don't really want to
;; add free vars to closest enclosing scope, since probably want to
;; treat all refs to same name within a function as references to a
;; single free var, but also don't really want to keep them around
;; forever like putting them in a global env does.
(defvar *undefined-environment* (make-instance 'lexical-environment))

(defun add-host-macro (name &key (env (car *lexical-environment*))
                              alias)
  "Add host macro defined by NAME to ENV or current environment as
NAME, or ALIAS if provided."
  (setf (gethash (or alias name) (functions env))
        (make-instance
         'host-macro
         :name (or alias name)
         :expander (macro-function name)
         :compiler-macro (compiler-macro-function name))))

(defun add-host-function (name &key (env (car *lexical-environment*))
                                 alias)
  "Add host function defined by NAME to ENV or current environment as
NAME, or ALIAS if provided."
  (setf (gethash (or alias name) (functions env))
        (make-instance
         'host-function
         :name (or alias name)
         :designator name
         :compiler-macro (compiler-macro-function name))))

(defmacro with-host-environment (() &body body)
  `(let ((*lexical-environment*
           (append *lexical-environment*
                   (list (make-instance 'host-environment)))))
     ,@body))

(defun make-cl-environment ()
  (let ((env (make-instance 'lexical-environment :lock t :context "CL")))
    (loop for s being the external-symbols of (find-package :cl)
          for ss = `(setf ,s)
          when (boundp s)
            do (cond
                 ((constantp s)
                  (setf (gethash s (variables env))
                        (make-instance 'constant-binding
                                       :name s
                                       :value (symbol-value s))))
                 (t
                  (setf (gethash s (variables env))
                        (make-instance 'host-variable
                                       :name s))))
          when (fboundp s)
            do (cond
                 ((macro-function s)
                  (add-host-macro s :env env))
                 (t
                  (add-host-function s :env env)))
          when (fboundp ss)
            do (setf (gethash s (setf-functions env))
                     (make-instance
                      'host-function
                      :name ss
                      :compiler-macro (compiler-macro-function ss))))
    env))

(setf *lexical-environment* (or *lexical-environment*
                                (list (make-cl-environment))))

(defmethod %lookup-variable (name env)
  (gethash name (variables env)))

(defmethod %lookup-variable (name (env host-environment))
  (or (gethash name (variables env))
      (and (boundp name)
           (setf (gethash name (variables env))
                 (make-instance 'host-variable
                                :name name
                                :context env)))))

(defun lookup-variable (name)
  (or (loop for e in *lexical-environment*
            when (%lookup-variable name e)
              return it)
      (gethash name (variables *undefined-environment*))
      (setf (gethash name (variables *undefined-environment*))
            (make-instance 'free-variable :name name))))

(defun make-constant ())


(defmethod %lookup-function (name env)
  (gethash name (functions env)))

(defmethod %lookup-function (name (env host-environment))
  (or (gethash name (functions env))
      (and
       (fboundp name)
       (cond
         ((macro-function name)
          (make-instance 'host-macro
                         :name name
                         :context env
                         :compiler-macro (compiler-macro-function name)
                         :expander (macro-function name)))
         ((special-operator-p name)
          (error "unrecognized special operator ~s in host env?" name))
         (t
          (setf (gethash name (functions env))
                (make-instance 'host-function
                               :name name
                               :context env
                               :compiler-macro (compiler-macro-function name)
                               :designator name)))))))

(defun lookup-function (name)
  #++
  (format t "lookup ~s -> ~s~%"
          name
          (loop for e in *lexical-environment*
                when (%lookup-function name e)
                  return it))
  (or (loop for e in *lexical-environment*
            when (%lookup-function name e)
              return it)
      (gethash name (functions *undefined-environment*))
      (setf (gethash name (functions *undefined-environment*))
            (make-instance 'unknown-function :name name))))

;; todo: allow setting block
(defun push-lexical-environment ()
  (push (make-instance 'lexical-environment) *lexical-environment*))

(defun pop-lexical-environment ()
  (when (cdr *lexical-environment*)
    (pop *lexical-environment*)))

(defmacro with-lexical-environment (() &body body)
  `(unwind-protect
        (flet ((set-context (a)
                 (let ((e (car *lexical-environment*)))
                   (assert (typep a 'form-with-scope))
                   (assert e)
                   (setf (slot-value a 'env) e)
                   (setf (slot-value e 'context) a))))
          (declare (ignorable #'set-context))
          (progn
            (push-lexical-environment)
            ,@body))
     (pop-lexical-environment)))

(defun make-lexical-binding (name &key init
                                    (type 'scoped-variable-binding)
                                    pvar kvar
                                    context)
  #++(format t "make lexical var ~s = ~s~%" name type)
  (make-instance type :name name
                      :initform init
                      :allow-other-keys t
                      :pvar pvar
                      :keyname (or kvar name)
                      :context context))

(defun add-lexical-variable (name &key init
                                    (type 'scoped-variable-binding)
                                    pvar kvar)
  ;; for now, not considering it an error to add a variable twice,
  ;; since LET* might do that
  (let ((env (car *lexical-environment*)))
    (assert env)
    (assert (not (lock env)))
    (setf (gethash name (variables env))
          (make-lexical-binding name :init init :type type
                                     :pvar pvar :kvar kvar
                                     :context env))))

(defun add-lexical-variable* (binding)
  ;; for now, not considering it an error to add a variable twice,
  ;; since LET* might do that
  (let ((env (car *lexical-environment*)))
    (assert env)
    (assert (not (lock env)))
    #++
    (format t "add lexical binding* ~s ~s~%" (name binding) (type-of binding))
    (assert (not (context binding)))
    (setf (slot-value binding 'context) env)
    (setf (gethash (name binding) (variables env)) binding)))

(defun add-lexical-function* (binding)
  (let ((env (car *lexical-environment*)))
    (assert env)
    (assert (not (lock env)))
    ;; don't think this should allow redefinition in 1 scope?
    (assert (not (gethash (name binding) (functions env))))
    (setf (slot-value binding 'context) env)
    (setf (gethash (name binding) (functions env)) binding)))


(defun add-block-binding (block-name)
  (let ((env (car *lexical-environment*)))
    (assert env)
    (assert (not (lock env)))
    (setf (gethash block-name (blocks env))
          (make-instance 'block-binding :name block-name))))

(defun lookup-block (block-name)
  (gethash block-name (blocks (car *lexical-environment*))
           (make-instance 'block-binding
                          :name (list :unknown-block block-name))))

(defun add-go-tag (tag)
  ;; todo: go tag namespace
  (make-instance 'go-tag-binding :name tag))

(defun lookup-go-tag (tag)
  ;; todo: go tag namespace
  (make-instance 'go-tag-binding :name tag))


;;; AST classes

;; parent class of evaluated nodes in AST
;; (currently self-evaluating nodes (keywords, numbers, etc) might be
;; included directly in ast rather than being wrapped in an ast-node)
(defclass ast-node ()
  ())

;; an evaluated symbol
(defclass variable-reference (ast-node)
  ((name :accessor name :initarg :name)
   (binding :accessor binding :initarg :binding)))

;; subclass to distinguish references to CONSTANT-BINDING, since we
;; might want to process them separately
(defclass constant-reference (variable-reference)
  ())

;; preserve symbol macros in AST so we can convert back to the
;; original if desired
(defclass symbol-macro-reference (variable-reference)
  ;; original expansion, so we can detect if AST was changed when
  ;; converting back to sexps
  ((original-form :reader original-form :initarg :original-form)
   ;; AST of expansion
   (expansion :reader expansion :initarg :expansion)))

;; a cons to be evaluated
(defclass application (ast-node)
  ;; original sexp form
  ((whole :reader whole :initarg :whole)))

;; ((lambda (..) ...) ...) forms
(defclass lambda-application (application)
  ;; lambda-function instance
  ((func :reader func :initarg :func)
   ;; list of ASTs for the arguments
   (args :reader args :initarg :args)))

(defclass normal-application (application)
  ;; ref to function etc being called
  ((binding :reader binding :initarg :binding)))

(defclass function-application (normal-application)
  ;; list of ASTs for the arguments
  ((args :reader args :initarg :args)))

;; not sure if this should always be used when compiler macro exists,
;; or only when it returns a modified form?
(defclass compiler-macro-application (normal-application)
  ;; original expansion in sexp form, for detecting changes
  ((original-expansion :reader original-expansion :initarg :original-expansion)
   ;; AST of expansion
   (expansion :reader expansion :initarg :expansion)))

(defclass macro-application (normal-application)
  ;; original expansion in sexp form, for detecting changes
  ((original-expansion :reader original-expansion :initarg :original-expansion)
   ;; AST of expansion
   (expansion :reader expansion :initarg :expansion)))

(defclass form-with-scope()
  ((env :reader env :initform nil)))

(defclass special-form-application (application)
  ())


(defclass special-form-catch (special-form-application
                              implicit-progn)
  ;; ast to generate catch tag
  ((tag-ast :reader tag-ast :initarg :tag-ast)))

(defclass special-form-eval-when (special-form-application
                                  implicit-progn)
  ((situations :reader situations :initarg :situations)))

(defclass special-form-function (special-form-application)
  ;; function-binding or lambda-function instance?
  ((binding :reader binding :initarg :binding)))

(defclass special-form-go (special-form-application)
  ;; go-tag-binding containing symbol or integer
  ((tag :reader tag :initarg :tag)))

(defclass special-form-if (special-form-application)
  ;; ASTs for test/then/else (or NIL for missing else form?)
  ((test :reader test :initarg :test)
   (then :reader then :initarg :then)
   (else :reader else :initarg :else)
   ;; store whether ELSE branch was supplied so we can reconstruct
   ;; original form more closely (needed to detect expansion of WHEN
   ;; hasn't changed)
   (elsep :reader elsep :initarg :elsep)))

(defclass special-form-load-time-value (special-form-application)
  ;; ast of body form
  ((form :reader form :initarg :form)
   ;; T or NIL
   (read-only :reader read-only :initarg :read-only)))

(defclass special-form-multiple-value-call (special-form-application
                                            implicit-progn)
  ;; ast that evaluates to function to call
  ((function-form :reader function-form :initarg :function-form)))

(defclass special-form-multiple-value-prog1 (special-form-application
                                             implicit-progn)
  ;; ast of first form (not sure if this is actually needed, or just
  ;; grab car of body?)
  ((first-form :reader first-form :initarg :first-form)))

(defclass special-form-progn (special-form-application
                              implicit-progn)
  ())


(defclass special-form-progv (special-form-application
                              implicit-progn)
  ;; ast that evaluates to list of symbols to bind
  ((symbols-ast :reader symbols-ast :initarg :symbols-ast)
   ;; ast that evaluates to values to bind to symbols
   (values-ast :reader values-ast :initarg :values-ast)))

(defclass special-form-quote (special-form-application)
  ;; literal object, not evaluated
  ((object :reader object :initarg :object)))

(defclass special-form-return-from (special-form-application)
  ;; block binding from which to return
  ((binding :reader binding :initarg :binding)
   ;; ast of result form, or NIL
   (result :reader result :initarg :result)))

(defclass special-form-the (special-form-application)
  ;; type specifier
  ((typespec :reader typespec :initarg :typespec)
   ;; ast of form
   (form :reader form :initarg :form)))

(defclass special-form-throw (special-form-application)
  ;; AST that returns tag
  ((tag :reader tag :initarg :tag)
   ;; ast for result for
   (result :reader result :initarg :result)))

(defclass special-form-unwind-protect (special-form-application implicit-progn)
  ;; AST for protected form (cleanups in implicit-progn body)
  ((protected-form :reader protected-form :initarg :protected-form)))


(defclass special-form-setq (special-form-application)
  ;; list of variable bindings (possibly should have single list of
  ;; binding+ast pairs?)
  ((variables :reader variables :initarg :variables)
   ;; list of ASTs of corresponding values
   (value-asts :reader value-asts :initarg :value-asts)))

;; adds to block namespace
(defclass special-form-block (special-form-application
                              implicit-progn)
  ;; ref to block-binding in lexical-env
  ((binding :reader binding :initarg :binding)))


;; adds to function namespace, declarations?
(defclass special-form-flet (special-form-application
                             implicit-progn
                             form-with-declarations
                             form-with-scope)
  ;; list of FUNCTION-BINDING objects
  ((bindings :reader bindings :initarg :bindings)))

(defclass special-form-labels (special-form-application
                               implicit-progn
                               form-with-declarations
                               form-with-scope)
  ;; list of FUNCTION-BINDING objects
  ((bindings :reader bindings :initarg :bindings)))

(defclass special-form-macrolet (special-form-application
                                 implicit-progn
                                 form-with-declarations
                                 form-with-scope)
  ;; list of MACRO-BINDING objects
  ((bindings :reader bindings :initarg :bindings)))


;; adds to variable namespace, declarations?
(defclass special-form-let (special-form-application
                            implicit-progn
                            form-with-declarations
                            form-with-scope)
  ;; list of SCOPED-VARIABLE-BINDING objects
  ((bindings :reader bindings :initarg :bindings)))

(defclass special-form-let* (special-form-application
                             implicit-progn
                             form-with-declarations
                             form-with-scope)
  ;; list of SCOPED-VARIABLE-BINDING objects
  ((bindings :reader bindings :initarg :bindings)))

(defclass special-form-symbol-macrolet (special-form-application
                                        implicit-progn
                                        form-with-declarations
                                        form-with-scope)
  ;; list of SYMBOL-MACRO-BINDING objects
  ((bindings :reader bindings :initarg :bindings)))

;; adds to declarations
(defclass special-form-locally (special-form-application
                                implicit-progn
                                form-with-declarations)
  ())


;; adds to list of valid GO tags?
(defclass special-form-tagbody (special-form-application
                                form-with-scope)
  ;; similar to implicit-progn, except tags are preserved as-is
  ((body :reader body :initarg :body)))



;;; conversion APIs

;; user API
(defgeneric sexp->ast (form &key))
(defun ast->sexp (ast &key restore-macros)
  (walk-ast ast (make-instance 'ast->sexp :collapse-macros restore-macros)))

;; internal / extension API
(defgeneric cons->ast (car form))


;; helper to call sexp->ast on a list of forms
(defun progn->ast (forms)
  (mapcar 'sexp->ast forms))

;; for now, keywords and non-symbol atoms are kept as-is in AST, but
;; NIL gets a constant-ref wrapper so filter can distinguish it from
;; something trying to remove a form from a list
(defmethod sexp->ast ((form null) &key)
  (make-instance 'constant-reference
                 :binding (lookup-variable nil)
                 :name nil))

(defmethod sexp->ast ((form t) &key)
  form)

(defmethod sexp->ast ((form symbol) &key)
  (cond
    ;; not sure if keywords should be 'constants' or just stay keywords?
    ((keywordp form) form)
    ;; treat any other symbol as a variable reference
    ((lookup-variable form)
     (let ((b (lookup-variable form)))
       (etypecase b
         (constant-binding
          (make-instance 'constant-reference :binding b :name form))
         ((or free-variable scoped-variable-binding host-variable)
          (make-instance 'variable-reference :binding b :name form))
         (symbol-macro-binding
          (make-instance 'symbol-macro-reference :binding b :name form
                                                 :expansion (sexp->ast (expansion b))
                                                 :original-form form)))))))


;; for conses, dispatch to cons->ast which should eql-specialize on CARs
(defmethod sexp->ast ((form cons) &key)
  (cons->ast (car form) form))

(defun add-lambda-list-bindings (lambda-list)
  (multiple-value-bind (req opt rest keywords allow-other aux key)
      (a:parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other key))
    (remove nil
            (append
             (loop for r in req
                   collect (add-lexical-variable
                            r
                            :type 'required-variable-binding))
             (loop for (name init sup) in opt
                   for sb = (when sup
                              (add-lexical-variable
                               sup :type 'suppliedp-variable-binding))
                   collect (add-lexical-variable
                            name :init (sexp->ast init)
                            :type 'optional-variable-binding
                            :pvar sb)
                   when sb collect it)
             (when rest (list (add-lexical-variable
                               rest :type 'rest-variable-binding)))
             (loop for ((kn name) init sup) in keywords
                   for sb = (add-lexical-variable
                             sup :type 'suppliedp-variable-binding)
                   collect (add-lexical-variable
                            name
                            :init (sexp->ast init)
                            :type 'key-variable-binding
                            :pvar sb
                            :kvar kn)
                   when sb collect it)
             (loop for (name init) in aux
                   collect (add-lexical-variable
                            name :init (sexp->ast init)
                            :type 'aux-variable-binding))))))

(defmacro with-lambda-list-env ((bindings lambda-list) &body body)
  `(with-lexical-environment ()
     (let ((,bindings (add-lambda-list-bindings ,lambda-list)))
       ,@body)))

(defun normalize-declarations (decl)
  ;; todo: convert multiple declarations to some consistent format,
  ;; either 1 declaration per DECLARE, or all in 1 DECLARE?
  decl)

;; LAMBDA is only valid non-symbol in car of evaluated cons
(defmethod cons->ast ((car cons) form)
  (assert (eql (car car) 'lambda))
  (multiple-value-bind (body decl docs)
      (a:parse-body (cddr car) :documentation t)
    (let ((fn (with-lambda-list-env (bindings (cadr car))
                (set-context
                 (make-instance 'lambda-function
                                :lambda-list (cadr car)
                                :declarations (normalize-declarations decl)
                                :docstring docs
                                :bindings bindings
                                :body (progn->ast body))))))
      (make-instance 'lambda-application
                     :func fn
                     :args (progn->ast (cdr form))
                     :whole form))))

;; normal application or macro
(defmethod cons->ast ((car symbol) form)
  (let ((b (lookup-function car)))
    (etypecase b
      (host-macro
       (let ((x (macroexpand form)))
         (make-instance 'macro-application
                        :expansion (sexp->ast x)
                        :original-expansion x
                        :binding b
                        :whole form)))
      (macro-binding
       (let ((x (funcall (expander b) form nil)))
         (make-instance 'macro-application
                        :expansion (sexp->ast x)
                        :original-expansion x
                        :binding b
                        :whole form)))
      ((or unknown-function local-function global-function host-function)
       (make-instance 'function-application
                      :binding b
                      :args (progn->ast (cdr form))
                      :whole form)))))

;;; special forms

(defmethod cons->ast ((car (eql 'block)) whole)
  (destructuring-bind (c name &rest body) whole
    (assert (eql c 'block))
    (with-lexical-environment ()
      (make-instance 'special-form-block
                     :binding (add-block-binding name)
                     :body (progn->ast body)
                     :whole whole))))

(defmethod cons->ast ((car (eql 'catch)) whole)
  (destructuring-bind (c tag &rest body) whole
    (assert (eql c 'catch))
    (with-lexical-environment ()
      (make-instance 'special-form-catch
                     :tag-ast (sexp->ast tag)
                     :body (progn->ast body)
                     :whole whole))))

(defmethod cons->ast ((car (eql 'eval-when)) whole)
  (destructuring-bind (c situations &rest body) whole
    (assert (eql c 'eval-when))
    (make-instance 'special-form-eval-when
                   :situations situations
                   :body (progn->ast body)
                   :whole whole)))


(defun make-local-function (fun &key recursive)
  (destructuring-bind (name lambda-list &rest body) fun
    (multiple-value-bind (body decl docs)
        (a:parse-body body :documentation t)
      (let ((binding (make-instance
                      'local-function
                      :name name
                      :lambda-list lambda-list
                      :declarations (normalize-declarations decl)
                      :docstring docs
                      ;; add rest later to get scoping right
                      :bindings nil
                      :body nil)))
        (when recursive
          (add-lexical-function* binding))
        ;; env for function body
        (with-lambda-list-env (local-bindings lambda-list)
          (set-context binding)
          (setf (slot-value binding 'bindings) local-bindings)
          (setf (slot-value binding 'body) (progn->ast body)))
        ;; return function binding if it needs added later (flet)
        binding))))

(defun add-local-functions (functions &key recursive)
  (let ((bindings (loop for f in functions
                        collect (make-local-function f :recursive recursive))))
    (unless recursive
      ;; add flet bindings to flet env after parsing bodies
      (map 'nil #'add-lexical-function* bindings))
    bindings))

(defmethod cons->ast ((car (eql 'flet)) whole)
  (destructuring-bind (c functions &rest body) whole
    (assert (eql c 'flet))
    (multiple-value-bind (body decl)
        (a:parse-body body :documentation nil)
      (with-lexical-environment ()
        (let ((bindings (add-local-functions functions :recursive nil)))
          (set-context
           (make-instance 'special-form-flet
                          :bindings bindings
                          :declarations (normalize-declarations decl)
                          :body (progn->ast body)
                          :whole whole)))))))

(defmethod cons->ast ((car (eql 'function)) whole)
  (destructuring-bind (c name) whole
    (assert (eql c 'function))
    (make-instance 'special-form-function :binding (lookup-function name)
                                          :whole whole)))

(defmethod cons->ast ((car (eql 'go)) whole)
  (destructuring-bind (c tag) whole
    (assert (eql c 'go))
    (make-instance 'special-form-go :tag (lookup-go-tag tag)
                                    :whole whole)))

(defmethod cons->ast ((car (eql 'if)) whole)
  (destructuring-bind (c test then &optional (else nil elsep)) whole
    (assert (eql c 'if))
    (make-instance 'special-form-if
                   :test (sexp->ast test)
                   :then (sexp->ast then)
                   :else (sexp->ast else)
                   :elsep elsep
                   :whole whole)))

(defmethod cons->ast ((car (eql 'labels)) whole)
  (destructuring-bind (c functions &rest body) whole
    (assert (eql c 'labels))
    (multiple-value-bind (body decl)
        (a:parse-body body :documentation nil)
      (with-lexical-environment ()
        (let ((bindings (add-local-functions functions :recursive t)))
          (set-context
           (make-instance 'special-form-labels
                          :bindings bindings
                          :declarations (normalize-declarations decl)
                          :body (progn->ast body)
                          :whole whole)))))))

(defun add-let-bindings (bindings declarations)
  (declare (ignorable declarations))
  (loop
    for r in bindings
    for (n i) = (a:ensure-list r)
    collect (make-lexical-binding n :init (sexp->ast i))))

(defun add-let*-bindings (bindings declarations)
  (declare (ignorable declarations))
  (loop
    for r in bindings
    for (n i) = (a:ensure-list r)
    collect (add-lexical-variable n :init (sexp->ast i))))


(defmethod cons->ast ((car (eql 'let)) whole)
  (destructuring-bind (c bindings &rest body) whole
    (assert (eql c 'let))
    (multiple-value-bind (body decl)
        (a:parse-body body :documentation nil)
      (let* ((decl (normalize-declarations decl))
             (bindings (add-let-bindings bindings decl)))
        (with-lexical-environment ()
          (loop for binding in bindings
                do (add-lexical-variable* binding))
          (set-context
           (make-instance 'special-form-let
                          :bindings bindings
                          :declarations decl
                          :body (progn->ast body)
                          :whole whole)))))))

(defmethod cons->ast ((car (eql 'let*)) whole)
  (destructuring-bind (c bindings &rest body) whole
    (assert (eql c 'let*))
    (with-lexical-environment ()
      (multiple-value-bind (body decl)
          (a:parse-body body :documentation nil)
        (set-context
         (make-instance 'special-form-let*
                        :bindings (add-let*-bindings bindings decl)
                        :declarations (normalize-declarations decl)
                        :body (progn->ast body)
                        :whole whole))))))

(defmethod cons->ast ((car (eql 'load-time-value)) whole)
  (destructuring-bind (c form &optional read-only-p) whole
    (assert (eql c 'load-time-value))
    (assert (typep read-only-p 'boolean))
    (make-instance 'special-form-load-time-value
                   :form (sexp->ast form)
                   :read-only read-only-p
                   :whole whole)))

(defmethod cons->ast ((car (eql 'locally)) whole)
  (destructuring-bind (c &rest body) whole
    (assert (eql c 'locally))
    (multiple-value-bind (body decl)
        (a:parse-body body :documentation nil)
      (make-instance 'special-form-locally
                     :declarations (normalize-declarations decl)
                     :body (progn->ast body)
                     :whole whole))))

(defun add-macro-binding (def)
  (let ((env (car *lexical-environment*))
        (name (car def)))
    (assert env)
    (assert (not (lock env)))
    ;; don't think this should allow redefinition in 1 scope?
    (assert (not (gethash name (functions env))))
    (setf (gethash name (functions env))
          (a:with-gensyms (form menv)
            (make-instance
             'macro-binding
             :name (car def)
             :expander (compile nil
                                (print
                                 `(lambda (,form ,menv)
                                    (declare (ignorable ,menv))
                                    (destructuring-bind ,(second def)
                                        (cdr ,form)
                                      (format t "expand macro ~s: ~s ->~%"
                                              ',(car def)
                                              ',form)
                                      ,@(cddr def)))))
             :context env
             :whole def)))))

(defmethod cons->ast ((car (eql 'macrolet)) whole)
  (destructuring-bind (c macros &rest body) whole
    (assert (eql c 'macrolet))
    (multiple-value-bind (body decl)
        (a:parse-body body :documentation nil)
      (with-lexical-environment ()
        (set-context
         (make-instance 'special-form-macrolet
                        :bindings (loop for m in macros
                                        collect (add-macro-binding m))
                        :whole whole
                        :body (progn->ast body)
                        :declarations decl))))))

(defmethod cons->ast ((car (eql 'multiple-value-call)) whole)
  (destructuring-bind (c function-form &rest forms) whole
    (assert (eql c 'multiple-value-call))
    (make-instance 'special-form-multiple-value-call
                   :function-form (sexp->ast function-form)
                   :body (progn->ast forms)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'multiple-value-prog1)) whole)
  (destructuring-bind (c first-form &body body) whole
    (assert (eql c 'multiple-value-prog1))
    (make-instance 'special-form-multiple-value-prog1
                   :first-form (sexp->ast first-form)
                   :body (progn->ast body)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'progn)) whole)
  (destructuring-bind (c &rest body) whole
    (assert (eql c 'progn))
    (make-instance 'special-form-progn
                   :body (progn->ast body)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'progv)) whole)
  (destructuring-bind (c symbols values &rest body) whole
    (assert (eql c 'progv))
    (make-instance 'special-form-progv
                   :symbols-ast (sexp->ast symbols)
                   :values-ast (sexp->ast values)
                   :body (progn->ast body)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'quote)) whole)
  (destructuring-bind (c object) whole
    (assert (eql c 'quote))
    (make-instance 'special-form-quote
                   :object object)))

(defmethod cons->ast ((car (eql 'return-from)) whole)
  (destructuring-bind (c block-name &optional result) whole
    (assert (eql c 'return-from))
    (assert (symbolp block-name))
    (make-instance 'special-form-return-from
                   :binding (lookup-block block-name)
                   :result (sexp->ast result)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'setq)) whole)
  (destructuring-bind (c &rest pairs) whole
    (assert (eql c 'setq))
    (assert (evenp (length pairs)))
    (multiple-value-bind (variables value-asts)
        (loop for (var val) on pairs by 'cddr
              collect (sexp->ast val) into vals
              collect var into vars
              finally (return (values vars vals)))
      (make-instance 'special-form-setq
                     :value-asts value-asts
                     :variables variables
                     :whole whole))))

(defmethod cons->ast ((car (eql 'symbol-macrolet)) whole)
  (destructuring-bind (c bindings &body body) whole
    (assert (eql c 'symbol-macrolet))
    (with-lexical-environment ()
      (multiple-value-bind (body decl)
          (a:parse-body body :documentation nil)
        (set-context
         (make-instance 'special-form-symbol-macrolet
                        :bindings (loop
                                    for (sym expansion) in bindings
                                    collect
                                    (add-lexical-variable*
                                     (make-instance 'symbol-macro-binding
                                                    :name sym
                                                    :expansion expansion)))
                        :declarations decl
                        :body (progn->ast body)
                        :whole whole))))))

(defmethod cons->ast ((car (eql 'tagbody)) whole)
  (destructuring-bind (c &rest body) whole
    (assert (eql c 'tagbody))
    (with-lexical-environment ()
      (loop for f in body
            when (typep f '(or symbol integer))
              do (add-go-tag f))
      (set-context
       (make-instance 'special-form-tagbody
                      :body (loop for f in body
                                  when (typep f '(or symbol integer))
                                    collect (lookup-go-tag f)
                                  else collect (sexp->ast f))
                      :whole whole)))))

(defmethod cons->ast ((car (eql 'the)) whole)
  (destructuring-bind (c value-type form) whole
    (assert (eql c 'the))
    (make-instance 'special-form-the
                   :typespec value-type
                   :form (sexp->ast form)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'throw)) whole)
  (destructuring-bind (c tag result) whole
    (assert (eql c 'throw))
    (make-instance 'special-form-throw
                   :tag (sexp->ast tag)
                   :result (sexp->ast result)
                   :whole whole)))

(defmethod cons->ast ((car (eql 'unwind-protect)) whole)
  (destructuring-bind (c protected-form &rest cleanup) whole
    (assert (eql c 'unwind-protect))
    (make-instance 'special-form-unwind-protect
                   :protected-form (sexp->ast protected-form)
                   :body (progn->ast cleanup)
                   :whole whole)))



;;; walker API

(defgeneric walk-ast (node walker)
  (:documentation "Walk AST tree NODE.
WALKER should be an instance of a user class to distinguish the particular
code walker (and if needed, store state)"))

(defmethod walk-ast (node walker)
  (unless (or (keywordp node)
              (null node)
              (and (atom node)
                   (not (symbolp node))))
    (error "todo: walk ~s" node)))

(defmethod walk-ast ((node initialized-variable-binding) walker)
  (walk-ast (initform node) walker))

(defun walk-list (list walker)
  (loop for b in list
        do (walk-ast b walker)))

(defun walk-list/c (list walker)
  (loop for b in list
        collect (walk-ast b walker)))

(defun walk-bindings (node walker)
  (walk-list (bindings node) walker))

(defmethod walk-ast ((node implicit-progn) walker)
  (walk-list (body node) walker))

(defmethod walk-ast ((node variable-reference) walker)
  #++
  (walk-ast (binding node) walker))

(defmethod walk-ast ((node constant-reference) walker)
  )

(defmethod walk-ast ((node symbol-macro-reference) walker)
  (walk-ast (expansion node) walker))

(defmethod walk-ast ((node lambda-function) walker)
  (walk-bindings node walker)
  (call-next-method))

(defmethod walk-ast ((node lambda-application) walker)
  (walk-list (args node) walker)
  (walk-ast (func node) walker))

(defmethod walk-ast ((node function-application) walker)
  (walk-list (args node) walker)
  ())

(defmethod walk-ast ((node function-application) walker)
  (walk-list (args node) walker))

(defmethod walk-ast ((node compiler-macro-application) walker)
  (error "todo walk compiler macro"))

(defmethod walk-ast ((node macro-application) walker)
  (walk-ast (expansion node) walker))

(defmethod walk-ast ((node special-form-block) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-catch) walker)
  (walk-ast (tag-ast node) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-eval-when) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-flet) walker)
  (walk-list (bindings node) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-function) walker)
  ;; nothing to do
  )

(defmethod walk-ast ((node special-form-go) walker)
  ;; nothing to do
  )

(defmethod walk-ast ((node special-form-if) walker)
  (walk-ast (test node) walker)
  (walk-ast (then node) walker)
  (walk-ast (else node) walker))

(defmethod walk-ast ((node special-form-labels) walker)
  (walk-list (bindings node) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-let) walker)
  (walk-list (bindings node) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-let*) walker)
  (walk-list (bindings node) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-load-time-value) walker)
  (walk-ast (form node) walker))

(defmethod walk-ast ((node special-form-locally) walker)
  ;; body walked by implicit progn method
  (call-next-method))

(defmethod walk-ast ((node special-form-macrolet) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-multiple-value-call) walker)
  (walk-ast (function-form node) walker)
  ;; walk args as implicit-progn
  (call-next-method))

(defmethod walk-ast ((node special-form-multiple-value-prog1) walker)
  (walk-ast (first-form node) walker)
  ;; walk body as implicit-progn
  (call-next-method))

(defmethod walk-ast ((node special-form-progn) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-progv) walker)
  (walk-ast (symbols-ast node) walker)
  (walk-ast (values-ast node) walker)
  (call-next-method))

(defmethod walk-ast ((node special-form-quote) walker)
  )

(defmethod walk-ast ((node special-form-return-from) walker)
  (walk-ast (result node) walker))

(defmethod walk-ast ((node special-form-setq) walker)
  (walk-list (value-asts node) walker))

(defmethod walk-ast ((node special-form-symbol-macrolet) walker)
  ;; just walk body?
  (call-next-method))

(defmethod walk-ast ((node go-tag-binding) walker)
  )

(defmethod walk-ast ((node unknown-go-tag-binding) walker)
  )

(defmethod walk-ast ((node special-form-tagbody) walker)
  (walk-list (body node) walker))

(defmethod walk-ast ((node special-form-the) walker)
  (walk-ast (form node) walker))

(defmethod walk-ast ((node special-form-throw) walker)
  (walk-ast (tag node) walker)
  (walk-ast (result node) walker))

(defmethod walk-ast ((node special-form-unwind-protect) walker)
  (walk-ast (protected-form node) walker)
  ;; cleanup in implicit progn
  (call-next-method))

;;; visitor class for converting ast back to sexps

(defclass ast->sexp ()
  ;; if set, try to use original form for macros etc if expansion is
  ;; unmodified. Possibly also should add option to remove
  ;; macrolet/symbol-macrolet when all macros are expanded?
  ((collapse-macros :reader collapse-macros
                    :initform nil
                    :initarg :collapse-macros)))

(defmethod walk-ast (node (walker ast->sexp))
  node)

(defmethod walk-ast ((node initialized-variable-binding)
                     (walker ast->sexp))
  (list (name node) (walk-ast (initform node) walker)))

(defmethod walk-ast ((node implicit-progn)
                     (walker ast->sexp))
  (let ((w (alexandria:rcurry 'walk-ast walker)))
    (map 'list w (body node))))

(defmethod walk-ast ((node variable-reference)
                     (walker ast->sexp))
  (name node))

(defmethod walk-ast ((node constant-reference)
                     (walker ast->sexp))
  ;; possibly should add option to expand to value instead of name for
  ;; constants?
  (name node))

(defmethod walk-ast ((node symbol-macro-reference)
                     (walker ast->sexp))
  (if (and (collapse-macros walker)
           (equalp (expansion (binding node))
                   (walk-ast (expansion node) walker)))
      (name (binding node))
      (walk-ast (expansion node) walker)))

(defmethod walk-ast ((node scoped-variable-binding)
                     (walker ast->sexp))
  (list (name node)
        (walk-ast (initform node) walker)))


(defmethod walk-ast ((node rest-variable-binding)
                     (walker ast->sexp))
  (list (name node)))

(defmethod walk-ast ((node optional-variable-binding)
                     (walker ast->sexp))
  (list (name node)
        ;; possibly should skip these if NIL?
        (walk-ast (initform node) walker)
        (when (pvar-binding node)
          (name (pvar-binding node)))))

(defmethod walk-ast ((node key-variable-binding)
                     (walker ast->sexp))
  ;; possibly should skip init and sup if nil?
  (let ((l (list (walk-ast (initform node) walker)
                 (when (pvar-binding node)
                   (name (pvar-binding node))))))
    (if (eql (name node) (keyname node))
        (push (name node) l)
        (push (list (keyname node) (name node)) l))
    l))


(defun lambda-list->sexp (bindings walker)
  (let ((state :req))
    (flet ((state (x)
             (prog1
                 (ecase x
                   (:req
                    (assert (eql state :req))
                    nil)
                   (:opt
                    (assert (member state '(:req :opt)))
                    (unless (eql state :opt)
                      '&optional))
                   (:rest
                    ;; can only have 1 rest variable
                    (assert (member state '(:req :opt)))
                    '&rest)
                   (:key
                    (assert (member state '(:req :opt :rest :key)))
                    (unless (eql state :key)
                      '&key))
                   (:aux
                    (assert (member state '(:req :opt :rest :key :aux)))
                    (unless (eql state :aux)
                      '&aux)))
               (setf state x))))
      (loop
        for binding in bindings
        when (etypecase binding
               (required-variable-binding
                (state :req))
               (optional-variable-binding
                (state :opt))
               (rest-variable-binding
                (state :rest))
               (key-variable-binding
                (state :key))
               (aux-variable-binding
                (state :aux))
               (suppliedp-variable-binding
                nil))
          collect it
        unless (typep binding 'suppliedp-variable-binding)
          collect (walk-ast binding walker)))))

(defun implicit-progn->sexp (body walker)
  (loop for b in body collect (walk-ast b walker)))

(defmethod walk-ast ((node lambda-function)
                     (walker ast->sexp))
  (progn #++if (collapse-macros walker)
         ;; use original lambda-list instead of walking expanded lambda-list
         ;;(error "todo collapse-macro lambda-function")
         (let ((l (implicit-progn->sexp (body node) walker)))
           (when (docstring node)
             (push (docstring node) l))
           (loop for d in (reverse (declarations node)) do (push d l))
           (push (lambda-list->sexp (bindings node) walker) l)
           (push 'lambda l)
           l)))

(defmethod walk-ast ((node local-function)
                     (walker ast->sexp))
  (progn #++if (collapse-macros walker)
         ;; use original lambda-list instead of walking expanded lambda-list
         ;;(error "todo collapse-macro lambda-function")
         (let ((l (implicit-progn->sexp (body node) walker)))
           (when (docstring node)
             (push (docstring node) l))
           (loop for d in (reverse (declarations node)) do (push d l))
           (push (lambda-list->sexp (bindings node) walker) l)
           (push (name node) l)
           l)))

(defun args->sexp (node walker)
  (walk-list/c (args node) walker))

(defmethod walk-ast ((node lambda-application)
                     (walker ast->sexp))
  (list*
   (walk-ast (func node) walker)
   (args->sexp node walker)))

(defmethod walk-ast ((node function-application)
                     (walker ast->sexp))
  (list* (name (binding node))
         (args->sexp node walker)))

(defmethod walk-ast ((node compiler-macro-application)
                     (walker ast->sexp))
  (error "todo walk compiler macro"))

(defmethod walk-ast ((node macro-application)
                     (walker ast->sexp))
  (let ((s (walk-ast (expansion node) walker)))
    (if (and (collapse-macros walker)
             (equalp s (original-expansion node)))
        (whole node)
        s)))

(defmethod walk-ast ((node special-form-block)
                     (walker ast->sexp))
  (list* 'block (name (binding node))
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-catch)
                     (walker ast->sexp))
  (list* 'catch
         (walk-ast (tag-ast node) walker)
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-eval-when)
                     (walker ast->sexp))
  (list* 'eval-when
         (situations node)
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-flet)
                     (walker ast->sexp))
  (let ((l (walk-list/c (body node) walker)))
    (loop for i in (reverse (declarations node))
          do (push i l))
    (push (walk-list/c (bindings node) walker) l)
    (list* 'flet l)))

(defmethod walk-ast ((node special-form-function)
                     (walker ast->sexp))
  (list 'function (name (binding node))))

(defmethod walk-ast ((node special-form-go)
                     (walker ast->sexp))
  (list 'go (name (tag node))))

(defmethod walk-ast ((node special-form-if)
                     (walker ast->sexp))
  (list* 'if
         (walk-ast (test node) walker)
         (walk-ast (then node) walker)
         (when (or (else node)
                   (elsep node))
           (list (walk-ast (else node) walker)))))

(defmethod walk-ast ((node special-form-labels)
                     (walker ast->sexp))
  (let ((l (walk-list/c (body node) walker)))
    (loop for i in (reverse (declarations node))
          do (push i l))
    (push (walk-list/c (bindings node) walker) l)
    (list* 'labels l)))

(defmethod walk-ast ((node special-form-let)
                     (walker ast->sexp))
  (let ((l (walk-list/c (body node) walker)))
    (loop for i in (reverse (declarations node))
          do (push i l))
    (push (walk-list/c (bindings node) walker) l)
    (cons 'let l)))

(defmethod walk-ast ((node special-form-let*)
                     (walker ast->sexp))
  (let ((l (walk-list/c (body node) walker)))
    (loop for i in (reverse (declarations node))
          do (push i l))
    (push (walk-list/c (bindings node) walker) l)
    (cons 'let* l)))

(defmethod walk-ast ((node special-form-load-time-value)
                     (walker ast->sexp))
  (list 'load-time-value
        (walk-ast (form node) walker)
        (read-only node)))

(defmethod walk-ast ((node special-form-locally)
                     (walker ast->sexp))
  (let ((l (implicit-progn->sexp (body node) walker)))
    (loop for d in (reverse (declarations node))
          do (push d l))
    (list* 'locally l)))

(defmethod walk-ast ((node special-form-macrolet)
                     (walker ast->sexp))
  (let ((l (implicit-progn->sexp (body node) walker)))
    (loop for d in (reverse (declarations node))
          do (push d l))
    (loop for binding in (reverse (bindings node))
          do (push (whole binding) l))
    (list* 'macrolet l)))

(defmethod walk-ast ((node special-form-multiple-value-call)
                     (walker ast->sexp))
  (list* 'multiple-value-call
         (walk-ast (function-form node) walker)
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-multiple-value-prog1)
                     (walker ast->sexp))
  (list* 'multiple-value-prog1
         (walk-ast (first-form node) walker)
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-progn)
                     (walker ast->sexp))
  (list* 'progn
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-progv)
                     (walker ast->sexp))
  (list* 'progv
         (walk-ast (symbols-ast node) walker)
         (walk-ast (values-ast node) walker)
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-quote)
                     (walker ast->sexp))
  (list 'quote (object node)))

(defmethod walk-ast ((node special-form-return-from)
                     (walker ast->sexp))
  (list 'return-from
        (name (binding node))
        (walk-ast (result node) walker)))

(defmethod walk-ast ((node special-form-setq) (walker ast->sexp))
  (list* 'setq
         (loop for var in (variables node)
               for val in (value-asts node)
               collect var
               collect (walk-ast val walker))))

(defmethod walk-ast ((node special-form-symbol-macrolet)
                     (walker ast->sexp))
  (let ((l (implicit-progn->sexp (body node) walker)))
    (loop for d in (reverse (declarations node))
          do (push d l))
    (list* 'symbol-macrolet
           (loop for b in (bindings node)
                 collect (list (name b) (expansion b)))
           l)))

(defmethod walk-ast ((node go-tag-binding) (walker ast->sexp))
  (name node))

(defmethod walk-ast ((node special-form-tagbody) (walker ast->sexp))
  (list* 'tagbody
         (implicit-progn->sexp (body node) walker)))

(defmethod walk-ast ((node special-form-the) (walker ast->sexp))
  (list 'the (typespec node) (walk-ast (form node) walker)))

(defmethod walk-ast ((node special-form-throw) (walker ast->sexp))
  (list 'throw
        (walk-ast (tag node) walker)
        (walk-ast (result node) walker)))

(defmethod walk-ast ((node special-form-unwind-protect) (walker ast->sexp))
  (list* 'unwind-protect
         (walk-ast (protected-form node) walker)
         (implicit-progn->sexp (body node) walker)))


;;; example visitor class, tries to validate an AST

(defclass validate-ast ()
  ())

(defun validate-ast (ast)
  (walk-ast ast (make-instance 'validate-ast))
  ast)

(defun assert-ast-node (node)
  (assert (typep node '(or ast-node
                        null
                        keyword
                        (and atom (not symbol))))))

(defun assert-valid-declarations (node)
  (loop for declaration in (declarations node)
        do (assert (typep declaration '(cons (eql declare))))))

(defun assert-valid-docstring (node)
  (assert (typep (docstring node) '(or null string))))

(defun assert-ast-node-list (list)
  (map nil 'assert-ast-node list))

(defun assert-binding (b &optional (type 'binding))
  (assert (typep b type)))

(defun assert-binding-list (b)
  (map nil 'assert-binding b))

(defmethod walk-ast :before ((node initialized-variable-binding)
                             (walker validate-ast))
  (assert-ast-node (initform node)))


(defmethod walk-ast :before ((node variable-reference)
                             (walker validate-ast))
  (assert-binding (binding node) 'variable-binding)
  (assert (eql (name node) (name (binding node)))))

(defmethod walk-ast :before ((node constant-reference)
                             (walker validate-ast))
  (assert-binding (binding node) 'constant-binding)
  (assert (eql (name node) (name (binding node)))))

(defmethod walk-ast :before ((node symbol-macro-reference)
                             (walker validate-ast))
  (assert-ast-node (expansion node)))


(defmethod walk-ast :before ((node lambda-function)
                             (walker validate-ast))
  (assert-binding-list (bindings node))
  (assert-valid-declarations node))

(defmethod walk-ast :before ((node local-function)
                             (walker validate-ast))
  (assert-binding-list (bindings node))
  (assert (typep (name node) '(or symbol (cons (eql setf) (cons symbol)))))
  (assert-valid-declarations node))

(defmethod walk-ast :before ((node lambda-application)
                             (walker validate-ast))
  (assert-ast-node-list (args node))
  (assert (typep (func node) 'lambda-function)))

(defmethod walk-ast :before ((node function-application)
                             (walker validate-ast))
  (assert-ast-node-list (args node)))

(defmethod walk-ast :before ((node compiler-macro-application)
                             (walker validate-ast))
  (error "todo walk compiler macro"))

(defmethod walk-ast :before ((node macro-application)
                             (walker validate-ast))
  (assert-ast-node (expansion node)))

(defmethod walk-ast :before ((node special-form-block)
                             (walker validate-ast))
  (assert (symbolp (name (binding node)))))

(defmethod walk-ast :before ((node special-form-catch)
                             (walker validate-ast))
  (assert-ast-node (tag-ast node)))

(defmethod walk-ast :before ((node special-form-eval-when)
                             (walker validate-ast))
  (assert (listp (situations node)))
  (loop for situation in (situations node)
        do (assert (member situation '(:compile-toplevel :load-toplevel
                                       :execute)))))

(defmethod walk-ast :before ((node special-form-flet)
                             (walker validate-ast))
  (assert-valid-declarations node)
  (loop for b in (bindings node)
        do (assert-valid-declarations b)
           (assert-valid-docstring b))
  (assert-ast-node-list (bindings node)))

(defmethod walk-ast :before ((node special-form-function)
                             (walker validate-ast))
  (assert (typep (binding node) 'function-binding))
  (assert (typep (name (binding node))
                 '(or symbol (cons (eql setf) (cons symbol nil))))))

(defmethod walk-ast :before ((node special-form-go)
                             (walker validate-ast))
  (assert (typep (tag node) 'go-tag-binding))
  (assert (typep (name (tag node)) '(or symbol integer))))

(defmethod walk-ast :before ((node special-form-if)
                             (walker validate-ast))
  (assert-ast-node (test node))
  (assert-ast-node (then node))
  (assert-ast-node (else node)))

(defmethod walk-ast :before ((node special-form-labels)
                             (walker validate-ast))
  (assert-valid-declarations node)
  (loop for b in (bindings node)
        do (assert-valid-declarations b)
           (assert-valid-docstring b))
  (assert-ast-node-list (bindings node)))

(defmethod walk-ast :before ((node implicit-progn)
                             (walker validate-ast))
  (assert-ast-node-list (body node)))

(defmethod walk-ast :before ((node special-form-let)
                             (walker validate-ast))
  (assert-valid-declarations node)
  (assert-ast-node-list (bindings node)))

(defmethod walk-ast :before ((node special-form-let*)
                             (walker validate-ast))
  (assert-valid-declarations node)
  (assert-ast-node-list (bindings node)))

(defmethod walk-ast :before ((node special-form-load-time-value)
                             (walker validate-ast))
  (assert-ast-node (form node))
  (assert (typep (read-only node) 'boolean)))

(defmethod walk-ast :before ((node special-form-locally)
                             (walker validate-ast))
  (assert-valid-declarations node))

(defmethod walk-ast :before ((node special-form-macrolet)
                             (walker validate-ast))
  (assert-valid-declarations node)
  (loop for b in (bindings node)
        do (assert (typep b 'macro-binding))))

(defmethod walk-ast :before ((node special-form-multiple-value-call)
                             (walker validate-ast))
  (assert-ast-node (function-form node)))

(defmethod walk-ast :before ((node special-form-multiple-value-prog1)
                             (walker validate-ast))
  (assert-ast-node (first-form node)))

(defmethod walk-ast :before ((node special-form-progn)
                             (walker validate-ast))
  ;; body handled by implicit-progn method
  )

(defmethod walk-ast :before ((node special-form-progv)
                             (walker validate-ast))
  (assert-ast-node (symbols-ast node))
  (assert-ast-node (values-ast node)))

(defmethod walk-ast :before ((node special-form-quote)
                             (walker validate-ast))
  ;; nothing to do?
  )

(defmethod walk-ast :before ((node special-form-return-from)
                             (walker validate-ast))
  (assert (symbolp (name (binding node))))
  (assert-ast-node (result node)))

(defmethod walk-ast :before ((node special-form-setq)
                             (walker validate-ast))
  (assert (eql (length (variables node))
               (length (value-asts node))))
  (assert-ast-node-list (value-asts node)))

(defmethod walk-ast :before ((node special-form-symbol-macrolet)
                             (walker validate-ast))
  ;; nothing to do?
  )

(defmethod walk-ast :before ((node special-form-tagbody)
                             (walker validate-ast))
  (loop for n in (body node)
        do (assert (typep n (or '(or
                                  ast-node
                                  go-tag-binding
                                  (and atom (not symbol))))))))

(defmethod walk-ast :before ((node special-form-the)
                             (walker validate-ast))
  (assert-ast-node (form node)))

(defmethod walk-ast :before ((node special-form-throw)
                             (walker validate-ast))
  (assert-ast-node (tag node))
  (assert-ast-node (result node)))

(defmethod walk-ast :before ((node special-form-unwind-protect)
                             (walker validate-ast))
  (assert-ast-node (protected-form node)))



;;; another example, collect stats of # of bindings, # of times used,
;;; # of free bindings etc



(defclass ast-stats ()
  ((variables :reader variables :initform (make-hash-table))
   (functions :reader functions :initform (make-hash-table))))

(defun ast-stats (ast)
  (labels ((format-context (a)
             (when a
               (if (stringp a)
                   a
                   (typecase (context a)
                     (special-form-let "let")
                     (special-form-let* "let*")
                     (special-form-flet "flet")
                     (special-form-labels "labels")
                     (special-form-symbol-macrolet "symbol-macrolet")
                     (special-form-macrolet "macrolet")
                     (local-function
                      (format nil "~a: ~s"
                              (format-context (context (context a)))
                              (name (context a))))
                     (lambda-function "lambda")
                     (t (format nil "~a" (type-of (context a))))))))
           (print-counts (hash type &optional (extra (constantly nil)))
             (let* ((l (loop for k being the hash-keys of hash
                             when (if (functionp type)
                                      (funcall type k)
                                      (typep k type))
                               collect k)))
               (when l
                 (loop
                   for i in (sort l 'string<
                                  :key (lambda (a)
                                         (symbol-name (name a))))
                   do (format t "  ~s: ~s~@[ (~a)~]~@[ @ ~s~]~%"
                              (name i) (gethash i hash)
                              (funcall extra i)
                              (format-context (context i))))))))
    (let ((stats (make-instance 'ast-stats)))
      (walk-ast ast stats)
      (format t "free variables:~%")
      (print-counts (variables stats) 'free-variable)
      (format t "host variables:~%")
      (print-counts (variables stats) 'host-variable)
      (format t "local variables:~%")
      (print-counts
       (variables stats)
       'scoped-variable-binding
       (lambda (a)
         (let ((b (format nil "~@{~@[~a~]~}"
                          (when (and
                                 (typep a 'scoped-variable-binding)
                                 (specialp a))
                            "*special* ")
                          (typecase a
                            (required-variable-binding "required")
                            (optional-variable-binding "&optional")
                            (rest-variable-binding "&rest")
                            (key-variable-binding "&key")
                            (aux-variable-binding "&aux")
                            (free-variable "free")
                            (suppliedp-variable-binding "-p")
                            (scoped-variable-binding nil)
                            (t (type-of a)))
                          (when (typep a 'scoped-variable-binding-with-p)
                            (format nil "~@[ @ ~a~]"
                                    (name (pvar-binding a)))))))
           (unless (string= b "") b))))
      (format t "constants:~%")
      (print-counts (variables stats) 'constant-binding)

      (format t "symbol-macros:~%")
      (print-counts (variables stats) 'symbol-macro-binding)

      (format t "global function calls:~%")
      (print-counts (functions stats) '(or global-function host-function))

      (format t "local functions calls:~%")
      (print-counts (functions stats)  'local-function)

      (format t "unknown function calls:~%")
      (print-counts (functions stats) 'unknown-function)

      (format t "non-call function refs:~%")
      (print-counts (variables stats)  'function-binding)

      (format t "macros:~%")
      (print-counts (functions stats) '(or macro-binding host-macro))))
  ast)

(defmethod walk-ast :before ((node variable-reference) (walker ast-stats))
  #++
  (format t "~&inc v ~s ~s | ~s~%" (name (binding node)) (type-of node)
          (type-of (binding node)))
  (incf (gethash (binding node) (variables walker) 0)))

(defmethod walk-ast :before ((node normal-application) (walker ast-stats))
  #++
  (format t "~&inc f ~s ~s | ~s ~%" (name (binding node)) (type-of node)
          (type-of (binding node)))
  #++
  (format t "  ~s~%" (whole node))
  (incf (gethash (binding node) (functions walker) 0)))

(defmethod walk-ast :before ((node special-form-function) (walker ast-stats))
  #++
  (format t "~&inc sff ~s ~s | ~s ~%" (name (binding node)) (type-of node)
          (type-of (binding node)))
  (incf (gethash (binding node) (variables walker) 0)))

;;; collect all nodes with multiple possible return values, and the
;;; nodes that might be returned from them, along with context
;;; indicating how return value is (or isn't) used. context is either
;;; :unused or parent ast node


(defclass ast-returns ()
  ;; for now just a hash table of node -> (context . returned-nodes)
  ((rets :reader rets :initform (make-hash-table))))

(defvar *ast-ret*)
(defvar *ast-ret-callers* '(:toplevel))

(defun ast-returns (ast)
  (let ((r (make-instance 'ast-returns)))
    (let ((*ast-ret*))
      (walk-ast ast r))
    (rets r)))


(defclass phi ()
  ((node :reader node :initarg :node)
   (rets :reader rets :initarg :rets)
   (@ :reader @ :initarg :@)))

(defun add-phi (node walker)
  (assert (not (gethash node (rets walker))))
  (when (boundp '*ast-ret*)
   (let ((ret *ast-ret*))
     (when (and (consp *ast-ret*))
       (setf ret (make-instance 'phi
                                :node node
                                :rets *ast-ret*
                                :@ *ast-ret-callers*)))
     (when t ;(typep ret 'phi)
       (setf (gethash node (rets walker)) ret)
       (setf *ast-ret* ret)))))

;; keep track of callers so we can add entries from nodes with variant
;; returns rather than callers
(defmethod walk-ast :around (node
                             (walker ast-returns))
  (let ((*ast-ret-callers* (list* node *ast-ret-callers*)))
    (prog1
        (call-next-method)
      (when (boundp '*ast-ret*)
        (format t "~&@@@~s: ret ~s~% at ~s~%"
                node *ast-ret* *ast-ret-callers*)
        (setf *ast-ret* (or *ast-ret* node)))
      (when (typep node '(or application))
       (add-phi node walker))
)))

;; default to just returning the node itself
(defmethod walk-ast ((node ast-node) (walker ast-returns))
  (unless *ast-ret*
    (format t "~&:::: def set ~s~%" node)
    (setf *ast-ret* node))
  (call-next-method))
;; implicit progNs add any nodes with multiple returns, with context
;; :unused or NODE depending on whether it is last form or not

(defun implicit-progn-rets (node walker)
  (let ((ret (lookup-variable nil)))
    (loop for (b . more) on (body node)
          do (let ((*ast-ret* nil)
                   ;; modify caller list depending on whether we use the
                   ;; return value
                   (*ast-ret-callers*
                     (if more
                         (list* `(:unused@ ,node) (cdr *ast-ret-callers*))
                         (list* node (cdr *ast-ret-callers*)))))
               (walk-ast b walker)
               (unless more
                 (format t "::::: last form of progn = ~s~%"
                         (ast->sexp b))
                 (format t "      returns ~s~%"
                         (ast->sexp *ast-ret*))
                 ;; save the returns of last form so we can return it
                 (setf ret *ast-ret*))))
    (format t "~&::::: ipr set rets = ~s~%" ret)
    (setf *ast-ret* ret)))

(defmethod walk-ast ((node implicit-progn)
                     (walker ast-returns))
  (implicit-progn-rets node walker))

(defmethod walk-ast ((node special-form-tagbody)
                     (walker ast-returns))
  (implicit-progn-rets node walker))

;; nodes that might have multiple returns

(defmethod walk-ast ((node special-form-if)
                     (walker ast-returns))
  (let ((ret nil))
    (flet ((a ()
             (if (consp *ast-ret*)
                 (setf ret (append *ast-ret* ret))
                 (push *ast-ret* ret))))
      (let ((*ast-ret* nil))
        ;; value of TEST isn't returned
        (walk-ast (test node) walker)
        ;; THEN and ELSE can be returned
        (walk-ast (then node) walker)
        (a)
        (walk-ast (else node) walker)
        (a)))
    #++(assert (not (gethash node (rets walker))))
    #++(setf (gethash node (rets walker))
          (list* (list :@ (cadr *ast-ret-callers*))
                 ret))
    (format t "~&:::: if ret ~s ~%" ret)
    (setf *ast-ret* ret)))

(defvar *block-rets* (make-hash-table))
(defmethod walk-ast ((node special-form-block)
                     (walker ast-returns))
  (unwind-protect
       (let ((ret (lookup-variable NIL)))
         (let ((*ast-ret*))
           (format t "==== add block ~s (~s)~%"
                   (name (binding node))
                   (binding node))
           (setf (gethash (binding node) *block-rets*) nil)
           (let ((*ast-ret*))
             (implicit-progn-rets node walker)
             (setf ret *ast-ret*)
             (format t " === progn ret = ~s~%" ret))
           (when (gethash (binding node) *block-rets*)
             (format t "block rets = ~s~%" (gethash (binding node) *block-rets*))
             (setf ret (list* *ast-ret*
                              (gethash (binding node) *block-rets*))))
           #++(assert (not (gethash node (rets walker))))
           #++(setf (gethash node (rets walker))
                    (list* (list :@ (cadr *ast-ret-callers*))
                           ret)))
         (setf *ast-ret* ret)
         (format t "==== remove block ~s (~s)~% ret=~s~%"
                 (name (binding node))
                 (binding node)
                 *ast-ret*))
    (remhash (binding node) *block-rets*)
    ))

;; return-from adds to corresponding block
(defmethod walk-ast ((node special-form-return-from)
                    (walker ast-returns))
  (format t "==== return-from ~s = ~s = ~s @ ~s~%"
          (name (binding node)) (ast->sexp (result node))
          (gethash (binding node) *block-rets* :???)
          node)
  (let ((*ast-ret*))
    (walk-ast (result node) walker)
    (push *ast-ret* (gethash (binding node) *block-rets*)))
  ;; fixme: add a class or something to indicate forms that don't
  ;; return normally
  (format t "~&:::: return-from :nlx~%")
  (setf *ast-ret* (vector :nlx node)))


;; fixme: explicitly catch the case where throw isn't inside a catch?
(defvar *catch-rets*)
(defmethod walk-ast ((node special-form-catch)
                     (walker ast-returns))
  (let ((throws nil)
        (ret nil))
    ;; can't catch any THROWs inside TAG form
    (let ((*ast-ret*))
      (walk-ast (tag-ast node) walker))
    ;; collect return value and throw values from body
    (let ((*ast-ret*)
          (*catch-rets* nil))
      (implicit-progn-rets node walker)
      (setf ret *ast-ret*)
      (setf throws *catch-rets*))
    ;; add to any parent catches too
    (when (boundp '*catch-rets*)
      (setf *catch-rets* (append throws *catch-rets*)))
    (setf *ast-ret* (remove-duplicates
                     (append (a:ensure-list ret)
                             (a:ensure-list throws)))
          #++(if (consp throws)
                 (list* ret throws)
                 ret))
    (format t "~&:::: catch ~s" *ast-ret*)
    #++(when throws
         (setf (gethash node (rets walker))
               (list* (list :@ (cadr *ast-ret-callers*))
                      *ast-ret*)))))


(defmethod walk-ast ((node special-form-throw)
                     (walker ast-returns))
  (let ((*ast-ret*))
    (walk-ast (tag node) walker)
    (walk-ast (result node) walker)
    (when (boundp '*catch-rets*)
      (push *ast-ret* *catch-rets*)))
  ;; fixme: add a class or something to indicate forms that don't
  ;; return normally
  (format t "~&:::: throw :nlx~%")
  (setf *ast-ret* (vector :nlx node)))

;; notice some functions that don't return
(defmethod walk-ast ((node function-application)
                     (walker ast-returns))
  (call-next-method)
  (setf *ast-ret* node)
  (when (member (name (binding node)) '(error
                                        #+sbcl SB-KERNEL:ECASE-FAILURE))
    (format t "&&& got nlx @ ~s~%" (name (binding node)))
    (setf *ast-ret* (vector :nlx node))))

#++
(ast-returns
 (sexp->ast
  '(block nil
    (catch a
      (ecase foo
        (1 (throw a1 :a1)
         11)
        (2 (catch b
             (throw b1 :b1)
             (throw a2 :a2))
         22)
        (3 (catch c
           (throw c1 :c1)
           (throw a3 :a3))
         33)
        (4 123))
      )
    (if a
        (return 'foo)
        (progn
          (return bar)
          2)))))
#++
(trace :methods t walk-ast)
#++
(untrace)

