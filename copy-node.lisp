(in-package #:3b-walker)

(defgeneric instance-initargs (object)
  (:documentation "return a plist of :INITARG VALUE that can be used to initialize a copy of OBJECT.")
  (:method-combination nconc))


(defgeneric copy-node (node)
  (:documentation "make a copy of NODE."))

(defmethod copy-node (node)
  ;; default to copying any slots with :initarg and a reader
  ;; method. More specialized methods can handle other slots
  (apply 'make-instance (class-of node) (instance-initargs node)))



(defgeneric instance-initargs (object)
  (:documentation "return a plist of :INITARG VALUE that can be used to initialize a copy of OBJECT.")
  (:method-combination nconc))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun make-copier (class-name)
    (let* ((class (find-class class-name))
           (slots (c2mop:class-direct-slots class))
           (copy nil))
      `(progn
         (defmethod instance-initargs nconc ((object ,class-name))
           (list
            ,@(loop for s in slots
                    for init = (c2mop:slot-definition-initargs s)
                    for reader = (c2mop:slot-definition-readers s)
                    when (and init reader)
                      collect (car init)
                      and collect `(,(car reader) object)
                    else do (pushnew (c2mop:slot-definition-name s) copy))))
         ,@(when copy
             `((defmethod copy-node ((object ,class-name))
                 (let ((new (call-next-method)))
                   ,@(loop for c in copy
                           collect `(setf (slot-value new ',c)
                                          (slot-value object ',c)))
                   new))))))))

(defmacro make-copiers (() class-name &body more-names)
  (let ((names (list* class-name more-names)))
    `(progn
       ,@ (loop for n in names
                collect (make-copier n)))))
(make-copiers ()
;; no default copier: AST->SEXP AST-STATS LEXICAL-ENVIRONMENT
  APPLICATION
  AST-NODE
  AUX-VARIABLE-BINDING
  BINDING
  BLOCK-BINDING
  COMPILER-MACRO-APPLICATION
  CONSTANT-BINDING
  CONSTANT-REFERENCE
  FORM-WITH-DECLARATIONS
  FORM-WITH-SCOPE
  FORM-WITH-DOCSTRING
  FREE-VARIABLE
  FUNCTION-APPLICATION
  FUNCTION-BINDING
  GLOBAL-FUNCTION
  GO-TAG-BINDING
  HOST-FUNCTION
  HOST-MACRO
  HOST-VARIABLE
  IMPLICIT-PROGN
  KEY-VARIABLE-BINDING
  LAMBDA-APPLICATION
  LAMBDA-FUNCTION
  LEXICAL-ENVIRONMENT
  LOCAL-FUNCTION
  MACRO-APPLICATION
  MACRO-BINDING
  NORMAL-APPLICATION
  OPTIONAL-VARIABLE-BINDING
  REQUIRED-VARIABLE-BINDING
  REST-VARIABLE-BINDING
  SCOPED-VARIABLE-BINDING
  SCOPED-VARIABLE-BINDING-WITH-P
  SPECIAL-FORM-APPLICATION
  SPECIAL-FORM-BLOCK
  SPECIAL-FORM-CATCH
  SPECIAL-FORM-EVAL-WHEN
  SPECIAL-FORM-FLET
  SPECIAL-FORM-FUNCTION
  SPECIAL-FORM-GO
  SPECIAL-FORM-IF
  SPECIAL-FORM-LABELS
  SPECIAL-FORM-LET
  SPECIAL-FORM-LET*
  SPECIAL-FORM-LOAD-TIME-VALUE
  SPECIAL-FORM-LOCALLY
  SPECIAL-FORM-MACROLET
  SPECIAL-FORM-MULTIPLE-VALUE-CALL
  SPECIAL-FORM-MULTIPLE-VALUE-PROG1
  SPECIAL-FORM-PROGN
  SPECIAL-FORM-PROGV
  SPECIAL-FORM-QUOTE
  SPECIAL-FORM-RETURN-FROM
  SPECIAL-FORM-SETQ
  SPECIAL-FORM-SYMBOL-MACROLET
  SPECIAL-FORM-TAGBODY
  SPECIAL-FORM-THE
  SPECIAL-FORM-THROW
  SPECIAL-FORM-UNWIND-PROTECT
  SUPPLIEDP-VARIABLE-BINDING
  SYMBOL-MACRO-BINDING
  SYMBOL-MACRO-REFERENCE
  UNKNOWN-GO-TAG-BINDING
  VALIDATE-AST
  VARIABLE-BINDING
  VARIABLE-REFERENCE)
