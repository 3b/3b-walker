#++ (ql:quickload '(parachute 3b-walker))
(defpackage #:3b-walker-test
  (:use #:cl #:parachute)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:w #:3b-walker)))

(in-package 3b-walker-test)


(define-test lexenv
  (let ((e w::*lexical-environment*))
    (w::with-lexical-environment ()
      (of-type w::lexical-environment (car w::*lexical-environment*))
      (is eql e (cdr w::*lexical-environment*))
      (of-type w::scoped-variable-binding (w::add-lexical-variable 'foo))
      (of-type w::scoped-variable-binding (w::lookup-variable 'foo))
      (is eql 'foo (w::name (w::lookup-variable 'foo)))
      (of-type w::free-variable (w::lookup-variable 'bar)))
    (is eql e w::*lexical-environment*)))

#++(test 'lexenv)
(define-test sexp->ast
  (of-type w::constant-binding (w::sexp->ast nil))
  (is eql "foo" (w::sexp->ast "foo"))
  (is eql :foo (w::sexp->ast :foo))
  (is eql 123 (w::sexp->ast 123))
  (is eql 4.5 (w::sexp->ast 4.5))
  (is eql #1=#*10101 (w::sexp->ast #1#))
  (of-type w::variable-reference (w::sexp->ast 'foo))
  (of-type w::variable-reference (w::sexp->ast '+++))
  (of-type w::constant-reference (w::sexp->ast 'most-positive-single-float))
  (w::with-lexical-environment ()
    (finish (w::add-lexical-variable 'bar))
    (of-type w::variable-reference (w::sexp->ast 'bar))
    (let ((f (finish
              (w::sexp->ast '((lambda (a &optional (b (+ a 1) bp)
                                       &rest r
                                       &key k1 (k2 (or (car r) 2) k2p)
                                       &aux (a1 (+ b k2)))
                                (let ((x (list a1 k1 bp k1 k2p)))
                                  (print x)
                                  x))
                              a 1))))
          (w (finish (make-instance 'w::validate-ast))))
      (finish (w::walk-ast f w))
      (of-type w::variable-reference (car (w::args f)))
      (of-type w::free-variable (w::binding (car (w::args f))))
      (is eql 1 (cadr (w::args f))))))

#++(test 'sexp->ast :report 'interactive)
#++
(trace :methods t w::walk-ast)
#++
(untrace)

#++

(w::sexp->ast '(let ((a 1)
                     (b a))
                (if a b nil)
                (setf a (the fixnum (1+ a)))
                b))

#++
(w::ast->sexp
 (w::ast-stats
  (w::validate-ast
   (w::sexp->ast '(block nil
                   (let* ((a 1)
                          (b (progn (print a) a))
                          (c (load-time-value (+ 1 2) t)))
                     (multiple-value-call #'/ 1 (floor 10 3))
                     (progv (list foo bar)
                         '(1 2)
                       (+ foo bar))
                     (if a b nil)
                     (if a b)
                     (setf a (+ 1 1)
                           b 2)
                     (symbol-macrolet ((a (* 1 2))
                                       (x :foo))
                       (list a x))
                     (macrolet ((mx (a)
                                  `(list ,a)))
                       (mx 123))
                     (flet ((x (a b &key ((:f cc) 1 p))
                              (declare foo)
                              "x docs"
                              (declare bar)
                              (list cc a b p)))
                       (labels ((y (x)
                                  (declare foo)
                                  "y docs"
                                  (declare bar)
                                  (x 1 x 2 :f 3))
                                (z ()
                                  (or (y 2) (z))))
                         (list (x) (y 1) (z)))
                       x)
                     (foo free-variable)
                     (undefined 2 3 *package*)
                     (when t
                       (return-from nil (- 123 4)))
                     (locally (declare foo)
                       (declare bar baz)
                       (eval-when (:execute)
                         (multiple-value-prog1
                             (floor 11 3)
                           (catch (+ 1 3)
                             (function eql)
                             (unwind-protect
                                  (throw (- 4 1) (list a b))
                               (print "a")
                               (print "b"))
                             a))))
                     ((lambda (a &optional (b (+ a 1) bp)
                               &rest r
                               &key ((:k? k1)) (k2 (or (car r) 2) k2p)
                               &aux (a1 (+ b k2)))
                        (declare (optimize safety)
                                 (type list r))
                        "does the thing"
                        (declare (fixnum a))
                        (declare)
                        (let ((x (list a1 k1 bp k1 k2p)))
                          (print x)))
                      a 1)
                     (tagbody
                        (go 1)
                      1
                        (go foo)
                      foo
                      :bar
                        (the fixnum (1+ a)))
                     b)))))
 :restore-macros nil)


#++
(w::ast->sexp
 (w::sexp->ast '((lambda (a &optional (b (+ a 1) bp)
                          &rest r
                          &key ((:k? k1)) (k2 (or (car r) 2) k2p)
                          &aux (a1 (+ b k2)))
                   (declare (optimize safety)
                            (type list r))
                   "does the thing"
                   (declare (fixnum a))
                   (declare)
                   (let ((x (list a1 k1 bp k1 k2p)))
                     (print x)))
                 a 1))
 :restore-macros t)



#++
(defclass free-wat ()
  ())

#++
(defmethod w:filter-ast ((node w:variable-reference)
                         (walker free-wat))
  (format t "ref to ~s~%" (w:name (w:binding node)))
  (if (typep (w:binding node) 'w:free-variable)
      :wat
      node))


#++
(defclass remove-free ()
  ())

#++
(defmethod w:filter-ast ((node w:variable-reference)
                         (walker remove-free))
  (if (typep (w:binding node) 'w:free-variable)
      nil
      node))

#++
(defclass free->a ()
  ())

#++
(defmethod w:filter-ast ((node w:variable-reference)
                         (walker free->a))
  (if (typep (w:binding node) 'w:free-variable)
      (w::make-variable-reference 'a)
      node))

#++
(let ((a (w:sexp->ast `(let ((a 1))
                         (list a b)))))
  (list (w:ast->sexp
         (w:filter-ast a 'free-wat))
        (w:ast->sexp
         (w:filter-ast a 'remove-free))
        (w:ast->sexp
         (w:filter-ast a 'free->a))))


#++
(defclass wrap-free ()
  ())

#++
(defmethod w:filter-ast ((node w:variable-reference)
                         (walker wrap-free))
  (if (typep (w:binding node) 'w:free-variable)
      (w::make-function-call 'scalar node)
      node))
#++
(w:ast->sexp (w:filter-ast (w:sexp->ast '(+ a 1 )) 'wrap-free))
