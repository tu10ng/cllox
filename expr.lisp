(in-package :cllox.expr)


;; macros

(defmacro defclass-slots (name (superclass) slots)
  "(defclass-only-slots binary (left operator right))"
  `(defclass ,name (,superclass)
     ,(loop for slot in slots
            collect `(,slot :initarg
                            ,(intern (symbol-name slot) "KEYWORD")))))

(defmacro expand-ast (name slots body)
  `(defmethod ast ((self ,name))
     (with-slots ,slots
         self
       ,@body)))

(defmacro expand-expr (lst)
  "(expand-expr ((binary left operator right)
              (a b c)))"
  `(progn
     ,@(loop for (name slots . body) in lst
             collect `(progn
                        (defclass-slots ,name
                            (expr)
                            ,slots)
                        (expand-ast ,name ,slots ,body)))))


;; class and some methods
(defclass expr ()
  ())

(expand-expr
 ((binary (left operator right)
          (format nil "(~a ~a ~a)"
                  (ast operator)
                  (ast left)
                  (ast right)))
  (grouping (expression)
            (format nil "(group ~a)"
                    (ast expression)))
  (literal (value)
           (write-to-string value))
  (unary (operator right)
         (format nil "(~a ~a)"
                 (ast operator)
                 (ast right)))))


;; ast

(defgeneric ast (object)
  (:documentation "return prefix string"))

;; can't use `with-slots' ? error: SB-PCL::MISSING-SLOT for `lexeme'
(defmethod ast ((token cllox.token:token))
  (format nil "~a" (cllox.token:token-lexeme token)))

;; other ast method need use slots, so i put them together with class define.
