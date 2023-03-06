(in-package :cllox.interpreter)

(defparameter *reg* 1)

;; interpret/compile

(defun interpret (expr)
  (let ((body (evaluate expr)))
    (concatenate 'string
                 (format nil "define i32 @main() {~%")
                 body
                 (format nil "ret i32 %~a~%}~%" (1- *reg*)))))



;; eval

(defgeneric evaluate (object)
  (:documentation "return a string of llvm ir."))

(defmethod evaluate ((self cllox.expr:literal))
  (with-slots (cllox.expr::value)
      self
    (let* ((reg *reg*)
           (res
             (when (integerp cllox.expr::value)
               (format nil
                       "%~a = alloca i32~%store i32 ~a, ptr %~a~%"
                       reg cllox.expr::value reg))))
      (incf *reg*)
      res)))

(defmethod evaluate ((self cllox.expr:grouping))
  (with-slots (cllox.expr::expression)
      self
    (evaluate cllox.expr::expression)))

(defmethod evaluate ((self cllox.expr:unary))
  (with-slots (cllox.expr::operator
               cllox.expr::right)
      self
    (let ((right (evaluate cllox.expr::right)))
      (let* ((reg *reg*)
             (res (case (cllox.token:token-type
                         cllox.expr::operator)
                    (:minus
                     (format nil
                             "~a%~a = load i32, ptr %~a~%%~a = sub nsw i32 0, %~a~%" right reg (1- reg) (1+ reg) reg)))))
        (incf *reg* 2)
        res))))

(defmethod evaluate ((self cllox.expr:binary))
  (with-slots (cllox.expr::left
               cllox.expr::operator
               cllox.expr::right)
      self
    (let ((left (evaluate cllox.expr::left))
          (res1 (1- *reg*)))
      (let* ((right (evaluate cllox.expr::right))
             (res2 (1- *reg*)))
        (let* ((reg *reg*)
               (res (case (cllox.token:token-type
                           cllox.expr::operator)
                      (:minus
                       (format nil
                               "%~a = load i32, ptr %~a~%%~a = load i32, ptr %~a~%%~a = sub nsw i32 %~a, %~a~%"
                               reg res1 (1+ reg) res2 (+ 2 reg) reg (1+ reg)))
                      (:plus
                       (format nil
                               "%~a = add nsw i32 %~a, %~a~%"
                               reg res1 res2))
                      
                      (:star
                       (format nil
                               "%~a = mul nsw i32 %~a, %~a~%"
                               reg res1 res2))
                      
                      (:slash
                       (format nil
                               "%~a = sdiv i32 %~a, %~a~%"
                               reg res1 res2)))))
          (incf *reg* 3)
          (concatenate 'string left right res))))))

                      
