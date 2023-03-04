(in-package :cllox.token)


;;; token

(defclass token ()
  ((type :initarg :type :reader token-type)
   (lexeme :initarg :lexeme :reader token-lexeme)
   (literal :initarg :literal :reader token-literal)
   (line :initarg :line :reader token-line))
  (:documentation "a parsed token"))

(defmethod print-object ((token token) s)
  "Output a token to a stream. from lexer.lisp"
  (print-unreadable-object (token s)
    (with-slots (type lexeme literal)
        token
      (format s "~a ~a ~a" type lexeme literal))))


;;; token.lisp ends here
