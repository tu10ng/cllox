(in-package :cllox.parser)


;;; error

(defmacro handling-errors (&body body)
  `(handler-bind
       ((parser-error
          #'(lambda (e)
              (format *error-output*
                      "parser-error: ~a~%" e)
              ;; currently we go into panic, later when we have statements, we will use `synchronize'
              (sb-ext:exit))))
     (progn ,@body)))
  
(define-condition parser-error (error)
  ((token :initarg :token)
   (reason :initarg :reason))
  (:documentation "signaled during parsing")
  (:report (lambda (c s)
             (with-slots (token reason)
                 c
               (format s "~a at '~a': ~a"
                       (cllox.token:token-line token)
                       (cllox.token:token-lexeme token)
                       reason)))))
  
(defun synchronize (parser)
  "continue to parse when trivial error occur, and jump to next statement.
TODO try use cl restart"
  (advance parser)
  (loop while (null (at-endp parser))
        do (when (eql (cllox.token:token-type (previous parser))
                      :semicolon)
             (return-from synchronize))
           (case (cllox.token:token-type (peek parser))
             (:class (return-from synchronize))
             (:fun (return-from synchronize))
             (:var (return-from synchronize))
             (:for (return-from synchronize))
             (:if (return-from synchronize))
             (:while (return-from synchronize))
             (:print (return-from synchronize))
             (:return (return-from synchronize)))
           (advance parser)))



;;; class

(defclass parser ()
  ((tokens :initarg :tokens :accessor parser-tokens)
   (current :initarg :current :initform 0 :accessor parser-current))
  (:documentation "a recursive descent parser"))


;;; parsing

(defun parse (parser)
  (handling-errors
    (expression parser)))

(defun expression (parser)
  (equality parser))

(defun equality (parser)
  (let ((expr (comparison parser)))
    (loop while (match parser :bang-equal :equal)
          do (let ((operator (previous parser))
                   (right (comparison parser)))
               (setf expr
                     (make-instance 'cllox.expr:binary
                                    :left expr
                                    :operator operator
                                    :right right))))
    expr))

(defun comparison (parser)
  (let ((expr (term parser)))
    (loop while
          (match parser
            :greater :greater-equal :less :less-equal)
          do (let ((operator (previous parser))
                   (right (term parser)))
               (setf expr
                     (make-instance 'cllox.expr:binary
                                    :left expr
                                    :operator operator
                                    :right right))))
    expr))

(defun term (parser)
  (let ((expr (factor parser)))
    (loop while (match parser :minus :plus)
          do (let ((operator (previous parser))
                   (right (factor parser)))
               (setf expr
                     (make-instance 'cllox.expr:binary
                                    :left expr
                                    :operator operator
                                    :right right))))
    expr))

(defun factor (parser)
  (let ((expr (unary parser)))
    (loop while (match parser :slash :star)
          do (let ((operator (previous parser))
                   (right (unary parser)))
               (setf expr
                     (make-instance 'cllox.expr:binary
                                    :left expr
                                    :operator operator
                                    :right right))))
    expr))

(defun unary (parser)
  (if (match parser :bang :minus)
      (let ((operator (previous parser))
            (right (unary parser)))
        (make-instance 'cllox.expr:unary
                       :operator operator
                       :right right))
      (primary parser)))

(defun primary (parser)
  ;; TODO difference between false and nil?
  (cond ((match parser :false) (make-instance 'cllox.expr:literal
                                              :value nil))
        ((match parser :true) (make-instance 'cllox.expr:literal
                                             :value t))
        ((match parser :nil) (make-instance 'cllox.expr:literal
                                             :value nil))
        ((match parser :number :string)
         (make-instance 'cllox.expr:literal
                        :value (cllox.token:token-literal
                                (previous parser))))
        ((match parser :left-paren)
         (let ((expr (expression parser)))
           (consume parser :right-paren
                    "expect ')' after expression.")
           (make-instance 'cllox.expr:grouping
                          :expression expr)))
        (t (error 'parser-error :token (peek parser)
                  :reason "expect expression"))))


;;; utilities

(defun consume (parser type message)
  (if (check parser type)
      (advance parser)
      (error 'parser-error :token (peek parser)
             :reason message)))

(defun match (parser &rest types)
  (loop for type in types
        do (when (check parser type)
             (advance parser)
             (return-from match t)))
  nil)

(defun check (parser type)
  (if (at-endp parser)
      nil
      (eql (cllox.token:token-type (peek parser))
           type)))

(defun advance (parser)
  (with-slots (current)
      parser
    (incf current)))

(defun at-endp (parser)
  (eql (cllox.token:token-type (peek parser))
       :eof))

(defun peek (parser)
  (with-slots (tokens current)
      parser
    (nth current tokens)))

(defun previous (parser)
  (with-slots (tokens current)
      parser
    (nth (1- current) tokens)))


;;; parser.lisp ends here
