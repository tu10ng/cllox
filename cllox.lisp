(defpackage :cllox
  (:use :cl :uiop :string-case :parse-number)
  (:export
   #:main))

(in-package :cllox)


;; constants

(defparameter *keywords*
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key value) in
          '(("and" :and)
            ("class" :class)
            ("else" :else)
            ("false" :false)
            ("for" :for)
            ("fun" :fun)
            ("if" :if)
            ("nil" :nil)
            ("or" :or)
            ("print" :print)
            ("return" :return)
            ("super" :super)
            ("this" :this)
            ("true" :true)
            ("var" :var)
            ("while" :while))
          do (setf (gethash key ht) value))
    ht))


;; error

(defmacro handling-errors (&body body)
  "https://www.cliki.net/REPL"
  `(handler-bind
       ((run-error
          #'(lambda (e)
              (format *error-output*
                      "got error: ~a~%" e)
              (invoke-restart 'ignore)))
        (simple-condition
          #'(lambda (err)
              (format *error-output* "~&~A: ~%" (class-name (class-of err)))
              (apply (function format) *error-output*
                     (simple-condition-format-control   err)
                     (simple-condition-format-arguments err))
              (format *error-output* "~&")
              (sb-ext:exit)))
        (end-of-file
          #'(lambda (e)
              (format *error-output* "goodbye~~~%")
              (sb-ext:exit)))
        (sb-sys:interactive-interrupt
          #'(lambda (err)
              (format *error-output* "~&~A: ~%  ~S~%goodbye~%"
                      (class-name (class-of err)) err)
              (sb-ext:exit))))
     (progn ,@body)))

(define-condition run-error (error)
  ((reason :initarg :reason :reader run-error-reason)
   (line :initarg :line :reader run-error-line))
  (:documentation "signaled during running code???")
  (:report (lambda (c s)
             (with-slots (reason line)
                 c
               (format s "~a on line ~a" reason line)))))


;;; token

(defclass token ()
  ((type :initarg :type :reader token-type)
   (lexeme :initarg :lexeme :reader token-lexeme)
   (literal :initarg :literal :reader token-literal)
   (line :initarg :line :reader token-line))
  (:documentation "a parsed token"))

(defmethod print-object ((token token) s)
  "Output a token to a stream. from lexer.lisp"
  (print-unreadable-object (token s :type t)
    (with-slots (type lexeme literal)
        token
      (format s "~a ~a ~a" type lexeme literal))))


;;; scanner

;; is this class unnecessary? it doesnt have inheritence
(defclass scanner ()
  ((source :initarg :source :accessor scanner-source
           :documentation "raw src code")
   (tokens :initarg :tokens :initform '() :accessor scanner-tokens
           :documentation "list of tokens")
   (start :initarg :start :initform 0 :accessor scanner-start)
   (current :initarg :current :initform 0 :accessor scanner-current)
   (line :initarg :line :initform 1 :accessor scanner-line))
  (:documentation "a handcraft scanner"))

(defun scan-tokens (scanner)
  (with-slots (source tokens start current line)
      scanner
    (do ()
        ((at-endp scanner))
      (setf start current)
      (scan-token scanner))
    ;; add eof to the end
    (setf tokens
          (cons (make-instance 'token
                               :type :eof
                               :lexeme ""
                               :literal nil
                               :line line)
                tokens))))

(defmacro add-token (type lexeme &optional literal)
  "literal will be nil if not provided."
  `(setf tokens
         (cons (make-instance 'token
                              :type ,type
                              :lexeme ,lexeme
                              :literal ,literal
                              :line line)
               tokens)))

(defmacro 1char-token-case (keyform &body lst)
  `(case ,keyform
     ,@(loop for (a b) in lst collect
             `(,a (add-token ,b (subseq source start current))
                  (advance scanner)
                  ;; maybe this return is not needed
                  (return-from scan-token)))))

(defmacro 2char-token-case (keyform &body lst)
  "string-case need extra paren around keyform"
  `(string-case:string-case (,keyform :default nil)
     ,@(loop for (a b) in lst collect
             `(,a (add-token ,b (subseq source start current))
                  (advance scanner)
                  (advance scanner)
                  (return-from scan-token)))))

(defmacro space-token-case (keyform lst)
  `(case ,keyform
     ,@(loop for a in lst collect
             `(,a (advance scanner)
                  (return-from scan-token)))))

(defun scan-token (scanner)
  (with-slots (source tokens start current line)
      scanner

    (when (< (1+ current) (length source))
      (let ((str (subseq source current (+ 2 current))))
        (2char-token-case str
          ("!=" :bang-equal)
          ("==" :equal-equal)
          ("<=" :less-equal)
          (">=" :greater-equal))
        (when (string= str "//")        ; comment
          (do ()
              ;; newline will be handled later
              ((eql (peek scanner) #\newline))
            (advance scanner)))))
    
    (space-token-case (peek scanner)
                      (#\space
                       #\return
                       #\tab
                       #\newline))
    
    ;; != need be tested before ! 
    (1char-token-case (peek scanner)
      (#\( :left-paren)
      (#\) :right-paren)
      (#\{ :left-brace)
      (#\} :right-brace)
      (#\, :comma)
      (#\. :dot)
      (#\- :minus)
      (#\+ :plus)
      (#\; :semicolon)
      (#\* :star)
      (#\! :bang)
      (#\= :equal)
      (#\< :less)
      (#\> :greater)
      (#\/ :slash))

    ;; string
    (when (eql #\" (peek scanner))
      (scan-string scanner)
      (return-from scan-token))
    
    (when (digit-char-p (peek scanner))
      (scan-number scanner)
      (return-from scan-token))

    (when (alpha-char-p (peek scanner))
      (scan-identifier scanner)
      (return-from scan-token))
    
    (error 'run-error :reason "Unexpected character"
                      :line line)))

(defun scan-string (scanner)
  (with-slots (source tokens start current line)
      scanner
    (advance scanner)
    (when (eql (peek scanner) #\")
      (advance scanner)
      (add-token :string
                 (subseq source start current)
                 (subseq source (1+ start) (1- current)))
      (return-from scan-string))
    
    (when (eql (peek scanner) #\newline)
      (setf line (1+ line)))

    (when (at-endp scanner)
      (error 'run-error :reason "Unterminated string"
                        :line line))
    (scan-string scanner)))

(defun scan-number (scanner)
  (with-slots (source tokens start current line)
      scanner
    (do ()
        ((or (null (peek scanner))
             (null (or (digit-char-p (peek scanner))
                       (eql #\. (peek scanner))))))
      (setf current (1+ current)))
    (add-token :number
               (subseq source start current)
               (parse-number:parse-number (subseq source start current)))))

(defun scan-identifier (scanner)
  (with-slots (source tokens start current line)
      scanner
    (do ()
        ((or (null (peek scanner))
             (null (alpha-char-p (peek scanner)))))
      (advance scanner))
    
    (let ((type (gethash (subseq source start current) *keywords*)))
      (when (eql type nil)
        (setf type :indentifier))
      (add-token type (subseq source start current)))))

(defun at-endp (scanner)
  (with-slots (source current)
      scanner
    (>= current (length source))))

(defun peek (scanner)
  "look into the char at current"
  (with-slots (source current)
      scanner
    (unless (at-endp scanner)
      (elt source current))))

(defun advance (scanner)
  (with-slots (current)
      scanner
    (setf current (1+ current))))


;;; token-type

(defparameter *token-type*
  '(:left-paren :right-paren
    :left-brace :right-brace
    :comma :dot :minus :plus :semicolon :slash :star

    :bang :bang-equal
    :equal :equal-equal
    :greater :greater-equal
    :less :less-equal

    ;; literals
    :identifier :string :number

    :and :class :else :false :fun :for :if :nil :or
    :print :return :super :this :true :var :while

    :eof))


;;; entry function

(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (len (length args)))
    (cond
      ((> len 1) (format t "Usage: cllox [script]~%"))
      ((= len 1) (run-file (first args)))
      (t (run-repl)))))

(defun run-file (path)
  (run (uiop:read-file-string path)))

(defun run-repl ()
  (do ((hist 1 (1+ hist)))
      (nil)
    (format t "~%lox[~a]> " hist)
    (finish-output)
    (handling-errors
      (let ((line (read-line)))
        (when (member line '("quit" "exit" "")
                      :test #'string=)
          (return-from run-repl))
        (run line)))))

(defun run (src)
  (restart-case
      (progn
        (format t "~a~%" src)
        (let ((scanner (make-instance 'scanner
                                      :source src)))
          (scan-tokens scanner)
          (format t "~{~a ~}~%" (scanner-tokens scanner))))
    (ignore ()
      :report "ignore error in running"
      nil)))


;;; finish up

;;(main)

;;; cllox.lisp ends here

