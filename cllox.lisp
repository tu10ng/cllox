(in-package :cllox)


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
        (format *error-output* "src:~%---~%~a~%---~%" src)
        (let* ((scanner (make-instance 'cllox.scanner:scanner
                                       :source src))
               (tokens (progn
                         (cllox.scanner:scan-tokens scanner)
                         (cllox.scanner:scanner-tokens
                          scanner))))
          (format *error-output* "tokens:~%---~%~a~%---~%" tokens)
          (let* ((parser
                  (make-instance
                   'cllox.parser:parser :tokens tokens))
                 (expr
                   (cllox.parser:parse parser)))
            (format *error-output* "ast:~%---~%~a~%"
                    (cllox.expr:ast expr))
            (format *error-output* "ir:~%---~%")
            (format t "~a~%"
                    (cllox.interpreter:interpret expr)))))
    (ignore ()
      :report "ignore error in running"
      nil)))


;;; finish up

;;(main)

;;; cllox.lisp ends here

