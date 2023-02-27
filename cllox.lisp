(require "uiop")



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
              (exit)))
        (sb-sys:interactive-interrupt
          #'(lambda (err)
              (format *error-output* "~&~A: ~%  ~S~%"
                      (class-name (class-of err)) err)
              (exit))))
     (progn ,@body)))


(define-condition run-error (error)
  ((reason :initarg :reason :reader run-error-reason)
   (line :initarg :line :reader run-error-line)
   (source :initarg :source :reader run-error))
  (:documentation "signaled during running code???")
  (:report (lambda (c s)
             (with-slots (reason line source)
                 c
               (format s "~a on line ~a~@[ of ~a~]" reason line source)))))



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
        (format t "~a~%" src))
    (ignore ()
      :report "ignore error in running"
      nil)))




(main)
