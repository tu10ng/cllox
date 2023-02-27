(require "uiop")

(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition (err) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

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
    (handling-errors
     (finish-output)
     (let ((line (read-line)))
       (when (member line
                     '("quit" "exit" "")
                     :test #'string=)
         (return-from run-repl))
       (run line)))))

(defun run (src)
  (format t "~a~%" src))

(defun print-error (line file message)
  (format *error-output* "[~a] Error ~a: ~a" line file message)) 



(main)
