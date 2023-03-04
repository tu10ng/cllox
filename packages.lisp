(in-package :cl-user)

(defpackage :cllox
  (:use :cl)
  (:export
   #:main
   #:run-error))

(defpackage :cllox.scanner
  (:use :cl)
  (:import-from :string-case #:string-case)
  (:import-from :parse-number #:parse-number)
  (:export
   #:scanner
   #:scan-tokens
   #:scanner-tokens))

(defpackage :cllox.token
  (:use :cl)
  (:export
   #:token
   #:token-type
   #:token-lexeme
   #:token-literal
   #:token-line))

(defpackage :cllox.expr
  (:use :cl)
  (:export
   #:binary
   #:grouping
   #:literal
   #:unary
   #:ast))
  
(defpackage :cllox.parser
  (:use :cl)
  (:export
   #:parser
   #:parse))

