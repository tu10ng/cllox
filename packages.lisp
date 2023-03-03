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
   #:token))
