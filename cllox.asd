(defsystem "cllox"
  :depends-on ("string-case" "parse-number")
  :components ((:file "cllox"))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "cllox"
  :entry-point "cllox:main")

