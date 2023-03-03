(defsystem "cllox"
  :depends-on ("string-case" "parse-number")
  :components ((:file "cllox"))
  :build-operation "program-op"
  :build-pathname "cllox"
  :entry-point "cllox:main")

