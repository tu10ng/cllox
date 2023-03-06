(defsystem "cllox"
  :depends-on ("string-case" "parse-number")
  :serial t
  :components ((:file "packages")
               (:file "token")
               (:file "scanner")
               (:file "expr")
               (:file "parser")
               (:file "interpreter")
               (:file "cllox"))
  :build-operation "program-op"
  :build-pathname "cllox"
  :entry-point "cllox:main")

