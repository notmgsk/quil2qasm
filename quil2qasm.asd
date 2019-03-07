;;;; quil2qasm.asd

(asdf:defsystem #:quil2qasm
  :description "Describe quil2qasm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexa #:parse-float)
  :components ((:file "package")
               (:file "quil2qasm")))
