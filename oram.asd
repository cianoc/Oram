;;;; Oram.asd

(asdf:defsystem #:oram
  :description "Describe Oram here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:incudine #:alexandria)
  :components ((:file "package")
               (:file "oram")
               (:file "wave")))
