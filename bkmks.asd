(defsystem "bkmks"
  :description "bkms: unix bookmark management that sucks less. Lisp edition!"
  :version "0.5.1"
  :author "belaja-akacija"
  :depends-on ("cl-ppcre")
  :components ((:file "utils")
               (:file "main" :depends-on ("utils")))
  :build-operation "program-op"
  :build-pathname "bkmks"
  :entry-point "main")
