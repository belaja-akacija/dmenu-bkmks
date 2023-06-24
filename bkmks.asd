(defsystem "bkmks"
  :description "bkms: unix bookmark management that sucks less. Lisp edition!"
  :version "0.5.1"
  :author "belaja-akacija"
  :depends-on ("cl-ppcre" "modest-config")
  :components ((:file "utils")
               (:file "config" :depends-on ("utils"))
               (:file "main" :depends-on ("utils" "config")))
  :build-operation "program-op"
  :build-pathname "bkmks"
  :entry-point "main"
  :in-order-to ((test-op (test-op "bkmks/tests"))))

(defsystem "bkmks/tests"
  :depends-on ("bkmks" "modest-config")
  :components ((:file "tests/bkmks-tests")))
