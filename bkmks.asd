(defsystem "bkmks"
  :description "bkms: unix bookmark management that sucks less. Lisp edition!"
  :version "0.5.4"
  :author "belaja-akacija"
  ;:depends-on ("cl-ppcre" "modest-config" "vlime")
  :depends-on ("cl-ppcre" "modest-config" "vlime")
  :components ((:file "utils")
               (:file "config" :depends-on ("utils"))
               (:file "main" :depends-on ("utils" "config")))
  :build-operation "program-op"
  :build-pathname "bkmks"
  :entry-point "main"
  :in-order-to ((test-op (test-op "bkmks/tests"))))

(defsystem "bkmks/tests"
  :depends-on ("bkmks" "fiveam")
  :components ((:file "tests/bkmks-tests")))
