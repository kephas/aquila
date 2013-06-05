(defpackage :nothos.net/2013.05.aquila-system
  (:use :common-lisp :asdf))

(in-package :nothos.net/2013.05.aquila-system)

(defsystem "aquila"
  :description "Prototype converter from Emdros to RDF"
  :version "0.1.0"
  :author "Pierre Thierry <pierre@nothos.net>"
  :licence "AGPL"
  :depends-on ("alexandria" "clsql" "thierry-macros")
  :components ((:file "package")
	       (:file "emdros")
	       (:file "turtle"))
  :serial t)
