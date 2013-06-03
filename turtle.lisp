(in-package :nothos.net/2013.05.aquila)

(defclass turtle-doc ()
  ((current-base-uri :initform "")
   (current-prefixes :initform (make-hash-table :test #'string-equal))
   (commands :initform nil)))


; RDF graph components

(defclass resource ()
  ((uri :initarg :uri)))

(defclass qnamed-resource (resource)
  ((qual :initarg :qual)
   (name :initarg :name)))

(defclass blank ()
  ((id :initarg :id)))


; turtle commands

(defclass directive () ())

(defclass base (directive)
  ((uri :initarg :uri)))

(defclass prefix (directive)
  ((name :initarg :name)
   (uri :initarg :uri)))

(defclass statement ()
  ((subject :initarg :subj)
   (verb :initarg :verb)
   (object :initarg :obj)))

(defclass predicate-list ()
  ((subject :initarg :subj)
   (predicates :initform nil :accessor predicates)))

(defclass predicate ()
  ((verb :initarg :verb)
   (object :initarg :obj)))

(defclass object-list ()
  ((verb :initarg :verb)
   (objects :initform nil :accessor objects)))


(defgeneric merge (target command))

(defgeneric mergeable? (target command)
  (:documentation "Is COMMAND mergeable into TARGET?"))

(defmethod mergeable? (target command)) ; default case: NO
