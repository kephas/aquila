(in-package :nothos.net/2013.05.aquila)

(defclass turtle-doc ()
  ((current-base-uri :initform "")
   (current-prefixes :initform (make-hash-table :test #'equal))
   (commands :initform nil :accessor commands)))


; RDF graph components

(defclass resource ()
  ((uri :initarg :uri :reader uri)))

(defclass qnamed-resource (resource)
  ((qual :initarg :qual)
   (name :initarg :name)))

(defclass blank ()
  ((id :initarg :id :reader id)))


(defgeneric rdf-eq? (subj1 subj2))

(defmethod rdf-eq? ((subj1 resource) (subj2 resource))
  (equal (uri subj1) (uri subj2)))

(defmethod rdf-eq? ((subj1 blank) (subj2 blank))
  (equal (id subj1) (id subj2)))

(defmethod rdf-eq? ((subj1 resource) (subj2 blank)))
(defmethod rdf-eq? ((subj1 blank) subj2)
  (rdf-eq? subj2 subj1))


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

(defun add-command (doc command)
  (if (commands doc)
      (let ((last (pop (commands doc))))
	(if (mergeable? last command)
	    (push (merge last command) (commands doc))
	    (push command (commands doc))))
      (push command (commands doc))))
