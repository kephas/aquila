(in-package :nothos.net/2013.05.aquila)

(defclass turtle-doc ()
  ((current-base-uri :initform "")
   (current-prefixes :initform (make-hash-table :test #'equal))
   (commands :initform nil :accessor commands)))


; RDF graph components

(defclass resource ()
  ((uri :initarg :uri :reader uri)))

(defclass qnamed-resource (resource)
  ((qual :initarg :qual :reader prefix)
   (name :initarg :name :reader name)))

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
  ((subject :initarg :subj :reader subject)
   (predicates :initform nil :initarg :preds :accessor predicates)))

(defclass predicate ()
  ((verb :initarg :verb :reader verb)
   (objects :initform nil :initarg :objs :accessor objects)))


(defun rdf-less? (res1 res2)
  (string-lessp (uri res1) (uri res2)))

(defun merge (target command)
  (make-instance
   'statement :subj (subject target)
   :preds (let@ rec ((stack)
		     (preds1 (predicates target))
		     (preds2 (predicates command)))
	    (let ((verb1 (verb (first preds1)))
		  (verb2 (verb (first preds2))))
	      (cond
		((rdf-eq? verb1 verb2)
		 (rec (cons (make-instance 'predicate :verb verb1
					   :objs (remove-duplicates (append (objects (first preds1))
									    (objects (first preds2)))))
			    stack)
		      (rest preds1) (rest preds2)))
		((rdf-less? verb1 verb2)
		 (rec (cons (first preds1) stack) (rest preds1) preds2))
		((null preds2)
		 (append (reverse stack) preds1))
		(t
		 (rec stack preds2 preds1)))))))

(defgeneric mergeable? (target command)
  (:documentation "Is COMMAND mergeable into TARGET?"))

(defmethod mergeable? (target command)) ; default case: NO

(defmethod mergeable? ((target statement) (command statement))
  (rdf-eq? (subject target) (subject command)))

(defun add-command (doc command)
  (if (commands doc)
      (let ((last (pop (commands doc))))
	(if (mergeable? last command)
	    (push (merge last command) (commands doc))
	    (push command (commands doc))))
      (push command (commands doc))))


(defgeneric turtlize (object)
  (:documentation "Return the turtle syntax representing OBJECT."))

(defmethod turtlize ((object resource))
  (format nil "<~a>" (uri object)))

(defmethod turtlize ((object qnamed-resource))
  (format nil "~a:~a" (prefix object) (name object)))

#|

REPL goodies

|#

(defun print-rdf (object stream)
  (print-unreadable-object (object stream)
    (format stream "rdf ~a " (turtlize object))))

(defmethod print-object ((object resource) stream) (print-rdf object stream))
