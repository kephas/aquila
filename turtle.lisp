 #| Aquila - A prototype converter from Emdros to RDF
    Copyright (C) 2013 Pierre Thierry <pierre@nothos.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>. |#

(in-package :nothos.net/2013.05.aquila)

(defclass turtle-doc ()
  ((current-base-uri :initform "")
   (current-prefixes :initform (make-hash-table :test #'equal))
   (commands :initform nil :accessor commands)))


; RDF graph components

(defclass uriref ()
  ((uri :initarg :uri :reader uri)))

(defclass resource (uriref) ())

(defclass qnamed-resource (resource)
  ((qual :initarg :pref :reader prefix)
   (name :initarg :name :reader name))
  (:default-initargs :uri nil))

(defclass blank (qnamed-resource)
  ()
  (:default-initargs :pref "_" :uri nil))


(defgeneric rdf-eq? (subj1 subj2))

(defmethod rdf-eq? ((subj1 resource) (subj2 resource))
  (equal (uri subj1) (uri subj2)))

(defmethod rdf-eq? ((subj1 blank) (subj2 blank))
  (equal (name subj1) (name subj2)))

(defmethod rdf-eq? ((subj1 resource) (subj2 blank)))
(defmethod rdf-eq? ((subj1 blank) subj2)
  (rdf-eq? subj2 subj1))


; turtle commands

(defclass directive () ())

(defclass base (directive uriref) ())

(defclass prefix (directive uriref)
  ((name :initarg :name :reader name)))

(defclass statement ()
  ((subject :initarg :subj :reader subject)
   (predicates :initform nil :initarg :preds :accessor predicates)))

(defclass predicate ()
  ((verb :initarg :verb :reader verb)
   (objects :initform nil :initarg :objs :accessor objects)))

(defun make-statement (subj pred obj)
  (make-instance 'statement :subj subj :preds (list (make-instance 'predicate :verb pred
								   :objs (list obj)))))


(defun rdf-less? (res1 res2)
  (string-lessp (uri res1) (uri res2)))

(defun merge (target command)
  (make-instance
   'statement :subj (subject target)
   :preds (let@ rec ((stack)
		     (preds1 (predicates target))
		     (preds2 (predicates command)))
	    (let ((verb1 (if-let (pred1 (first preds1)) (verb pred1)))
		  (verb2 (if-let (pred2 (first preds2)) (verb pred2))))
	      (cond
		((and verb1 verb2 (rdf-eq? verb1 verb2))
		 (rec (cons (make-instance 'predicate :verb verb1
					   :objs (remove-duplicates (append (objects (first preds1))
									    (objects (first preds2)))))
			    stack)
		      (rest preds1) (rest preds2)))
		((and verb1 verb2 (rdf-less? verb1 verb2))
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


(defgeneric ensure-rdf-eqness (object doc)
  (:documentation "Return a copy of OBJECT (or itself) such that all references reachable
from it are comparable with RDF-EQ?, with qnames' URIs expanded in the
context of DOC."))

(defmethod ensure-rdf-eqness ((object statement) doc)
  (make-instance 'statement
		 :subj (ensure-rdf-eqness (subject object) doc)
		 :preds	(mapcar (lambda (p) (ensure-rdf-eqness p doc)) (predicates object))))

(defmethod ensure-rdf-eqness ((object predicate) doc)
  (make-instance 'predicate
		 :verb (ensure-rdf-eqness (verb object) doc)
		 :objs (mapcar (lambda (o) (ensure-rdf-eqness o doc)) (objects object))))

(defmethod ensure-rdf-eqness ((object qnamed-resource) doc)
  (if-let (prefix-uri (gethash (prefix object) (slot-value doc 'current-prefixes)))
    (make-instance 'qnamed-resource
		   :pref (prefix object)
		   :name (name object)
		   :uri (format nil "~a~a" prefix-uri (name object)))
    (error "Prefix ~S not found in turtle document." (prefix object))))

(defmethod ensure-rdf-eqness ((object blank) doc)
  object)


(defgeneric add-command (doc command))

(defmethod add-command (doc (command base))
  (push command (commands doc))
  (setf (slot-value doc 'current-base-uri) (uri command)))

(defmethod add-command (doc (command prefix))
  (push command (commands doc))
  (setf (gethash (name command) (slot-value doc 'current-prefixes))
	(uri command)))

(defmethod add-command (doc (command statement))
  (let ((command (ensure-rdf-eqness command doc)))
    (if (commands doc)
	(let ((last (pop (commands doc))))
	  (if (mergeable? last command)
	      (push (merge last command) (commands doc))
	      (progn
		(push last (commands doc))
		(push command (commands doc)))))
	(push command (commands doc)))))


(defgeneric turtlize (object)
  (:documentation "Return the turtle syntax representing OBJECT."))

(defmethod turtlize ((object base))
  (format nil "@base <~a>" (uri object)))

(defmethod turtlize ((object prefix))
  (format nil "@prefix ~a:~a" (name object) (uri object)))

(defmethod turtlize ((object resource))
  (format nil "<~a>" (uri object)))

(defmethod turtlize ((object qnamed-resource))
  (format nil "~a:~a" (prefix object) (name object)))

(defmethod turtlize ((object string)) object)


(defun turtlize-list (list separator)
  (with-output-to-string (out)
    (let@ rec ((separate? nil)
	       (elements list))
      (when elements
	(if separate? (format out " ~a " separator))
	(princ (turtlize (first elements)) out)
	(rec t (rest elements))))))

(defmethod turtlize ((object statement))
  (format nil "~a ~a ." (turtlize (subject object)) (turtlize-list (predicates object) ";")))

(defmethod turtlize ((object predicate))
  (format nil "~a ~a" (turtlize (verb object)) (turtlize-list (objects object) ",")))


(defun view-turtle (doc)
  (with-output-to-string (out)
    (let@ rec ((commands (reverse (commands doc))))
      (let ((next (first commands)))
	(when next
	  (format out "~a~%" (turtlize next))
	  (rec (rest commands)))))))
#|

REPL goodies

|#

(defun print-rdf (object stream)
  (print-unreadable-object (object stream)
    (format stream "rdf ~a " (turtlize object))))

(defmethod print-object ((object resource) stream) (print-rdf object stream))

(defmethod print-object ((object statement) stream) (print-rdf object stream))
