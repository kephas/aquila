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
  ((qual :initarg :qual :reader prefix)
   (name :initarg :name :reader name))
  (:default-initargs :uri nil))

(defclass blank (qnamed-resource)
  ()
  (:default-initargs :qual "_" :uri nil))


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

(defclass base (directive uriref) ())

(defclass prefix (directive uriref)
  ((name :initarg :name)))

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

#|

REPL goodies

|#

(defun print-rdf (object stream)
  (print-unreadable-object (object stream)
    (format stream "rdf ~a " (turtlize object))))

(defmethod print-object ((object resource) stream) (print-rdf object stream))

(defmethod print-object ((object statement) stream) (print-rdf object stream))
