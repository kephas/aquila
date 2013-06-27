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
(file-enable-sql-reader-syntax)

#|

Emdros stuff

|#

(def-view-class emdros-object ()
  ((id :column object-id-d :type integer)))

(def-view-class emdros-range ()
  ((first-monad :type integer)
   (last-monad :type integer)))

(defun %sym (format sym)
  (intern (format nil format sym)))

(defun container/ee-view-class-code (join-class class-fmt reader-fmt slot? set?)
  (let ((class (%sym class-fmt join-class))
	(slot (%sym "MDF-~a" join-class))
	(reader (%sym reader-fmt join-class))
	(join-slot (%sym "%~a" join-class)))
    `(def-view-class ,class ()
       ,(append
	 (if slot? `((,slot :type integer)))
	 `((,join-slot :reader ,reader
		       :db-kind :join
		       :db-info (:join-class ,join-class :home-key ,slot
					     :foreign-key ,slot
					     :set ,set?)))))))

(defmacro define-containee-view-class (join-class)
  (container/ee-view-class-code join-class "EMDROS-IN-~a" "IN-~a" t nil))

(defmacro define-container-view-class (join-class)
  (container/ee-view-class-code join-class "EMDROS-HAS-~a" "~aS-OF" nil t))

(define-containee-view-class book)
(define-containee-view-class chapter)
(define-container-view-class chapter)
(define-container-view-class verse)

(def-view-class book (emdros-object emdros-range emdros-has-chapter emdros-has-verse)
  ((mdf-book :type integer))
  (:base-table book-objects))

(def-view-class chapter (emdros-object emdros-range emdros-in-book)
  ((mdf-chapter :type integer))
  (:base-table chapter-objects))

(def-view-class verse (emdros-object emdros-range emdros-in-book emdros-in-chapter)
  ((mdf-verse :type integer)
   (mdf-verse-label :type string))
  (:base-table verse-objects))

(def-view-class half-verse (emdros-object emdros-range)
  ((mdf-half-verse :type string))
  (:base-table half-verse-objects))


(def-view-class emdros-with-parents ()
  ((mdf-parents :type string)))

(def-view-class emdros-with-mother ()
  ((mdf-mother :type integer)))

(def-view-class emdros-with-monads ()
  ((monads :type string)))


(def-view-class sentence-atom (emdros-object emdros-range emdros-with-parents)
  ((mdf-sentence-atom-number :type integer))
  (:base-table sentence-atom-objects))

(def-view-class sentence (emdros-object emdros-range emdros-with-monads)
  ((mdf-number-within-chapter :type integer))
  (:base-table sentence-objects))


(def-view-class phrase-atom (emdros-object emdros-range emdros-with-parents emdros-with-mother)
  ()
  (:base-table phrase-atom-objects))

(def-view-class phrase (emdros-object emdros-range emdros-with-parents emdros-with-monads)
  ()
  (:base-table phrase-objects))

(def-view-class subphrase (emdros-object emdros-range emdros-with-parents emdros-with-mother)
  ((mdf-subphrase-kind :type integer)
   (mdf-subphrase-type :type integer))
  (:table subphrase-objects))


(def-view-class clause-atom (emdros-object emdros-range emdros-with-parents)
  ((mdf-clause-atom-type :type integer)
   (mdf-clause-atom-number :type integer))
  (:base-table clause-atom-objects))

(def-view-class clause (emdros-object emdros-range emdros-with-parents)
  ()
  (:base-table clause-objects))


(def-view-class word (emdros-object)
  ((first-monad :type integer))
  (:base-table word-objects))


(defvar *object-types* '(book chapter verse half-verse
			 sentence sentence-atom phrase phrase-atom subphrase
			 clause clause-atom word))

(defun get-object (id)
  (let@ rec ((types *object-types*))
    (when types
      (if-let (found (select (first types) :where [= [object-id-d] id] :flatp t))
	(first found)
	(rec (rest types))))))


(defgeneric parents (object))

(defmethod parents ((object emdros-object))) ; for objects with no parents field

(defmethod parents ((object emdros-with-parents))
  (mapcar #'get-object
	  (mapcar #'parse-integer
		  (remove "" (split-sequence #\ (slot-value object 'mdf-parents)) :test #'equal))))


#|

REPL goodies

|#

(defgeneric %monads-range (object)
  (:documentation "Returns a string displaying the range of monads of OBJECT."))

(defmethod %monads-range (object)
  "") ; not a range -> nothing to display

(defmethod %monads-range ((object emdros-range))
  (format nil " {~a-~a}" (slot-value object 'first-monad) (slot-value object 'last-monad)))

(defmethod print-object ((object emdros-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (slot-value object 'id) stream)
    (princ (%monads-range object) stream)))
