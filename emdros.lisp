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

#|

Emdros stuff

|#

(def-view-class emdros-object ()
  ((id :column object-id-d :type integer)))

(def-view-class emdros-range ()
  ((first-monad :type integer)
   (last-monad :type integer)))

(defun %sym (format sym)
  (intern (string-upcase (format nil format sym))))

(defmacro define-containee-view-class (field)
  (let ((class (%sym "emdros-in-~a" field))
	(slot (%sym "mdf-~a" field))
	(reader (%sym "in-~a" field))
	(join-slot (%sym "%~a" field))
	(join-class (%sym "~a" field)))
    `(def-view-class ,class ()
       ((,slot :type integer)
	(,join-slot :reader ,reader
		    :db-kind :join
		    :db-info (:join-class ,join-class :home-key ,slot
					  :foreign-key ,slot
					  :set nil))))))

(define-containee-view-class "book")
(define-containee-view-class "chapter")

(def-view-class book (emdros-object emdros-range)
  ((mdf-book :type integer)
   (%chapters :reader chapters-of
	      :db-kind :join
	      :db-info (:join-class chapter
				    :home-key mdf-book
				    :foreign-key mdf-book)))
  (:base-table book-objects))

(def-view-class chapter (emdros-object emdros-range emdros-in-book)
  ((mdf-chapter :type integer))
  (:base-table chapter-objects))


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
