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

(defgeneric dump-rdf (object stream))

(defmethod dump-rdf :after (object stream)
  (format stream ".~%"))

(defmethod dump-rdf (object stream))

(defmethod dump-rdf :before ((object emdros-object) stream)
  (format stream "obj:~a a voc:~a; "
	  (slot-value object 'id)
	  (string-downcase (type-of object))))

(defmethod dump-rdf ((object emdros-range) stream)
  (with-slots (first-monad last-monad) object
    (format stream "voc:first mon:~a; voc:last mon:~a; "
	    first-monad last-monad))
  (call-next-method))
				     
(defmethod dump-rdf ((object book) stream)
  (with-slots (mdf-book) object
    (format stream "voc:book-number ~a; " mdf-book))
  (call-next-method))

(defmethod dump-rdf ((object chapter) stream)
  (with-slots (mdf-chapter) object
    (format stream "voc:chapter-number ~a; " mdf-chapter))
  (call-next-method))

(defmethod dump-rdf ((object verse) stream)
  (with-slots (mdf-verse mdf-verse-label) object
    (format stream "voc:verse-number ~a; voc:verse-label ~s; " mdf-verse mdf-verse-label))
  (call-next-method))

(defmethod dump-rdf ((object half-verse) stream)
  (with-slots (mdf-half-verse) object
    (format stream "voc:half-verse voc:half-~a; " mdf-half-verse))
  (call-next-method))
