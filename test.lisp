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

(defun qref (pref name &optional uri)
  (make-instance 'qnamed-resource :pref pref :name name :uri uri))

(defun make-test-doc ()
  (let ((doc (make-instance 'turtle-doc)))
    (add-command doc (make-instance 'base :uri "http://vu.nl/id/"))
    (add-command doc (make-instance 'prefix :name "obj" :uri "objects/"))
    (add-command doc (make-instance 'prefix :name "voc" :uri "terms/"))
    (add-command doc (make-instance 'prefix :name "mon" :uri "monads/"))
    (add-command doc (make-statement (qref "obj" "1") (qref "voc" "hasChapter") (qref "obj" "2")))
    (add-command doc (make-statement (qref "obj" "1") (qref "voc" "hasChapter") (qref "obj" "3")))
    (add-command doc (make-statement (qref "obj" "1") (qref "voc" "firstMonad") (qref "mon" "1")))
    (add-command doc (make-statement (qref "obj" "1") (qref "voc" "lastMonad") (qref "mon" "5432")))
    (add-command doc (make-statement (qref "obj" "2") (qref "voc" "firstMonad") (qref "mon" "1")))
    (add-command doc (make-statement (qref "obj" "2") (qref "voc" "lastMonad") (qref "mon" "317")))
    doc))
