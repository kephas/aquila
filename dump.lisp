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

(defun %bounds-condition (object range?)
  [and
  [>= [first-monad] (slot-value object 'first-monad)]
  [<= (if range? [last-monad] [first-monad]) (slot-value object 'last-monad)]])

(defun dump-within-object (object dumper)
  (let ((range-condition (%bounds-condition object t))
	(word-condition (%bounds-condition object nil))
	(range-class (find-class 'emdros-range)))
    (dolist (type *object-types*)
      (dolist (dumpee (select type :where (if (find range-class (closer-mop:class-precedence-list (find-class type)))
					      range-condition word-condition)
			      :flatp t))
	(funcall dumper dumpee)))))
