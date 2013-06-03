(in-package :nothos.net/2013.05.aquila)

#|

Emdros stuff

|#

(def-view-class emdros-object ()
  ((id :column object-id-d :type integer)))

(def-view-class emdros-range ()
  ((first-monad :type integer)
   (last-monad :type integer)))

(def-view-class book (emdros-object emdros-range)
  ((mdf-book :type integer)
   (%chapters :reader chapters-of
	      :db-kind :join
	      :db-info (:join-class chapter
				    :home-key mdf-book
				    :foreign-key mdf-book)))
  (:base-table book-objects))

(def-view-class chapter (emdros-object emdros-range)
  ((mdf-chapter :type integer)
   (mdf-book :type integer)
   (%book :reader in-book
	  :db-kind :join
	  :db-info (:join-class book :home-key mdf-book
				:foreign-key mdf-book
				:set nil)))
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
  (print-unreadable-object (object stream :type t)
    (princ (slot-value object 'id) stream)
    (princ (%monads-range object) stream)))
