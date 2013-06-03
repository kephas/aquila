(def-view-class book ()
  ((id :column object-id-d :type integer)
   (first-monad :type integer)
   (last-monad :type integer)
   (mdf-book :type integer)
   (%chapters :reader chapters-of
	      :db-kind :join
	      :db-info (:join-class chapter
				    :home-key mdf-book
				    :foreign-key mdf-book)))
  (:base-table book-objects))

(def-view-class chapter ()
  ((id :column object-id-d :type integer)
   (first-monad :type integer)
   (last-monad :type integer)
   (mdf-chapter :type integer)
   (mdf-book :type integer)
   (%book :reader in-book
	  :db-kind :join
	  :db-info (:join-class book :home-key mdf-book
				:foreign-key mdf-book
				:set nil)))
  (:base-table chapter-objects))
