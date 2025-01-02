(in-package #:cloveleaf)

;;;
;;; classes
;;;

(defclass classes ()
  ((name :accessor classes-name
	 :documentation "The name of a class of SMuFL glyphs."
	 :type string
	 :initarg :name)
   (glyphs :accessor classes-glyphs
	   :documentation "A list of glyphs that belong to this class."
	   :type list
	   :initarg :glyphs
	   :initform ()))
  (:documentation "SMuFL classes are groups of glyphs."))

(defun read-classes (&key (filename (merge-pathnames
				     +cloveleaf-classes-filename+
				     *cloveleaf-source-directory-pathname*))
		       glyph-table
		       (hash (make-hash-table :test 'equal)))
  "Returns a populated HASH of classes hashed on CLASSES-NAME."
  (assert (hash-table-p glyph-table))
  (assert (hash-table-p hash))
  (with-open-file (file filename)
    (loop for classes-def = (read file nil nil)
	  while classes-def
	  do (let ((name (getf classes-def :name))
		   (glyphs (getf classes-def :glyphs)))
	       (setf (gethash name hash)
		     (make-instance 'classes
				    :glyphs (mapcar #'(lambda (g)
							(gethash g glyph-table))
						    glyphs)
				    :name name)))))
  hash)
