(in-package #:cloveleaf)

;;;
;;; Ranges
;;;

(defclass ranges ()
  ((name :accessor ranges-name
	 :documentation "A pretty name for a SMuFL range."
	 :type string
	 :initarg :name)
   (description :accessor ranges-description
		:documentation "A description of the range."
		:type string
		:initarg :description)
   (glyphs :accessor ranges-glyphs
	   :documentation "A list of glyph objects."
	   :type list
	   :initarg :glyphs
	   :initform ())
   (end :accessor ranges-end
	:documentation "The last glyph in the unicode range."
	:type number
	:initarg :end)
   (start :accessor ranges-start
	  :documentation "The first glyph in the unicode range."
	  :type number
	  :initarg :start))
  (:documentation
   "SMuFL ranges define unicode blocks of glyphs with a START and END."))

(defun read-ranges (&key
		      (filename (merge-pathnames
				 +cloveleaf-ranges-filename+
				 *cloveleaf-source-directory-pathname*))
		      glyph-table
		      (hash (make-hash-table :test 'equal)))
  "Returns a populated HASH of ranges objects based upon the SMuFL defined
ranges in FILENAME."
  (assert (hash-table-p glyph-table))
  (assert (hash-table-p hash))
  (assert (pathnamep filename))
  (with-open-file (file filename)
    (loop for ranges-def = (read file nil nil)
	  while ranges-def
	  do (let ((name (getf ranges-def :name)))
	       (setf (gethash name hash)
		     (make-instance 'ranges
				    :name name
				    :description (getf ranges-def :description)
				    :start (getf ranges-def :start)
				    :end (getf ranges-def :end)
				    :glyphs (mapcar #'(lambda (g)
							(gethash g glyph-table))
						    (getf ranges-def :glyphs)))))))
  hash)
