(in-package #:cloveleaf)

(defun read-smufl-data (&key (directory *cloveleaf-source-directory-pathname*)
			  (glyphnames +cloveleaf-glyphnames-filename+)
			  (classes +cloveleaf-classes-filename+)
			  (ranges +cloveleaf-ranges-filename+)
			  (hash (make-hash-table :test #'equal)))
  "Reads in all smufl data and returns HASH with added SMuFL data.
The three keys are \"glyphnames\", \"classes\", and \"ranges\", which
are read from the associated files."
  (assert (hash-table-p hash))
  (assert (or (stringp directory)
	      (pathnamep directory)))
  (assert (or (stringp glyphnames)
	      (pathnamep glyphnames)))
  (assert (or (stringp classes)
	      (pathnamep classes)))
  (assert (or (stringp ranges)
	      (pathnamep ranges)))
  (let ((glyphdata (read-glyphnames :filename (merge-pathnames glyphnames directory))))
    (setf (gethash +smufl-metadata-glyphnames-key+  hash) glyphdata)
    (setf (gethash +smufl-metadata-classes-key+ hash)
	  (read-classes :filename (merge-pathnames classes directory)
			:glyph-table glyphdata))
    (setf (gethash +smufl-metadata-ranges-key+ hash)
	  (read-ranges :filename (merge-pathnames ranges directory)
		       :glyph-table glyphdata)))
  hash)
