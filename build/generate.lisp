(uiop:define-package cloveleaf/smufl
  (:use #:cl #:com.inuoe.jzon #:cloveleaf))
(in-package #:cloveleaf/smufl)

(defvar *smufl-metadata-directory-pathname*
  nil
  "A default pathname to use for the SMuFL metadata.")

(defvar *cloveleaf-source-directory-pathname*
  (car (directory (asdf:system-relative-pathname (asdf:find-system :cloveleaf)
						 "src")))
  "A directory pathname to install the generated filenames into.")


(defparameter +smufl-metadata-glyphnames-filename+
  "glyphnames.json"
  "The filename of the SMuFL glyphnames.json file to read in.")
(defparameter +cloveleaf-glyphnames-filename+
  "glyphnames.lisp"
  "The filename for the cloveleaf glyphname definitions.")
(defparameter +smufl-metadata-classes-filename+
  "classes.json"
  "The filename of the SMuFL classes.json file to read in.")
(defparameter +smufl-metadata-classes-ranges.json+
  "ranges.json"
  "The filename of the SMuFL ranges.json file to read in.")

;;; We want to read in the SMuFL distributed files and emit lisp forms
;;; that will be used for our purposes.  First we look at emitting
;;; the glyphnames.json file.  The definition of a glyph exists in the
;;; clovetree source code, so the emitted code will need to sync with the
;;; class definition over there.

(defgeneric emit (object stream)
  (:documentation
   "A method used to write OBJECT to STREAM in a way that the main program can read.")
  (:method ((object glyph) (stream stream))
    (with-accessors ((name glyph-name)
		     (code glyph-code)
		     (description glyph-description)
		     (alternates glyph-alternates))
	object
      (fresh-line stream)
      (write `(:name ,name
	       :code ,code
	       :description ,description
	       :alternates ,(glyph-list-to-codes alternates))
	     :stream stream)))
  (:method ((object classes) (stream stream))
    (with-accessors ((name classes-name)
		     (glyphs classes-glyphs))
	object
      (fresh-line stream)
      (write `(:name ,name
	       :glyphs ,(glyph-list-to-codes glyphs))
	     :stream stream)))
  (:method ((object ranges) (stream stream))
    (with-accessors ((name ranges-name)
		     (description ranges-description)
		     (end ranges-end)
		     (start ranges-start)
		     (glyphs ranges-glyphs))
	object
      (fresh-line stream)
      (write `(:name ,name
	       :description ,description
	       :end end
	       :start start
	       :glyphs ,(glyph-list-to-codes glyphs))
	     :stream stream))))

(defun generate-glyphnames (&key (pathname *smufl-metadata-directory-pathname*)
			   (filename +smufl-metadata-glyphnames-filename+)
			   (outfile +cloveleaf-glyphnames-filename+)
			   (outdir *cloveleaf-source-directory-pathname*))
  "Does the actual work of parsing the JSON file into glyphs and writting them to file."
  (let ((output-file  (merge-pathnames outfile outdir)))
    (with-parser (parser (merge-pathnames filename pathname))
      (parse-next parser)		    ; pop off the first :begin-object.
      (with-open-file (file output-file
			    :direction :output
			    :if-does-not-exist :create)
	(print-header file)
	(loop for glyph = (parse-glyph parser)
	      while glyph
	      do (emit glyph file))
	(fresh-line file)
	output-file))))

(defun glyph-list-to-codes (list)
  "Converts a LIST of GLYPHs to a list of codes.  If already a code list, return it."
  (assert (listp list))
  (assert (or (every #'glyphp list)
	      (every #'numberp list)))
  (if (every #'numberp list)
      list
      (mapcar #'(lambda (g)(glyph-code g)) list)))

(defun print-header (stream)
  "Prints a header for auto generated files."
  (format stream "~%;;; Autogenerated by cloveleaf based upon SMuFL spec files.~%"))

(defun parse-glyph (parser)
  "Parses the JSON for an individual glyph and returns either a glyph object or NIL."
  (assert (typep parser 'parser))
  (multiple-value-bind (key glyphname)
      (parse-next parser)		; we are done parsing glyphs.
    (unless (or (eq :end-object key) 
		(null key)
		(null glyphname))
      (unless (and (eq key :object-key)
		   (stringp glyphname))
	(error "Parsing glyph name: ~S and ~S" key glyphname))
      (let ((glyph (make-instance 'glyph :name glyphname)))
	(parse-glyph-body parser glyph)
	glyph))))

(defun parse-glyph-body (parser glyph)
  "Parses the body of a GLYPH from JSON"
  (assert (typep parser 'parser))
  (assert (typep glyph 'glyph))
  (unless (eq :begin-object (parse-next parser))
    (error "Failed to find start of glyph object."))
  (with-accessors ((code glyph-code)
		   (description glyph-description)
		   (alternates glyph-alternates))
      glyph
    (flet ((object-keyp (key value keystring)
	     (and (eq :object-key key)
		  (stringp value)
		  (string= value keystring)))
	   (string-valuep (key value)
	     (and (eq :value key) (stringp value))))
      (loop for (key value) = (multiple-value-list (parse-next parser))
	    while (not (eq :end-object key))
	    do (cond ((object-keyp key value "codepoint")
		      (multiple-value-bind (key2 value2)
			  (parse-next parser)
			(when (string-valuep key2 value2)
			  (setf code (codepoint-code value2)))))
		     ((object-keyp key value "description")
		      (multiple-value-bind (key2 value2)
			  (parse-next parser)
			(when (string-valuep key2 value2)
			  (setf description value2))))
		     ((object-keyp key value "alternateCodepoint")
		      (multiple-value-bind (key2 value2)
			  (parse-next parser)
			(when (string-valuep key2 value2)
			  (setf alternates (list (codepoint-code value2))))))
		     (t (error "Unknown key '~S' and value '~S' in parsing glyph body."
			       key
			       value)))))))

(defun codepoint-code (code-point)
  "Converts hexidecimal string codepoints into integers."
  (assert (stringp code-point))
  (parse-integer (string-left-trim "U+" code-point) :radix 16))
