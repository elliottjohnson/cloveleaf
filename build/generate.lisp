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
   "A method used to write OBJECT in a readable way to STREAM.")
  (:method ((object glyph) (stream stream))
    (with-accessors ((name glyph-name)
		     (code glyph-code)
		     (description glyph-description)
		     (alternates glyph-alternates))
	object
      (write `(:name ,name
	       :code ,code
	       :description ,description
	       :alternates ',(glyph-list-to-codes alternates))
	     :stream stream)))
  (:method ((object classes) (stream stream))
    (with-accessors ((name classes-name)
		     (glyphs classes-glyphs))
	object
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
      (write `(:name ,name
	       :description ,description
	       :end end
	       :start start
	       :glyphs ',(glyph-list-to-codes glyphs))
	     :stream stream))))

(defun glyph-list-to-codes (list)
  (assert (listp list))
  (assert (every #'glyphp list))
  (mapcar #'(lambda (g)(glyph-code g)) list))

(defun parse-glyphnames (&key (pathname *smufl-metadata-directory-pathname*)
			   (filename +smufl-metadata-glyphnames-filename+)
			   (outfile +cloveleaf-glyphnames-filename+)
			   (outdir *cloveleaf-source-directory-pathname*))
  (with-parser (parser (merge-pathnames filename pathname))
    (parse-next parser)		    ; pop off the first :begin-object.
    (with-open-file (file (merge-pathnames outfile outdir) :direction :output :if-does-not-exist :create)
      (loop with glyph = (parse-glyph parser)
	    while glyph
	    do (emit glyph file)))))

(defun parse-glyph (parser)
  (assert (typep parser 'parser))
  (multiple-value-bind (key glyphname)
      (parse-next parser)		; we are done parsing glyphs.
    (unless (or (eq :end-object key) 
		(null key)
		(null glyphname))
      (unless (and (eq key :object-key)
		   (stringp glyphname))
	(error "Parsing glyph name: ~S and ~S" key glyphname))
      (multiple-value-bind (code description alternates)
	  (parse-glyph-body parser)
	(make-instance 'glyph
		       :name glyphname
		       :code code
		       :description description
		       :alternates alternates)))))

(defun parse-glyph-body (parser)
  (assert (typep parser 'parser))
  (unless (eq :begin-object (parse-next parser))
    (error "Failed to find start of glyph object."))
  (let (code description alternates)
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
			     value))))
    (values code description alternates)))

(defun object-keyp (key value keystring)
  "Returns true if key equals :OBJECT-KEY and if VALUE is STRING= to KEYSTRING."
  (and (eq :object-key key)
       (stringp value)
       (string= value keystring)))

(defun string-valuep (key value)
  "Returns true if KEY equals :VALUE and VALUE is a string."
  (and (eq :value key) (stringp value)))

(defun codepoint-code (code-point)
  "Converts hexidecimal string codepoints into integers."
  (assert (stringp code-point))
  (parse-integer (string-left-trim "U+" code-point) :radix 16))


