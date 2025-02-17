(in-package #:cloveleaf)


;;;
;;; Glyph classes
;;;

(defclass glyph ()
  ((name :accessor glyph-name
	 :documentation "A namestring representing the glyph name."
	 :initarg :name
	 :type string)
   (code-point :accessor glyph-code-point
	       :documentation "A string representing the unicode codepoint."
	       :initarg :code-point
	       :type string)
   (code :accessor glyph-code
	 :documentation "A code is a number used to represent this glyph."
	 :initarg :code
	 :type number)
   (character :accessor glyph-character
	      :documentation "A unicode character to represent this glyph."
	      :initarg :character
	      :type character)
   (description :accessor glyph-description
		:documentation "A string descriptions of the character."
		:initarg :description
		:type string)
   (alternates :accessor glyph-alternates
	       :documentation "A list of alternates for this character."
	       :initarg :alternates
	       :initform ()
	       :type list)
   (alternate-for :accessor glyph-alternate-for
		  :documentation "This glyph is an alternate for other glyphs."
		  :initarg :alternate-for
		  :initform ()
		  :type list)
   (advanced-width :accessor glyph-advanced-width
		   :documentation "Defines a glyph width from the origin of the glyph in the next glyph."
		   :initarg :advanced-with
		   :type number)
   (anchor :accessor glyph-anchor
	   :documentation "A list of up to 22 attributes wand values forthe given glyph."
	   :initarg :glyph-anchor
	   :type list)
   (bounding-boxes :accessor glyph-bounding-boxes
		   :documentation "Defines the bounding box of the glyph or smallest rectaingle test encloses very part of the path."
		   :initarg :bounding-boxes))
  (:documentation "A class to represent the SMuFL spec's defined glyphs."))

(defclass ligature-glyph (glyph)
  ((component-glyphs :accessor ligature-glyph-component-glyphs
		     :documentation "A list of glyphs that make up a ligature for this graph."
		     :initarg :component-glyphs))
  (:documentation "A class for all ligature glyphs."))

(defun glyphp (thing &optional environment)
  "Returns true if THING is a glyph."
  (typep thing 'glyph environment))

(defmethod print-object ((object glyph) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (glyph-code-point object) (glyph-name object))))

;;;
;;; Code to read in SMuFL defined glyphnames.
;;;

(defun read-glyphnames (&key
			  (filename (merge-pathnames
				     +cloveleaf-glyphnames-filename+
				     *cloveleaf-source-directory-pathname*))
			  (hash (make-hash-table :test 'eq)))
  "Returns a populated HASH of glyphs based upon the SMuFL defined 
glyph names in FILENAME."
  (with-open-file (file filename)
    (loop for glyph-def = (read file nil nil)
	  while glyph-def
	  do (let* ((code (getf glyph-def :code))
		    (character (code-char code)))
	       (setf (gethash character hash)
		     (make-instance 'glyph
				    :name (getf glyph-def :name)
				    :character character
				    :code code
				    :description (getf glyph-def :description)
				    :alternates (getf glyph-def :alternates)
				    :code-point (code-codepoint code))))))
  hash)

(defgeneric copy-smufl-glyph-metadata (data glyph)
  (:method ((data hash-table) (glyph glyph))
    (with-accessors ((character glyph-character)
		     (name glyph-name)
		     (code glyph-code)
		     (code-point glyph-code-point)
		     (description glyph-description)
		     (alternates glyph-alternates))
	glyph
      (unless (slot-boundp glyph 'code)
	(setf code (char-code character)))
      (unless (slot-boundp glyph 'code-point)
	(setf code-point (code-codepoint code)))
      (let ((smufl-glyph (gethash character (get-glyphnames data))))
	;; First we need to make sure we have a matching glyph.
	(if smufl-glyph
	    ;; If we have a match, we can copy over the SMuFL assigned values.
	    (with-accessors ((smufl-name glyph-name)
			     (smufl-code glyph-code)
			     (smufl-code-point glyph-code-point)
			     (smufl-description glyph-description)
			     (smufl-alternates glyph-alternates))
		smufl-glyph
	      (unless (slot-boundp glyph 'name)
		(setf name smufl-name))
	      (unless (string= name smufl-name)
		(warn "The glyph name for character '~C' is '~A', which differs from the SMuFL definition '~A'.  Using the SMuFL name."
		      character
		      name
		      smufl-name))
	      (setf name smufl-name
		    code smufl-code
		    code-point smufl-code-point
		    description smufl-description
		    alternates smufl-alternates)
	      (format *debug-io*
		      "~&Added glyph '~C' named '~A' with description '~A'."
		      character
		      name
		      description))
	    (warn "Unable to associate '~C' with a SMuFL defined glyph, moving on."
		  character)))))
  (:documentation
   "A generic function to copy over some basic information from a SMuFL glyph definition to a newly created font glyph."))

