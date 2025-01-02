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
   (anchors :accessor glyph-anchors
	    :documentation "A list of up to 22 attributes wand values forthe given glyph."
	    :initarg :glyph-anchors
	    :type list)
   (bounding-boxes :accessor glyph-bounding-boxes
		   :documentation "Defines the bounding box of the glyph or smallest rectaingle test encloses very part of the path."
		   :initarg :bounding-boxes))
  (:documentation "A class to represent the SMuFL spec's defined glyphs."))

(defclass ligature-glyph (glyph)
  ((component-glyphs :accessor glyph-ligature-component-glyths
		     :documentation "A list of glyphs that make up a ligature for this graph."
		     :initarg :ligature))
  (:documentation "A class for all ligature glyphs."))

(defun glyphp (thing &optional environment)
  "Returns true if THING is a glyph."
  (typep thing 'glyph environment))



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
