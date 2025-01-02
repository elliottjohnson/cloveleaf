(uiop:define-package cloveleaf
  (:use #:cl #:com.inuoe.jzon)
  (:export #:*smufl-specification-version*
	   #:font
	   #:font-name
	   #:font-version
	   #:font-pathname
	   #:font-glyph-hash
	   #:font-engraving-defaults
	   #:font-design-size
	   #:font-size-range
	   #:font-classes
	   #:font-ranges
	   #:font-sets
	   #:text-font
	   #:metadata-font
	   #:font-metadata
	   #:glyph
	   #:glyph-name
	   #:glyph-code-point
	   #:glyph-code
	   #:glyph-character
	   #:glyph-description
	   #:glyph-alternates
	   #:glyph-alternate-for
	   #:glyph-advanced-width
	   #:glyph-anchors
	   #:glyph-bounding-boxes
	   #:ligature-glyph
	   #:glyph-ligature-component-glyphs
	   #:glyphp
	   #:+cloveleaf-glyphnames-filename+
	   #:+cloveleaf-classes-filename+
	   #:+cloveleaf-ranges-filename+
	   #:*cloveleaf-source-directory-pathname*
	   #:read-glyphnames
	   #:code-codepoint
	   #:classes
	   #:classes-name
	   #:classes-glyphs
	   #:ranges
	   #:ranges-name
	   #:ranges-description
	   #:ranges-end
	   #:ranges-start
	   #:ranges-glyphs))

(in-package #:cloveleaf)

(defvar *smufl-specification-version* "1.40"
  "A version string to identify the SMuFL specification version this library is compatible with.")

;;;
;;; Font classes
;;;

(defclass font ()
  ((name :accessor font-name
	 :documentation "A display name string for this font."
	 :initarg :name
	 :type string)
   (version :accessor font-version
	    :documentation "A version string to identify this version of this font."
	    :initarg :version
	    :type string)
   (pathname :accessor font-pathname
	     :documentation "A pathname to the font definition."
	     :initarg :pathname
	     :type pathname)
   (glyph-hash :accessor font-glyph-hash
	       :documentation "A hashtable that holds all of the assocated font's glyphs by unicode character."
	       :initarg :glyph-hash
	       :type hashtable
	       :initform (make-hash-table :test #'eq))
   (engraving-defaults :accessor font-engraving-defaults
		       :documentation "The engraving defaults for this font."
		       :initarg :engraving-defaults)
   (design-size :accessor font-design-size
		:documentation "A point size for which the font is optimized for, specified in integral decipoints."
		:initarg :design-size
		:type number)
   (size-range :accessor font-size-range
	       :documentation "A cons of two point sizes denoting the smallest and largest point sizes for which the font will serve well."
	       :initarg :size-range
	       :type cons)
   (classes :accessor font-classes
	    :documentation "Classes are named groups of associated glyphs."
	    :initarg :classes)
   (ranges :accessor font-ranges
	   :documentation "Ranges are groups of assocated glyphs by contiguous code point blocks."
	   :initarg :ranges)
   (sets :accessor font-sets
	 :documentation "Sets are groups of alternate glyphs for specialty rendering."
	 :initarg :sets))
  (:documentation "A SMuFL font, which includes glyph data as well as information needed to properly render the font."))

(defclass text-font (font)
  ()
  (:documentation "A class to represent text fonts, which are commonly used as in-text elements rather than in a score program."))

(defclass metadata-font (font)
  ((metadata :accessor font-metadata
	     :documentation "The pathname to a SMuFL font metadata file."
	     :initarg :metadata
	     :type pathname))
  (:documentation "A class for all SMuFL fonts that need an external metadata file to provide information."))


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

(defparameter +cloveleaf-glyphnames-filename+
  "glyphnames.lisp"
  "The filename for the cloveleaf SMuFL glyphname definitions.")
(defparameter +cloveleaf-classes-filename+
  "classes.lisp"
  "The filename for the cloveleaf SMuFL classes definitions.")
(defparameter +cloveleaf-ranges-filename+
  "ranges.lisp"
  "The filename for the cloveleaf SMuFL ranges definitions.")
(defvar *cloveleaf-source-directory-pathname*
  (car (directory
	(asdf:system-relative-pathname (asdf:find-system :cloveleaf) "src")))
  "A directory pathname to install the generated filenames into.")

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

(defun code-codepoint (code)
  "Converts a character code to a SMuFL code-point string."
  (assert (integerp code))
  (format nil "U+~4,'0X" code))

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
		       glyph-hash
		       (hash (make-hash-table :test 'equal)))
  "Returns a populated HASH of classes hashed on CLASSES-NAME."
  (assert (hash-table-p glyph-hash))
  (assert (hash-table-p hash))
  (with-open-file (file filename)
    (loop for classes-def = (read file nil nil)
	  while classes-def
	  do (progn
	       (format t "~%~S" classes-def)
	       (let ((name (getf classes-def :name))
		   (glyphs (getf classes-def :glyphs)))
	       (setf (gethash name hash)
		     (make-instance 'classes
				    :glyphs (mapcar #'(lambda (g)
							(gethash g glyph-hash))
						    glyphs)
				    :name name))))))
  hash)

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
