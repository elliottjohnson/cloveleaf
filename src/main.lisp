(uiop:define-package cloveleaf
  (:use #:cl #:com.inuoe.jzon))
(in-package #:cloveleaf)

(defvar *smufl-specification-version* "1.40"
  "A version string to identify the SMuFL specification version this library is compatible with.")

;;;
;;; Font clases
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
   
   (alternates :accessor glyph-altenates
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
   (bounding-boxes :accessor glyph-bounding-box
		   :documentation "Defines the bounding box of the glyph or smallest rectaingle test encloses very part of the path."
		   :initarg :bounding-boxes))
    (:documentation "A class to represent the SMuFL spec's defined glyphs."))

(defclass ligature-glyph (glyph)
  ((component-glyphs :accessor glyph-ligature-glyph-componts-flyths
		     :documentation "A list of glyphs that make up a ligature for this graph."
		     :initarg :ligature))
  (:documentation "A class for all ligature classes"))
