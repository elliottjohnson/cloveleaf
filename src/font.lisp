(in-package :cloveleaf)

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
