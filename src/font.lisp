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
   (name-hash :accessor font-name-hash
	      :documentation "A hashtable that holds all of the associated font's glyphs hashed by name."
	      :initarg :name-hash
	      :type hashtable
	      :initform (make-hash-table :test #'equal))
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

(defgeneric clear-font (font &key total)
  (:method ((font font) &key total)
    (flet ((font-slot-makunbound (s)
	     (slot-makunbound font s)))
      (loop for slot in '(name
			  version
			  glyph-hash
			  engraving-defaults
			  design-size
			  size-range
			  classes
			  ranges
			  sets)
	    do (font-slot-makunbound slot))
      (when total (font-slot-makunbound 'pathname))))
  (:method :around ((font metadata-font) &key total)
    (when total (slot-makunbound font 'metadata))
    (call-next-method))
  (:documentation
   "Clears out all data that is parsed in to create the font definition.
If TOTAL is true, then the entire font-object is cleared, including
slots, which maybe required to locate the font definition."))

(defgeneric load-font (font &key clear)
  (:method :before ((font font) &key clear)
    (when clear (clear-font font)))
  (:method ((font metadata-font) &key clear)
    (declare (ignore clear))
    (assert (pathnamep (font-metadata font)))
    (let ((metadata (parse (font-metadata font))))
      (parse-font font :data metadata)
      (load-glyphs font metadata)
      (assign-alternate-glyphs font metadata)
      (assign-ligature-glyphs font metadata)
      (assign-optional-glyphs font metadata)
      (assign-glyph-sets font metadata)
      (set-glyph-advanced-widths font metadata)
      (set-glyph-anchors font metadata)
      (set-alternates font metadata)
      (set-bounding-boxes font metadata)
      (associate-classes font metadata)
      (associate-ranges font metadata)))
  (:documentation
   "A generic function for loading fonts and the various tasks associated."))  

(defgeneric parse-font (font &key data &allow-other-keys)
  (:method ((font metadata-font) &key data &allow-other-keys)
    (assert (hash-table-p data))
    (flet ((datahash (key) (gethash key data)))
      (with-accessors ((name font-name)
		     (version font-version)
		     (engraving-defaults font-engraving-defaults)
		     (design-size font-design-size)
		     (size-range font-size-range))
	  font 
	(setf name (datahash "fontName")
	      version (datahash "fontVersion")
	      engraving-defaults (datahash "engravingDefaults")
	      design-size (datahash "designSize")
	      size-range (datahash "sizeRange"))))
    ;; Todo, there can be optional key/value pairs here... capture these somehow?
    font)
  (:documentation
   "Populates data about the font.  DATA can be provided if the font itself does
not contain the information.  Other keys are allowed for future functionality.
Aside from setting slots in the FONT object, the FONT is returned as a value."))

(defgeneric load-glyphs (font &key data &allow-other-keys)
  (:documentation
   "A backend dependent method for loading glyphs from a font file."))

(defgeneric add-font-glyph (font glyph &optional error-p)
  (:method ((font font) (glyph glyph) &optional (error-p t))
    (with-accessors ((glyph-hash font-glyph-hash)
		     (name-hash font-name-hash))
	font
      (assert (hash-table-p glyph-hash))
      (assert (hash-table-p name-hash))
      (with-accessors ((char glyph-character)
		       (name glyph-name))
	  glyph
	(assert (characterp char))
	(assert (stringp name))
	(when (and error-p
		   (or (gethash char glyph-hash)
		       (gethash name name-hash)))
	  (error "Glyph: ~A already is hashed for font: ~A"
		 name
		 (font-name font)))
	(setf (gethash char glyph-hash) glyph
	      (gethash name name-hash) glyph))))
  (:documentation "A method of adding a glyph to a font."))
