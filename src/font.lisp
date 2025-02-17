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
	    :initarg :classes
	    :initform (make-hash-table :test #'equal))
   (ranges :accessor font-ranges
	   :documentation "Ranges are groups of assocated glyphs by contiguous code point blocks."
	   :initarg :ranges
	   :type list
	   :initform ())
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
			  name-hash
			  engraving-defaults
			  design-size
			  size-range
			  classes
			  ranges
			  sets)
	    do (font-slot-makunbound slot))
      (when total (font-slot-makunbound 'pathname))
      ;; Setup the font to be used again
      (setf (font-glyph-hash font) (make-hash-table :test #'eq)
	    (font-name-hash font) (make-hash-table :test #'equal)
	    (font-classes font) (make-hash-table :test #'equal)
	    (font-ranges font) ())))
  (:method :around ((font metadata-font) &key total)
    (when total (slot-makunbound font 'metadata))
    (call-next-method))
  (:documentation
   "Clears out all data that is parsed in to create the font definition.
If TOTAL is true, then the entire font-object is cleared, including
slots, which maybe required to locate the font definition."))

(defgeneric load-font (font &key clear &allow-other-keys)
  (:method :before ((font font) &key clear)
    (when clear (clear-font font)))
  (:method ((font metadata-font) &key clear &allow-other-keys)
    (declare (ignore clear))
    (assert (pathnamep (font-metadata font)))
    (let ((metadata (parse (font-metadata font))))
      (read-smufl-data :hash metadata) ; pass keys for SMuFL files or use defaults?
      (parse-font font :data metadata)
      (load-glyphs font :data metadata)
      (assign-alternate-glyphs font metadata)
      (assign-ligature-glyphs font metadata)
      (assign-optional-glyphs font metadata)
      (assign-glyph-sets font metadata)
      (set-glyph-advanced-widths font metadata)
      (set-glyph-anchors font metadata)
      (set-alternates font metadata)
      (set-bounding-boxes font metadata)
      (associate-classes font metadata)
      (associate-ranges font metadata)
      font))
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
    ;; Todo, there can be optional font attributes here... in the future capture these?
    ;;   I'm going to wait to see examples of these before I go implementing anything.
    font)
  (:documentation
   "Populates data about the font.  DATA can be provided if the font itself does
not contain the information.  Other keys are allowed for future functionality.
Aside from setting slots in the FONT object, the FONT is returned as a value."))

(defgeneric load-glyphs (font &key data &allow-other-keys)
  (:documentation
   "A backend dependent method for loading glyphs from a font file."))

(defgeneric add-font-glyph (font data glyph &optional error-p)
  (:method ((font font) (data hash-table) (glyph glyph) &optional error-p)
    (copy-smufl-glyph-metadata data glyph)
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
	(let ((char-value (gethash char glyph-hash))
	      (name-value (gethash name name-hash)))
	  (when (or char-value name-value)
	    (funcall (if error-p #'error #'warn)
		     "Multiple glyphs for character '~C' with name '~A' encountered when loading font: '~A'"
		     char
		     name
		     (font-name font)))
	  (format *debug-io*
		  "Adding glyph '~C' '~A' to font." char name)
	  (typecase char-value
	    (null (setf (gethash char glyph-hash) glyph))
	    (atom (setf (gethash char glyph-hash) (list glyph char-value)))
	    (list (push glyph (gethash char glyph-hash))))
	  (typecase name-value
	    (null (setf (gethash name name-hash) glyph))
	    (atom (setf (gethash name name-hash) (list glyph name-value)))
	    (list (push glyph (gethash name name-hash))))))))
  (:documentation
   "A method of adding a glyph to a font.  If ERROR-P is true, errors instead of warnings will be created if multiple glyphs are associated with a single character."))

(defgeneric assign-alternate-glyphs (font data)
  (:method ((font font) (data hash-table))
    (with-accessors ((glyph-hash font-glyph-hash)
		     (name-hash font-name-hash))
	font
      (loop for name being the hash-keys in name-hash
	    using (hash-value glyphs)
	    do (flet ((alts-to-glyph (glyph)
			(with-accessors ((alts glyph-alternates))
			    glyph
			  (when alts
			    (setf alts
				  (mapcar #'(lambda (g) (get-glyph font g)) alts))))))
		 (etypecase glyphs
		   (null)
		   (glyph (alts-to-glyph glyphs))
		   (list (loop for g in glyphs do (alts-to-glyph g))))))))
  (:documentation
   "Loops over all glyphs in a font and maps the alternate characters to their associated glyph objects."))

(defgeneric assign-ligature-glyphs (font metadata)
  (:method ((font metadata-font) (data hash-table))
    (loop for ligature-name being the hash-keys in (gethash "ligatures" data)
	    using (hash-value glyph-data)
	  do (let ((glyph (make-instance 'ligature-glyph
					 :name ligature-name
					 :description (gethash "description"
							       glyph-data))))
	       (with-accessors ((code glyph-code)
				(code-point glyph-code-point)
				(character glyph-character)
				(component-glyphs ligature-glyph-component-glyphs))
		   glyph
		 (setf code-point (gethash "codepoint" glyph-data)
		       code (codepoint-code code-point)
		       character (code-char code)
		       component-glyphs (get-glyph font
						   (gethash "componentGlyphs"
							    glyph-data))))
	       (add-font-glyph font data glyph))))
  (:documentation
   "Locates the ligatures defined by a font, adds new LIGATURE-GLYPHS made up of the defined COMPONENT-GLYPHS."))

(defgeneric get-glyph (font glyph-descriptor)
  (:method ((font font) (glyph-descriptor glyph))
    (declare (ignore font))
    glyph-descriptor)
  (:method ((font font) (glyph-descriptor string))
    (egethash glyph-descriptor (font-name-hash font)))
  (:method ((font font) (glyph-descriptor character))
    (egethash glyph-descriptor (font-glyph-hash font)))
  (:method ((font font) (glyph-descriptor integer))
    (get-glyph font (code-char glyph-descriptor)))
  (:method ((font font) (glyph-descriptor list))
    (mapcar #'(lambda (g) (get-glyph font g)) glyph-descriptor))
  (:method ((font font) (glyph-descriptor array))
    (loop for g across glyph-descriptor collect (get-glyph font g)))
  (:documentation "GET-GLYPH attempts to return a glyph using some sane lookups or errors otherwise."))

(defgeneric assign-optional-glyphs (font data)
  (:method ((font metadata-font) (data hash-table))
    (with-accessors ((classes font-classes)
		     (name-hash font-name-hash))
	font
      (loop for glyph-name being the hash-keys in (gethash "optionalGlyphs" data)
	      using (hash-value glyph-data)
	    do (let ((glyph
		       (get-glyph font
				  (codepoint-code (gethash "codepoint" glyph-data)))))
		 (flet ((update-glyph-name (g)
			  (with-accessors ((name glyph-name)
					   (description glyph-description))
			      g
			    (warn "Updating glyph named '~A' to alternate name '~A'"
				  name
				  glyph-name)
			    ;; TODO revisit remhash of the old name.
			    ;; for now we just change the glyph-name & hash a new one.
			    (setf name glyph-name
				  (gethash name name-hash) glyph)
			    (let ((alt-description (gethash "description" glyph-data)))
			      (when alt-description
				(setf description alt-description))))))
		   (typecase glyph
		     (glyph (update-glyph-name glyph))
		     (list (mapcar #'update-glyph-name glyph))
		     (t (error "Failed to find a optional glyph definition for ~A with codepoint: ~A"
			       glyph-name
			       (gethash "codepoint" glyph-data)))))
		 

		 ;; Stash a list of glyphs in the font-classes table.
		 ;;  later we'll go over this table and instantiate applicable classes.
		 (macrolet ((glyph-classes () '(gethash "classes" glyph-data)))
		   (when (glyph-classes)
		     (loop for class across (glyph-classes)
			   do (pushnew glyph (gethash class classes)))))))))
  (:documentation
   "Optional Glyphs are ones outside of the typical SMuFL defined glyphs and are defined on a per font basis.  This function also stashes information about class membership for these optional glyphs."))

(defgeneric assign-glyph-sets (font metadata)
  (:method ((font metadata-font) (data hash-table))
    (unless (slot-boundp font 'sets)
      (setf (font-sets font) nil))
    (loop for set-name being the hash-keys in (gethash "sets" data)
	    using (hash-value set-data)
	  do (pushnew (make-set font data set-name set-data)
		      (font-sets font))))
  (:documentation
   "ASSIGN-GLYPH-SETS loops over the font metadata \"sets\" and updates the widths."))


(defgeneric set-glyph-advanced-widths (font metadata)
  (:method ((font metadata-font) (data hash-table))
    (let ((widths (gethash "glyphAdvanceWidths" data)))
      (if (hash-table-p widths)
	  (loop for glyph-name being the hash-keys in widths
		  using (hash-value width)
		for glyph = (get-glyph font glyph-name)
		do (flet ((set-aw (g w) (setf (glyph-advanced-width g) w)))
		     (typecase glyph
		       (list (loop for g in glyph do (set-aw g width)))
		       (glyph (set-aw glyph width))
		       (t (warn "In trying to set advanced-widths, cannot locate a glyph for: '~A'" glyph-name)))))
	  (warn "No advanced glyph widths are found for font: ~A" (font-name font)))))
  (:documentation
   "Iterates over the font's advanced width declarations and sets the related 
glyph's slot values.  Currently these are being read as double-floats.  
Might be low hanging fruit to optimize."))

(defgeneric set-glyph-anchors (font metadata)
  (:method ((font metadata-font) (data hash-table))
    (let ((anchors (gethash "glyphsWithAnchors" data)))
      (if (hash-table-p anchors)
	  (loop for glyph-name being the hash-keys in anchors
		  using (hash-value anchor-data)
		for glyph = (get-glyph font glyph-name)
		do (labels
		       ((get-coord (key)
			  (let ((coord (gethash key anchor-data)))
			    (when coord
			      (list (aref coord 0) (aref coord 1)))))
			(anchors-away (g)
			  (let ((anchor (when (slot-boundp g 'anchor)
					  (glyph-anchor g))))
			    (unless anchor (setf anchor (make-instance 'anchor)
						 (glyph-anchor g) anchor))
			    (with-accessors ((split-stem anchor-split-stem)
					     (stem anchor-stem)
					     (nominal-width anchor-nominal-width)
					     (numeral anchor-numeral)
					     (cut-out anchor-cut-out)
					     (grace-note-slash anchor-grace-note-slash)
					     (repeat-offset anchor-repeat-offset)
					     (notehead-origin anchor-notehead-origin)
					     (optical-center anchor-optical-center))
				anchor
			      (setf split-stem (make-position-alist
						:se (get-coord "splitStemUpSE")
						:sw (get-coord "splitStemUpSW")
						:ne (get-coord "splitStemDownNE")
						:nw (get-coord "splitStemDownNW"))
				    stem (make-position-alist
					  :se (get-coord "stemUpSE")
					  :sw (get-coord "stemUpSW")
					  :ne (get-coord "stemDownNE")
					  :nw (get-coord "stemDownNW"))
				    nominal-width (get-coord "nominalWidth")
				    numeral (list (get-coord "numeralTop")
						  (get-coord "numeralButtom"))
				    cut-out (make-position-alist
					     :se (get-coord "cutOutSE")
					     :sw (get-coord "cutOutSW")
					     :ne (get-coord "cutOutNE")
					     :nw (get-coord "cutOutNW"))
				    grace-note-slash (make-position-alist
						      :se (get-coord "graceNoteSlashSE")
						      :sw (get-coord "graceNoteSlashSW")
						      :ne (get-coord "graceNoteSlashNE")
						      :nw (get-coord "graceNoteSlashNW"))
				    repeat-offset (get-coord "repeatOffset")
				    notehead-origin (get-coord "noteheadOrigin")
				    optical-center (get-coord "opticalCenter"))))))
		     (typecase glyph
		       (list (loop for g in glyph do (anchors-away g)))
		       (glyph (anchors-away glyph))
		       (t (warn "In trying to set glyph-anchor, Cannot locate a glyph for ~A" glyph-name)))))
	  (warn "No glyphs with anchors were found for font: ~A" (font-name font)))))
  (:documentation "Iterates over the defined anchors and populates the anchor values for their related glyphs."))

(defgeneric set-alternates (font data)
  (:method ((font metadata-font) (data hash-table))
    (let ((alternates (gethash "glyphsWithAlternates" data)))
      (if alternates
	  (loop for glyph-name being the hash-keys in alternates
		  using (hash-value alternate-data)
		for glyph = (get-glyph font glyph-name)
		do (let ((alts (gethash "alternates" alternate-data)))
		     (loop for alt across alts
			   for alt-name = (gethash "name" alt)
			   for alt-glyph = (get-glyph font alt-name)
;			   for alt-codepoint = (gethash "codepoint" alt)
			   do (when (and glyph alt-glyph)
				(pushnew alt-glyph (glyph-alternates glyph))
				(pushnew glyph (glyph-alternate-for alt-glyph))))))
	  (warn "No alternates for font: ~A" (font-name font)))))
  (:documentation "Loops over the definitions of alternate glyphs and associates them."))

(defgeneric set-bounding-boxes (font data)
  (:method ((font metadata-font) (data hash-table))
    (let ((bboxes (gethash "glyphBBoxes" data)))
      (loop for glyph-name being the hash-keys in bboxes
	      using (hash-value bbox-data)
	    for glyph = (get-glyph font glyph-name)
	    do (labels ((get-coord (key) ; code reuse here! factor me out.
			  (let ((coord (gethash key bbox-data)))
			    (when coord
			      (list (aref coord 0) (aref coord 1)))))
			(set-box (g)
			  (when (and (slot-boundp g 'bounding-boxes)
				     (glyph-bounding-boxes g))
			    (warn "Overwritting bounding box for: ~A" glyph-name))
			  (setf (glyph-bounding-boxes g)
				(make-position-alist :se (get-coord "bBoxSE")
						     :sw (get-coord "bBoxSW")
						     :ne (get-coord "bBoxNE")
						     :nw (get-coord "bBoxNW")))))
		 (typecase glyph
		   (list (loop for g in glyph do (set-box g)))
		   (glyph (set-box glyph))
		   (t (warn "Cannot locate glyph: ~A to set defined bounding box."
			    glyph-name)))))))
  (:documentation
   "Reads the bounding box metadata and associates it with the font glyphs."))

(defgeneric associate-classes (font data)
  (:method (font data)
    (declare (ignore font data))
    t)
  (:documentation
   "Initially I spec'd out this function to handle the association of classes
with glyphs, but currently the optionalGlyphs hold this association, so
we capture it there.  I've left this method just in case there is some
future need to do the association independently of the optional glyphs."))

(defgeneric associate-ranges (font data)
  (:method ((font metadata-font) (data hash-table))
    (let ((ranges (get-ranges data)))
      (if (and ranges (hash-table-p ranges))
	  (loop for range being the hash-value in ranges
		do (pushnew range (font-ranges font)))
	  (warn "No ranges specified for font: ~A" (font-name font)))))
  (:documentation "Associates the SMuFL defined ranges with the font."))
