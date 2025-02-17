(in-package :cloveleaf)

(defvar *position-list* '(:se :sw :ne :nw)
  "A list of positions to represent areas for an anchor.")

(defun make-position-alist (&key se sw ne nw)
  "Returns an initial alist for anchor positions."
  (pairlis *position-list* (list se sw ne nw)))

(defun empty-position-p (alist)
  "Returns true if the position ALIST is empty."
  (every #'(lambda (x) (null (cdr x))) alist))

(defun set-position (alist pos value)
  (setf (cdr (assoc pos alist)) value))

(defclass anchor ()
  ((split-stem :accessor anchor-split-stem
	       :initarg :split-stem
	       :documentation
	       "A collection of positions (SE SW NE NW) for split-stem anchors."
	       :type list
	       :initform (make-position-alist))
   (stem :accessor anchor-stem
	 :initarg :stem
	 :documentation "A collection of positions (SE SW NE NW) for stem anchors."
	 :type list
	 :initform (make-position-alist))
   (nominal-width :accessor anchor-nominal-width
		  :documentation
		  "The width in staff spaces.  Useful for spacing bar lines."
		  :type list
		  :initform ())
   (numeral :accessor anchor-numeral
	    :initarg :numeral
	    :documentation
	    "A collection of positions (TOP BOTTOM) for numeral placement."
	    :type list
	    :initform ())
   (cut-out :accessor anchor-cut-out
	    :initarg :cut-out
	    :documentation
	    "A box (SE SW NE NW) that intersects the glyph bounding box.  Useful for a detailed view of the glyph to allow for kerning and complex interlocking symbols."
	    :type list
	    :initform (make-position-alist))
   (grace-note-slash :accessor anchor-grace-note-slash
		     :initarg :grace-note-slash
		     :documentation
		     "A set of coordinates (SE SW NE NW) for positioning relative to 
the stem of a unbeamed grace note."
		     :type list
		     :initform (make-position-alist))
   (repeat-offset :accessor anchor-repeat-offset
		  :initarg :repeat-offset
		  :documentation
		  "The horizontal (X Y) position at which identical or similarly grouped glyphs repeat or tessellate."
		  :type list
		  :initform ())
   (notehead-origin :accessor anchor-notehead-origin
		    :initarg :notehead-origin
		    :documentation
		    "The left-hand (X Y) edge of a notehead for horizontal alignment."
		    :type list
		    :initform ())
   (optical-center :accessor anchor-optical-center
		   :initarg :optical-center
		   :documentation
		   "The optical center for horizontal alignment relative to notehead and stem.  Currently only recommended for DYNAMICS range glyphs."
		   :type list
		   :initform ()))
  (:documentation "An anchor object defines aspects of a glyphs position."))
