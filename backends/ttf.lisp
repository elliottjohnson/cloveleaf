(uiop:define-package cloveleaf/ttf
  (:use #:cl #:cloveleaf #:zpb-ttf)
  (:export #:ttf-font))
(in-package :cloveleaf/ttf)

(defclass ttf-font (metadata-font)
  ((collection-index :accessor ttf-font-collection-index
		     :documentation "The TTF font collection index number.  Defaults to 0."
		     :initarg :collection-index
		     :type integer
		     :initform 0)
   (italic-angle :accessor ttf-font-italic-angle
		 :documentation "The typographic italic angle."
		 :initarg :italic-angle
		 :type number
		 :initform 0)
   (underline-thickness :accessor ttf-font-underline-thickness
			:documentation "The typographic underline thickness."
			:initarg :underline-thickness
			:type number)
   (underline-position :accessor ttf-font-underline-position
		       :documentation "The typographic underline position."
		       :initarg :underline-position
		       :type number)
   (fixed-pitch-p :accessor ttf-font-fixed-pitch-p
		  :documentation "A boolean to indicate if the font is a fixed width."
		  :initarg :fixed-pitch-p
		  :type boolean)
   (units/em :accessor ttf-font-units/em
	     :documentation "The number of units in the typographic em-square."
	     :initarg :units/em
	     :type number)
   (ascender :accessor ttf-font-ascender
	     :documentation "The typographic ascender value."
	     :initarg :ascender
	     :type number)
   (descender :accessor ttf-font-descender
	      :documentation "The typographic descender value."
	      :initarg :descender
	      :type number)
   (line-gap :accessor ttf-font-line-gap
	     :documentation "The typographic line gap."
	     :initarg :line-gap
	     :type number)
   (postscript-name :accessor ttf-font-postscript-name
		    :documentation "The postscript name of the font."
		    :initarg :postscript-name
		    :type string)
   (full-name :accessor ttf-font-full-name
	      :documentation "The full name of the font."
	      :initarg :full-name
	      :type string)
   (family-name :accessor ttf-font-family-name
		:documentation "The family name of the font."
		:initarg :family-name
		:type string)
   (subfamily-name :accessor ttf-font-subfamily-name
		   :documentation "The sub-family name of the font."
		   :initarg :subfamily-name
		   :type string)
   (kerning-pairs :accessor ttf-font-kerning-pairs
		  :documentation "A list of all the kerning pairs available.  A list of left and right glyphs with kerning offset."
		  :initarg :kerning-pairs
		  :type list)
   (bounding-box :accessor ttf-font-bounding-box
		 :documentation "Horizontal and vertical extremes for the ttf font."
		 :initarg :bounding-box
		 :type (simple-vector 4)))
  (:documentation "A class for all SMuFL fonts using True Type Font formats."))

(defmethod cloveleaf::parse-font ((font ttf-font) &key clear &allow-other-keys)
  (declare (ignore clear))
  (call-next-method)
  (with-font-loader (font-loader
		     (font-pathname font)
		     :collection-index (ttf-font-collection-index font))
    (setf (ttf-font-italic-angle font) (italic-angle font-loader)
	  (ttf-font-underline-thickness font) (underline-thickness font-loader)
	  (ttf-font-underline-position font) (underline-position font-loader)
	  (ttf-font-fixed-pitch-p font) (fixed-pitch-p font-loader)
	  (ttf-font-units/em font) (units/em font-loader)
	  (ttf-font-ascender font) (ascender font-loader)
	  (ttf-font-descender font) (descender font-loader)
	  (ttf-font-line-gap font) (line-gap font-loader)
	  (ttf-font-postscript-name font) (postscript-name font-loader)
	  (ttf-font-full-name font) (full-name font-loader)
	  (ttf-font-subfamily-name font) (subfamily-name font-loader)
	  (ttf-font-kerning-pairs font) (all-kerning-pairs font-loader)
	  (ttf-font-bounding-box font) (bounding-box font-loader))))

(defclass ttf-glyph (glyph)
  ((glyph :accessor ttf-glyph-glyph
	  :documentation "The ZPB-TTF::GLYPH object parsed from the TTF file."
	  :initarg :glyph
	  :type zpb-ttf::glyph))
  (:documentation "A class for all TTF font SMuFL glyphs."))

(defmethod load-glyphs ((font ttf-font) &key data &allow-other-keys)
  (with-font-loader (font-loader
		     (font-pathname font)
		     :collection-index (ttf-font-collection-index font))
    (loop for i from 0 to (1- (glyph-count font-loader))
	  do (let ((zpb-glyph (index-glyph i font-loader)))
	       (bounding-box zpb-glyph)
	       (contours zpb-glyph)
	       (add-font-glyph font
			       data
			       (make-instance 'ttf-glyph
					      :glyph zpb-glyph
					      :name (postscript-name zpb-glyph)
					      :character (code-char
							  (code-point zpb-glyph))))))))
