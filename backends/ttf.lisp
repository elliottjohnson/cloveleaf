(uiop:define-package cloveleaf/ttf
  (:use #:cl #:cloveleaf #:zpb-ttf)
  (:export #:ttf-font))
(in-package :cloveleaf/ttf)

(defclass ttf-font (metadata-font)
  ()
  (:documentation "A class for all SMuFL fonts using True Type Font formats."))

(defclass ttf-glyph (glyph)
  ((glyph :accessor ttf-glyph-glyphS
	  :documentation "The ZPB-TTF::GLYPH object parsed from the TTF file."
	  :initarg :glyph
	  :type zpb-ttf::glyph))
  (:documentation "A class for all TTF font SMuFL glyphs."))

(defmethod load-glyphs ((font ttf-font) &key data &allow-other-keys)
  (with-font-loader (font-loader (font-pathname font))
    (loop for i from 0 to (1- (glyph-count font-loader))
	  for zpb-glyph = (index-glyph i font-loader)
	  for zpb-code = (code-point zpb-glyph)
	  do (add-font-glyph font
			     data
			     (make-instance 'ttf-glyph
					    :glyph zpb-glyph
					    :name (postscript-name zpb-glyph)
					    :character (code-char zpb-code))))))
