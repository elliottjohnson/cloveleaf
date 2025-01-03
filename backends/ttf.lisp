(uiop:define-package cloveleaf/ttf
  (:use #:cl #:cloveleaf #:zpb-ttf)
  (:export #:ttf-font))
(in-package :cloveleaf/ttf)

(defclass ttf-font (metadata-font)
  ()
  (:documentation "A class for all SMuFL fonts using True Type Font formats."))

(defclass ttf-glyph (glyph)
  ((glyph :accessor ttf-glyph-glyph
	  :documentation "The ZPB-TTF::GLYPH object parsed from the TTF file."
	  :initarg :glyph
	  :type zpb-ttf::glyph))
  (:documentation "A class for all TTF font SMuFL glyphs."))

(defmethod load-glyphs ((font ttf-font) &key data &allow-other-keys)
  (declare (ignore data))
  (with-font-loader (font-loader (font-pathname font))
    (loop for i from 0 to (1- (glyph-count font-loader))
	  with zpb-glyph = (index-glyph i font-loader)
	  with zpb-character = (code-char (code-point zpb-glyph))
	  do (let ((ttf-glyph (make-instance 'ttf-glyph
					     :glyph zpb-glyph
					     :character zpb-character)))
	       ))))
