(uiop:define-package cloveleaf/ttf
  (:use #:cl #:cloveleaf #:zpb-ttf)
  (:export #:ttf-font))
(in-package :cloveleaf/ttf)

(defclass ttf-font (metadata-font)
  ()
  (:documentation "A class for all SMuFL fonts using True Type Font formats."))

(defmethod load-glyphs ((font ttf-font) &key data &allow-other-keys)
  (declare (ignore data))
  (with-font-loader (font-loader (font-pathname font))
    (loop for i from 0 to (1- (glyph-count font-loader))
	  with ttf-glyph = (index-glyph i font-loader)
	  do (destructuring-bind (sym &rest glyphdata)
		 glyph
	       (let ((codepoint (cdr (assoc :codepoint glyphdata))))
		 )))))
