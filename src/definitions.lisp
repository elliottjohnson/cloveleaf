(in-package :cloveleaf)

(defvar *smufl-specification-version* "1.40"
  "A version string to identify the SMuFL specification version this library is compatible with.")

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

(defparameter +smufl-metadata-glyphnames-key+
  (symbol-name (gensym "glyphnames"))
  "A unique string to use as a key in referencing SMuFL glyph metadata.")

(defparameter +smufl-metadata-classes-key+
  (symbol-name (gensym "classes"))
  "A unique string to be used as a key in referencing SMuFL classes metadata.")

(defparameter +smufl-metadata-ranges-key+
  (symbol-name (gensym "ranges"))
  "A unique string to be used as a key in referenceing SMuFL classes metadata.")
