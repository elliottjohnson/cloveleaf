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
