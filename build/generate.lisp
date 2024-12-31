(uiop:define-package cloveleaf/smufl
  (:use #:cl #:com.inuoe.jzon))
(in-package #:cloveleaf/smufl)

(defvar *smufl-metadata-directory-pathname*
  nil
  "A default pathname to use for the SMuFL metadata.")

(defparameter +smufl-metadata-glyphnames-filename+
  "glyphnames.json"
  "The filename of the SMuFL glyphnames.json file to read in.")
(defparameter +smufl-metadata-classes-filename+
  "classes.json"
  "The filename of the SMuFL classes.json file to read in.")
(defparameter +smufl-metadata-classes-ranges.json+
  "ranges.json"
  "The filename of the SMuFL ranges.json file to read in.")
