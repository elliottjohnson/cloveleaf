(in-package #:cloveleaf)

(defun code-codepoint (code)
  "Converts a character code to a SMuFL code-point string."
  (assert (integerp code))
  (format nil "U+~4,'0X" code))

(defun get-glyphnames (metadata)
  "Returns the glyphnames hash from SMuFL metadata."
  (assert (hash-table-p metadata))
  (gethash +smufl-metadata-glyphnames-key+ metadata))

(defun get-classes (metadata)
  "Returns the classes hash from SMuFL metadata."
  (assert (hash-table-p metadata))
  (gethash +smufl-metadata-classes-key+ metadata))

(defun get-ranges (metadata)
  "Returns the ranges hash from SMuFL metadata."
  (assert (hash-table-p metadata))
  (gethash +smufl-metadata-ranges-key+ metadata))
