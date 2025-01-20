(in-package #:cloveleaf)

(defun code-codepoint (code)
  "Converts a character code to a SMuFL code-point string."
  (assert (integerp code))
  (format nil "U+~4,'0X" code))

(defun codepoint-code (code-point)
  "Converts hexidecimal string codepoints into integers."
  (assert (stringp code-point))
  (parse-integer (string-left-trim "U+" code-point) :radix 16))

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

(defun egethash (key hash-table &optional default)
  "A version of gethash that errors if nothing is associated with the provided key."
  (assert (hash-table-p hash-table))
  (let ((hash-values
	  (multiple-value-list (gethash key hash-table default))))
    (if (second hash-values)
	(apply #'values hash-values)
	(error "Hash table '~S' does not have a key for '~S' " hash-table key))))

