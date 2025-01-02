(in-package #:cloveleaf)

(defun code-codepoint (code)
  "Converts a character code to a SMuFL code-point string."
  (assert (integerp code))
  (format nil "U+~4,'0X" code))
