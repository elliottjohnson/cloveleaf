(in-package :cloveleaf)

;; Sets are a list of stylistic sets of glyphs defined for a font.
;; There are currently 5 types of sets defined with possible future expansion.
;; The name of the set is considered it's "key" with a list of glyphs that are
;; alternates for typically defined glyphs.

;; From what I've seen, we can have alternates already defined, but there is a
;; possibility that new glyphs can be defined here and associated with others.
;; This is prime territory for code reuse from the other areas that link in
;; new glyphs.  Otherwise we are just building a list of already existing glyphs.

(defclass set ()
  ((name :accessor set-name
	 :documentation "The name of a SMuFL set."
	 :initarg :name
	 :type string)
   (description :accessor set-description
		:documentation "A description string for this set."
		:initarg :description
		:type string)
   (type :accessor set-type
	 :documentation "A string representing the type of SMuFL set."
	 :initarg :type
	 :type string)
   (glyphs :accessor set-glyphs
	   :documentation "A list of glyphs associated with the SMuFL set."
	   :initarg :glyphs
	   :type list))
  (:documentation "A set of alternate glyphs to represent a collective purpose.
There are currently 5 SET-TYPEs: opticalVariantsSmall, flagsShort, flagsStraight,
timeSigsLarge, and noteheadsLarge."))

(defgeneric register-set (font set)
  (:method ((font font) (set set))
    (pushnew set (font-sets font)))
  (:documentation "Adds a set to a font."))

(defgeneric make-set (font metadata name data &optional set-class)
  (:method  ((font font)
	     (metadata hash-table)
	     (name string)
	     (data hash-table) &optional (set-class 'set))
    (let ((set (make-instance set-class :name name)))
      (with-accessors ((description set-description)
		       (type set-type)
		       (glyphs set-glyphs))
	  set
	(setf description (gethash "description" data)
	      type (gethash "type" data)
	      glyphs (loop for glyph across (gethash "glyphs" data)
			   for name = (gethash "name" glyph)
			   for desc = (gethash "description" glyph)
			   for code = (codepoint-code (gethash "codepoint" glyph))
			   for alt = (get-glyph font (gethash "alternateFor" glyph))
			   collect (let ((g (get-glyph font name)))
				     (unless g
				       (warn "Couldn't locate glyph '~A', so making a new one." name)
				       (setf g (make-instance 'glyph
							      :name name
							      :description desc
							      :codepoint code))
				       (add-font-glyph font metadata g))
				     (when alt
				       (warn "Adding alt '~A' to glyph '~A'" alt g)
				       (pushnew alt (glyph-alternate-for g))
				       (etypecase (glyph-alternates alt)
					 (null (setf (glyph-alternates alt) (list g)))
					 (list (pushnew g (glyph-alternates alt)))))
				     g))))
      (warn "Made new set '~A' with description '~A'"
	    (set-name set)
	    (set-description set))
      set)))

(defmethod print-object ((object set) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A: ~A" (set-name object) (set-description object))))
