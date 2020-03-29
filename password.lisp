
(in-package :xbps)

(clim:define-presentation-type password ()
  :inherit-from '((string)
		  :description "Password")
  :description "password")

(define-presentation-method present (password (type password) stream
						   (view textual-view)
						   &key acceptably)
  (when acceptably (error "Not acceptably"))
  (clim-lisp:write-string (make-string (length password) :initial-element #\*) stream))

(define-presentation-method accept ((type password) stream view ;; (view textual-view)
						    &key)
  (let* ((s (stream-scan-pointer stream))
	 (p (with-output-recording-options (stream :draw nil :record nil)
	      (read-token stream))))
    (presentation-replace-input stream p 'password view
				:buffer-start s)
    (return-from accept p)))
