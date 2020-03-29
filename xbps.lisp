;;;; xbps.lisp

(in-package #:xbps)

(defun make-xbps-graphical-executable ()
  (sb-ext:save-lisp-and-die "xbps"
			    :toplevel (lambda ()
					(app-main))
			    :executable t
			    :purify t))

(let ((zenity-installed
	(handler-case (with-output-to-string (s)
			(uiop:run-program "which zenity" :output s))
	  (t () nil)))
      passwd)
  (defun zenity-password-prompt ()
    (setf passwd
	  (string-trim '(#\newline)
		       (with-output-to-string (stream)
			 (uiop:run-program "zenity --password" :output stream)))))
  (defun prompt-for-password ()
    (let ((stream (frame-standard-input *application-frame*)))
      (accepting-values (stream :own-window nil)
	(formatting-table (stream)
	  (formatting-row (stream)
	    (formatting-column (stream)
	      (formatting-cell (stream)
		(format stream "Enter Password, submit with M-RET"))))
	  (formatting-row (stream)
	    (formatting-column (stream)
	      (formatting-cell (stream)
		(setf passwd
		      (accept 'password
			      :stream stream :prompt nil))))))))
    ;; (if reset
    ;; 	(setf passwd nil)
    ;; 	(let ((stream (frame-standard-input *application-frame*)))
    ;; 	  (unless passwd
    ;; 	    (accepting-values (stream :own-window nil)
    ;; 	      (formatting-table (stream)
    ;; 		(formatting-row (stream)
    ;; 		  (formatting-column (stream)
    ;; 		    (formatting-cell (stream)
    ;; 		      (format stream "Enter Password, submit with M-RET"))))
    ;; 		(formatting-row (stream)
    ;; 		  (formatting-column (stream)
    ;; 		    (formatting-cell (stream)
    ;; 		      (setf passwd
    ;; 			    (accept 'password
    ;; 				    :stream stream :prompt nil))))))))))
    )
  (defun password-prompt (&optional reset)
    (cond (reset
	   (setf passwd nil))
	  (passwd passwd)
	  (t
	   (if zenity-installed
	       (zenity-password-prompt)
	       (prompt-for-password))
	   passwd))))

(defparameter *sudo-timer*
  (sb-ext:make-timer (lambda ()
		       (password-prompt t))))

(defparameter *sudo-password* nil)

(defmacro with-sudo (password &body body)
  `(let ((*sudo-password* ,(or password ;; '(prompt-for-password)
			       '(password-prompt))))
     (prog1 (progn ,@body)
       (setf *sudo-password* nil))))

(define-condition unknown-package-error (error)
  ())

;; (defun xbps (program flags &rest arguments)
;;   (handler-case
;;       (let* ((args (if (and (= 1 (length arguments))
;; 			    (listp (car arguments)))
;; 		       (car arguments)  arguments))
;; 	     (xbps-string (format nil (cond ((and (listp (car args))
;; 						  (listp (caar args)))
;; 					     "xbps-~a ~a~{~{~{~a ~}~}~}")
;; 					    ((listp (car args))
;; 					     "xbps-~a ~a~{~{~a ~}~}")
;; 					    (t "xbps-~a ~a~{~a ~}"))
;; 				  (or (and (symbolp program)
;; 					   (string-downcase
;; 					    (symbol-name program)))
;; 				      (and (stringp program) program))
;; 				  (or (and flags
;; 					   (< 0 (length flags))
;; 					   (concatenate 'string
;; 							"-" flags " "))
;; 				      "")
;; 				  args))
;; 	     (execstr (concatenate 'string
;; 				     (if *sudo-password* "echo \"~a\" | sudo -S " "~a")
;; 				     (cond ((and (listp (car args))
;; 						 (listp (caar args)))
;; 					    "xbps-~a ~a~{~{~{~a ~}~}~}")
;; 					   ((listp (car args))
;; 					    "xbps-~a ~a~{~{~a ~}~}")
;; 					   (t "xbps-~a ~a~{~a ~}")))))
;; 	(with-output-to-string (output)
;; 	  (uiop:run-program (format nil execstr
;; 				    (or *sudo-password* "")
;; 				    (or (and (symbolp program)
;; 					     (string-downcase
;; 					      (symbol-name program)))
;; 					(and (stringp program) program))
;; 				    (or (and flags
;; 					     (< 0 (length flags))
;; 					     (concatenate 'string
;; 							  "-" flags " "))
;; 					"")
;; 				    args)
;; 			    :output output)))
;;     (uiop:subprocess-error (err)
;;       (let ((code (uiop:subprocess-error-code err)
;; 		  ;; (uiop:subprocess-error-)
;; 		  ))
;; 	(cond ((= code 2)
;; 	       (error 'unknown-package-error))
;; 	      (t
;; 	       (notify-user *application-frame* 
;; 			    (format nil "An Error Occured in XBPS:~%~a" ;; err
;; 				    xbps-string)
;; 			    :name "XBPS ERROR")))))))

(defun xbps (program flags &rest arguments)
  (let* ((args (if (and (= 1 (length arguments))
			(listp (car arguments)))
		   (car arguments)  arguments))
	 (xbps-string (format nil (cond ((and (listp (car args))
					      (listp (caar args)))
					 "xbps-~a ~a~{~{~{~a ~}~}~}")
					((listp (car args))
					 "xbps-~a ~a~{~{~a ~}~}")
					(t "xbps-~a ~a~{~a ~}"))
			      (or (and (symbolp program)
				       (string-downcase
					(symbol-name program)))
				  (and (stringp program) program))
			      (or (and flags
				       (< 0 (length flags))
				       (concatenate 'string
						    "-" flags " "))
				  "")
			      args))
	 (execstr (concatenate 'string
			       (if *sudo-password* "echo \"~a\" | sudo -S " "~a")
			       (cond ((and (listp (car args))
					   (listp (caar args)))
				      "xbps-~a ~a~{~{~{~a ~}~}~}")
				     ((listp (car args))
				      "xbps-~a ~a~{~{~a ~}~}")
				     (t "xbps-~a ~a~{~a ~}")))))
    (handler-case
	(with-output-to-string (output)
	  (uiop:run-program (format nil execstr
				    (or *sudo-password* "")
				    (or (and (symbolp program)
					     (string-downcase
					      (symbol-name program)))
					(and (stringp program) program))
				    (or (and flags
					     (< 0 (length flags))
					     (concatenate 'string
							  "-" flags " "))
					"")
				    args)
			    :output output))
      (uiop:subprocess-error (err)
	(let ((code (uiop:subprocess-error-code err)
		    ;; (uiop:subprocess-error-)
		    ))
	  (password-prompt t)
	  (cond ((= code 2)
		 (error 'unknown-package-error))
		((= code 1)
		 (notify-user *application-frame*
			      (format nil "Encountered error code 1, likely due to an incorrect sudo password, in~%~a" xbps-string)))
		(t
		 (notify-user *application-frame* 
			      (format nil "An Error Occured in XBPS:~%~a" ;; err
				      xbps-string)
			      :name "XBPS ERROR"))))
	nil))))

(let ((shell (uiop:launch-program "/bin/sh" :input :stream :output :stream)))
  (defun fast-xbps (program flags &rest arguments)
    (handler-case
	(let* ((args (if (and (= 1 (length arguments))
			      (listp (car arguments)))
			 (car arguments)  arguments))
	       (formatstr (concatenate 'string
				       (if *sudo-password* "echo \"~a\" | sudo -S " "~a")
				       (cond ((and (listp (car args))
						   (listp (caar args)))
					      "xbps-~a ~a~{~{~{~a ~}~}~}")
					     ((listp (car args))
					      "xbps-~a ~a~{~{~a ~}~}")
					     (t "xbps-~a ~a~{~a ~}")))))
	  (write-line (format nil formatstr
			      (or *sudo-password* "")
			      (or (and (symbolp program)
				       (string-downcase
					(symbol-name program)))
				  (and (stringp program) program))
			      (or (and flags
				       (< 0 (length flags))
				       (concatenate 'string
						    "-" flags " "))
				  "")
			      args)
		      (uiop:process-info-input shell))
	  (force-output (uiop:process-info-input shell))
	  (let ((ostr (read-line (uiop:process-info-output shell)))
		(stream (uiop:process-info-output shell)))
	    (if (listen stream)
		(loop while (listen stream)
		      do (setf ostr (concatenate 'string
						 ostr
						 '(#\Newline)
						 (read-line stream)))))
	    ;; (do ((line (read-line stream nil)
	    ;; 	       (read-line stream nil)))
	    ;; 	((null line))
	    ;;   (setf string (concatenate 'string string line)))
	    ostr)
	  ;; string
	  )
      (uiop:subprocess-error (err)
	(let ((code (uiop:subprocess-error-code err)))
	  (cond ((= code 2)
		 (error 'unknown-package-error))
		(t
		 (notify-user *application-frame*
			      (format nil "An Error Occured in XBPS" ;; err
				      )
			      :name "XBPS ERROR"))))))))


