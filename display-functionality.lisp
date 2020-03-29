(in-package :xbps)

(defun replace-characters (character-alist string)
  (coerce (loop for c across string
		collect (let ((r (assoc c character-alist)))
			  (or (cdr r) c)))
	  'string))

(defun parse-xbps-search-stream (stream &optional trim-version-number)
  (print "in search-stream")
  (let (accumulator)
    (do ((line (read-line stream nil)
	       (read-line stream nil)))
	((null line))
      (let* ((lstream (make-string-input-stream line))
	     tinst tname tdesc)
	(read-char lstream)
	(when (char= (read-char lstream) #\*) (setf tinst t))
	(read-char lstream)
	(read-char lstream)
	(block read-to-space-block
	  (do ((char (read-char lstream nil)
		     (read-char lstream nil)))
	      ((null char))
	    (setf tname (cons char tname))
	    (when (char= char #\space)
	      (return-from read-to-space-block))))
	(do ((char (read-char lstream nil)
		   (read-char lstream nil)))
	    ((null char))
	  (setf tdesc (cons char tdesc)))
	(setf accumulator (cons (list
				 tinst
				 (if trim-version-number 
				     (reverse
				      (format nil "~{~a~^-~}"
					      (cdr (cl-ppcre:split
						    "-" (coerce tname 'string)))))
				     (string-trim
				      '(#\space) (coerce (reverse tname) 'string)))
				 (string-trim
				  '(#\space) (coerce (reverse tdesc) 'string)))
				accumulator))))
    (reverse accumulator)))

(defun parse-xbps-search-string (string &optional trim-version-number)
  (let ((stream (make-string-input-stream string))
	accumulator)
    (do ((line (read-line stream nil)
	       (read-line stream nil)))
	((null line))
      (let* ((lstream (make-string-input-stream line))
	     tinst tname tdesc)
	(read-char lstream)
	(when (char= (read-char lstream) #\*) (setf tinst t))
	(read-char lstream)
	(read-char lstream)
	(block read-to-space-block
	  (do ((char (read-char lstream nil)
		     (read-char lstream nil)))
	      ((null char))
	    (setf tname (cons char tname))
	    (when (char= char #\space)
	      (return-from read-to-space-block))))
	(do ((char (read-char lstream nil)
		   (read-char lstream nil)))
	    ((null char))
	  (setf tdesc (cons char tdesc)))
	(setf accumulator (cons (list
				 tinst
				 (if trim-version-number 
				     (reverse
				      (format nil "~{~a~^-~}"
					      (cdr (cl-ppcre:split
						    "-" (coerce tname 'string)))))
				     (string-trim
				      '(#\space) (coerce (reverse tname) 'string)))
				 (string-trim
				  '(#\space) (coerce (reverse tdesc) 'string)))
				accumulator))))
    (reverse accumulator)))

(let ((search-display nil)
      (last-search-term nil))
  (defun set-last-search-term (s)
    (setf last-search-term s))
  (defun refresh-search (&optional (search-function 'search-for-package))
    ;; (populate-search-with-string (apply search-function last-search-term))
    (set-search-display
     (parse-xbps-search-string (apply search-function (list last-search-term)) t)))
  (defun set-search-display (display-list)
    (setf search-display display-list))
  (defun display-search (f pane)
    (declare (ignorable f))
    (with-end-of-line-action (pane :allow)
      (slim:with-table (pane)
	(loop for item in search-display
	      do (destructuring-bind (installed name desc) item
		   (with-output-as-presentation (pane name
						      'package-presentation
						      :single-box t)
		     (slim:row
		       (slim:cell (write-string (if installed "*" " ") pane))
		       (with-drawing-options
			   (pane :ink (if (member name *marks* :test #'string=)
					  +orange-red+ +black+))
			 (slim:cell (write-string name pane))
			 (slim:cell (write-string desc pane)))))))))))

(define-xbps-command (com-search :name "Search") ((string string))
  (unless (equal 'default (frame-current-layout *application-frame*))
    (setf (frame-current-layout *application-frame*) 'default))
  (set-search-display (parse-xbps-search-string (search-for-package string) t))
  (set-last-search-term string))

(let ((shell (uiop:launch-program "/bin/sh" :input :stream :output :stream)))
  (defun fast-search-for-package (name &key (remote t))
    (write-line (format nil "xbps-query ~a ~a"
			(if remote "-Rs" "-s") name)
		(uiop:process-info-input shell))
    (print "forcing output")
    (force-output (uiop:process-info-input shell))
    (print "beginning parsing")
    (let (;; (stream (uiop:process-info-output shell))
    	  accumulator)
      (do ((line (read-line (uiop:process-info-output shell) nil)
    		 (read-line (uiop:process-info-output shell) nil)))
    	  ((null line))
    	(let* ((lstream (make-string-input-stream line))
    	       tinst tname tdesc)
    	  (read-char lstream)
    	  (when (char= (read-char lstream) #\*) (setf tinst t))
    	  (read-char lstream)
    	  (read-char lstream)
    	  (block read-to-space-block
    	    (do ((char (read-char lstream nil)
    		       (read-char lstream nil)))
    		((null char))
    	      (setf tname (cons char tname))
    	      (when (char= char #\space)
    		(return-from read-to-space-block))))
    	  (do ((char (read-char lstream nil)
    		     (read-char lstream nil)))
    	      ((null char))
    	    (setf tdesc (cons char tdesc)))
    	  (setf accumulator (cons (list
    				   tinst
    				   (reverse
    				    (format nil "~{~a~^-~}"
    					    (cdr (cl-ppcre:split
    						  "-" (coerce tname 'string)))))
    				   (string-trim
    				    '(#\space) (coerce (reverse tdesc) 'string)))
    				  accumulator))))
      (reverse accumulator))
    (let ((ostr "")
    	  (stream (uiop:process-info-output shell))
    	  (first t))
      (when (listen stream)
    	(loop while (listen stream)
    	      ;; do (let ((c (read-char stream)))
		   
    	      ;; 	   (setf ostr
    	      ;; 		 (concatenate 'string ostr
    	      ;; 			      (list c))))
    	      ;; do (let (accumulator installed name
    	      ;; 	       (char (read-char stream)))
    	      ;; 	   (cond ((char= char #\newline)
    	      ;; 		  (psetq ostr (concatenate 'string ostr
    	      ;; 					   '(#\newline)
    	      ;; 					   name " " accumulator)
    	      ;; 			 accumulator nil)
    	      ;; 		  ;; parse [-] or [*]
    	      ;; 		  (when (listen stream)
    	      ;; 		    (read-char stream)
    	      ;; 		    (setf installed (char= (read-char stream) #\*))
    	      ;; 		    (read-char stream)
    	      ;; 		    (read-char stream)))
    	      ;; 		 ((char= char #\space)
    	      ;; 		  (psetq name accumulator
    	      ;; 			 accumulator nil))
    	      ;; 		 (t (setf accumulator (cons char accumulator)))))
    	      do (setf ostr (concatenate 'string ostr
    	      				 (if first
    	      				     (setf first nil)
    	      				     '(#\newline))
    	      				 (read-line stream)))
    	      ))
      ostr)
    ))

(define-xbps-command (com-test-search :name "Fast Search") ((string string))
  (unless (equal 'default (frame-current-layout *application-frame*))
    (setf (frame-current-layout *application-frame*) 'default)))

(define-xbps-command (com-inspect :name "Inspect Package") ((package string))
  (swap-to-package-display package))

(define-presentation-type back-to-search-pane ())

(define-xbps-command (com-back-to-search-pane) ((trash t))
  (declare (ignore trash))
  (unless (equal 'default (frame-current-layout *application-frame*))
    (setf (frame-current-layout *application-frame*) 'default)))

(define-presentation-to-command-translator return-to-search
    (back-to-search-pane com-back-to-search-pane xbps
     :gesture :select)
    (m)
  (list m))

(define-presentation-to-command-translator inspect-package
    (package-presentation com-inspect xbps
     :gesture :select
     :documentation "Inspect Package"
     :priority 1)
    (p)
  (list p))

(define-xbps-command (com-compose-email) ((email string) (package string))
  (uiop:run-program (format nil "xdg-email --subject \"regarding ~a\" '~a'"
			    package email)))

(define-presentation-type open-email ())

(define-presentation-to-command-translator open-email-translator
    (open-email com-compose-email xbps
     :gesture :select
     :documentation "Compose an email")
    (e-and-p)
  (list (car e-and-p) (cdr e-and-p)))

(define-xbps-command (com-open-homepage-url) ((url string))
  (uiop:launch-program (format nil "xdg-open '~a'" (string-trim '(#\space) url))))

(define-presentation-type open-homepage ())

(define-presentation-to-command-translator open-url
    (open-homepage com-open-homepage-url xbps
     :gesture :select
     :documentation "Open Homepage")
    (url)
  (list url))

(let ((package ""))
  (defun swap-to-package-display (pkg)
    (setf package pkg)
    (setf (frame-current-layout *application-frame*) 'inspect-package))
  (defun display-package (frame pane)
    (declare (ignorable frame))
    (with-end-of-line-action (pane :allow)
      (with-drawing-options (pane :ink +green4+)
	(bold (pane)
	  (with-output-as-presentation (pane ""
					     'back-to-search-pane)
	    (format pane "Return To Search~%"))))
      (let ((p (make-string-input-stream
		(handler-case (xbps 'query nil package)
		  (unknown-package-error ()
		    (xbps 'query "R" package))))))
	(for-stream-lines-clawk (p)
	  (with-fields-clawk ((part &rest end) nil ":")
	    (cond ((string= part "homepage")
		   (format pane "~a: " part)
		   (with-drawing-options (pane :ink +green4+)
		     (surrounding-output-with-border (pane :shape :underline
							   :move-cursor nil)
		       (with-output-as-presentation (pane
						     (format nil "~{~a~^:~}" end)
						     'open-homepage
						     :single-box t)
			 ;; (format pane "~a: ~{~a~}~%" part end)
			 (format pane "~a" (format nil "~{~a~^:~}" end)))))
		   (format pane "~%")
		   (stream-increment-cursor-position pane 0 1))
		  ((string= part "maintainer")
		   (format pane "~a: " part)
		   (let* ((col? nil)
			  (maintainer
			    (coerce
			     (loop for c across (format nil "~{~a~^:~}" end)
				   ;; do (format pane "~a" c)
				   when (char= c #\<)
				     do (setf col? t)
				   when col?
				     collect (progn (when (char= c #\>)
						      (setf col? nil))
						    c))
			     'string)))
		     (with-drawing-options (pane :ink +green4+)
		       (surrounding-output-with-border (pane :shape :underline
							     :move-cursor nil)
			 (with-output-as-presentation (pane
						       (cons
							(string-trim '(#\> #\<)
								     maintainer)
							package)
						       'open-email
						       :single-box t)
			   (format pane "~{~a~^:~}" end))))
		     (format pane "~%")
		     (stream-increment-cursor-position pane 0 1)))
		  ((char= (char part 0) #\tab)
		   (format pane "↳    ~a~%" (subseq part 1)))
		  (t (format pane "~a: ~{~a~}~%" part end)))))))))
