
(in-package :xbps)

(defmacro bold ((stream) &body body)
  `(with-text-face (,stream :bold)
     ,@body))

(defmacro italic ((stream) &body body)
  `(with-text-face (,stream :italic)
     ,@body))

(define-application-frame xbps () ()
  (:menu-bar xbps-menu-bar)
  (:top-level (clim:default-frame-top-level :prompt 'prompt))
  (:panes
   (marks :application
	  :incremental-redisplay t
	  :scroll-bars nil
	  :display-function #'display-marks)
   (interactor :interactor :incremental-redisplay t)
   ;; (help-pane :application :incremental-redisplay t
   ;; 	      :scroll-bars nil :display-function #'display-help)
   (inspection-pane :application
		    :incremental-redisplay t
		    :scroll-bars nil
		    :display-time nil
		    :display-function #'display-package)
   (search-pane :application
                :scroll-bars nil
		:incremental-redisplay t
		:display-function #'display-search
		;; :display-time nil
		)
   (shell-command-pane :application
		       :scroll-bars nil
		       :incremental-redisplay t
		       :display-function #'display-shell-command))
  (:layouts
   (default
    (vertically ()
      (20 marks)
      (:fill (scrolling () search-pane))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (1/4 interactor)))
   (inspect-package
    (vertically ()
      (20 marks)
      (:fill (scrolling () inspection-pane))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (1/4 interactor)))
   (shell-command
    (vertically ()
      (20 marks)
      (:fill (scrolling () shell-command-pane))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (1/4 interactor)))))

(defun prompt (pane frame)
  (declare (ignore frame))
  (terpri pane)
  (surrounding-output-with-border (pane :move-cursor nil ;; :shape :drop-shadow
					)
    (format pane "Enter Command: "))
  (stream-increment-cursor-position pane 15 3))

(defun app-main ()
  (when (= 0 (length (sb-ext:list-all-timers)))
    (sb-ext:schedule-timer *sudo-timer* 60 :repeat-interval 60))
  (run-frame-top-level (make-application-frame 'xbps)))

(make-command-table 'xbps-menu-bar
		    :errorp nil
		    :menu '(("Quit" :command com-quit)
			    ("Marks" :menu xbps-marks-menu)))

(make-command-table 'xbps-marks-menu
		    :errorp nil
		    :menu '(("Install Marked Packages"
			     :command com-install-marks)
			    ("Remove Marked Packages"
			     :command com-uninstall-marks)))

(define-xbps-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

(define-presentation-type package-presentation ())

(define-presentation-to-command-translator install-package
    (package-presentation com-install-package xbps
     :gesture :select
     :documentation "Install Package"
     :priority 0)
    (p)
  (list p))

(define-presentation-to-command-translator mark-package
    (package-presentation com-mark-package xbps
     :gesture :select
     :priority 5
     :documentation "Mark Package")
    (p)
  (list p))

(defparameter *marks* nil)

(defmacro for-stream-lines-clawk ((stream &optional (strmvar (gensym))
				      (linevar (gensym)))
				  &body body)
  "This macro exists because clawk doesnt use &body, resulting ugly indentation"
  #+:Symbolics (declare (zwei:indentation 1 1))
  (clawk::expand-for-stream-lines strmvar linevar stream body))

(defmacro with-fields-clawk ((&optional fields sourcestr (fieldsep-pattern '(clawk::FS)))
			     &body body)
  "This macro exists because clawk doesnt use &body, resulting ugly indentation
Split the source string into fields based on the field separator,
bind the field array to the fields variable."
  #+:Symbolics (declare (zwei:indentation 1 1))
  (clawk::expand-with-fields fields sourcestr fieldsep-pattern body))

(let (before-current
      current
      after-current
      last-search-term)
  (defun set-last-search-term (string)
    (setf last-search-term string))
  (defun refresh-search (&optional (search-function 'search-for-package))
    (populate-search-with-string (apply search-function last-search-term)))
  (defun current () current)
  (defun add-current-to-mark ()
    (when current
      (setf *marks* (cons (second current) *marks*))))
  (defun move-current-up (&optional (ammount 1))
    (loop for x from 1 to ammount
	  when (and current before-current)
	    do (setf after-current (cons current after-current))
	       (setf current (pop before-current))))
  (defun move-current-down (&optional (ammount 1))
    (loop for x from 1 to ammount
          when (and current after-current)
	    do (setf before-current (cons current before-current))
	       (setf current (pop after-current))))
  (defun populate-search-with-string (string)
    (let ((stream (make-string-input-stream (format nil "~a" string)))
	  i n d)
      (setf current nil after-current nil before-current nil)
      (for-stream-lines-clawk (stream)
	(with-fields-clawk ((x y &rest z))
	  (setf i (append i (list x))
		n (append n (list y))
		d (append d (list z)))))
      (let* ((installed (pop i))
	     (name (pop n))
	     (desc (pop d)))
	(setf current
	      (list (format nil "~a" installed)
		    (format nil "~a" name)
		    (format nil "~{~a ~}" desc))))
      (loop for installed in (reverse i)
	    for name in (reverse n)
	    for desc in (reverse d)
	    do (setf after-current
		     (cons (list (format nil "~a" installed)
				 (format nil "~a" name)
				 (format nil "~{~a ~}" desc))
			   after-current)))))
  (defun display-search (frame pane)
    (declare (ignorable frame))
    (slim:with-table (pane)
      (labels ((display (item)
		 (let ((list item))
		   (with-output-as-presentation (pane
						 (second item)
						 'package-presentation
						 :single-box t)
		     (slim:row
		       (slim:cell (format pane "~a" (pop list)))
		       (slim:cell (format pane "~a" (pop list)))
		       (slim:cell (format pane "~a" (pop list)))))
		   )))
	(when before-current
	  (loop for item in (reverse before-current)
		do (with-drawing-options
		       (pane :ink (if (member (second item) *marks*
					      :test #'string=)
				      +orange-red+ +black+))
		     (display item))))
	(when current
	  (with-drawing-options (pane :ink +purple+)
	    (with-drawing-options
		(pane :ink (if (member (second current) *marks*
				       :test #'string=)
			       +red+ +purple+))
	      (display current))))
	(when after-current
	  (loop for item in after-current
		do (with-drawing-options
		       (pane :ink (if (member (second item) *marks*
					      :test #'string=)
				      +orange-red+ +black+))
		     (display item))))))))

(defun display-marks (f pane)
  (declare (ignorable f))
  (format pane "Marked Packages:  ~{~a~^, ~}" *marks*))

;;; do do window type stuff from xlib and whatnot
;; (format pane "wm-class: ~a, wm-type: ~a"
;; 	  (xlib:get-wm-class
;; 	   (clim-clx:clx-port-window
;; 	    (clim:port *application-frame*)))
;; 	  (xlib:get-property
;; 	   (clim-clx:clx-port-window
;; 	    (clim:port *application-frame*))
;; 	   :_net_wm_window_type))
;; (xlib:set-wm-class
;;  (clim-clx:clx-port-window
;;   (clim:port *application-frame*))
;;  "XBPS_12" "XBPS_CLASS")

(defun display-help (f p)
  (declare (ignore f))
  (format p "Enter your password. To submit, press M-RET (Meta/Alt + Return/Enter)"))

(let ((display ""))
  (defun set-shell-command-display (string)
    (setf display string))
  (defun display-shell-command (f p)
    (declare (ignore f))
    (with-end-of-line-action (p :allow)
      (write-string display p))))
