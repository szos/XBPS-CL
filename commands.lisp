(in-package :xbps)

(defmacro define-mark-command (name-and-options arguments &body body)
  "This defines a command that will automatically refresh the last search 
performed. "
  `(define-xbps-command ,name-and-options ,arguments
     ,@body
     (refresh-search)))

(define-xbps-command (com-search :name "Search") ((package string))
  (let ((str (search-for-package package)))
    (populate-search-with-string str)
    (set-last-search-term package)))

(define-xbps-command (com-manual-redisplay :name t) ()
  (redisplay-frame-panes *application-frame*))

(define-xbps-command (com-navigate-down :keystroke (#\n :control)) ()
  (move-current-down)
  ;; (redisplay-frame-pane *application-frame* 'search-pane :force-p t)
  )

(define-xbps-command (com-navigate-up :keystroke (#\p :control)) ()
  (move-current-up)
  ;; (redisplay-frame-pane *application-frame* 'search-pane :force-p t)
  )

(define-xbps-command (com-mark-package) ((package string))
  (setf *marks* (if (member package *marks* :test #'string=)
		    (remove package *marks* :test #'string=)
		    (cons package *marks*))))

(define-xbps-command (com-install-package :name t) ((package string))
  (install-packages-by-name package))

(define-mark-command (com-uninstall-marks :name "Remove Marked Packages") ()
  (when (remove-packages-by-name *marks*)
    (setf *marks* nil)))

(define-mark-command (com-install-marks :name "Install Marked Packages") ()
  (when (install-packages-by-name *marks*)
    (setf *marks* nil)))

(define-xbps-command (com-run-shell-command :name t) ((command string))
  (set-shell-command-display
   (with-output-to-string (output)
     (uiop:run-program command :output output)))
  (unless (equal 'shell-command (frame-current-layout *application-frame*))
    (setf (frame-current-layout *application-frame*) 'shell-command)))

(define-xbps-command (com-run-sudo-shell-command :name t) ((command string))
  (with-sudo nil
    (set-shell-command-display
     (with-output-to-string (output)
       (uiop:run-program (format nil "echo \"~a\" | sudo -S ~a"
				 *sudo-password* command)
			 :output output))))
  (unless (equal 'shell-command (frame-current-layout *application-frame*))
    (setf (frame-current-layout *application-frame*) 'shell-command)))
