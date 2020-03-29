;;;; xbps.asd

(asdf:defsystem #:xbps
  :description "Describe xbps here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clawk
	       #:str
	       #:mcclim
	       #:slim)
  :components ((:file "package")
	       (:file "password")
               (:file "xbps")
	       (:file "xbps-functions")
	       (:file "frame")
	       (:file "commands")
	       (:file "display-functionality")))
