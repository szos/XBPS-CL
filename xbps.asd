;;;; xbps.asd

(asdf:defsystem #:xbps
  :description "Graphical frontend for xbps"
  :author "szos at posteo dot net"
  :license  "GPLv3"
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
