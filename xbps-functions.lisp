
(in-package :xbps)

;;;;;;;;;;;;;;;;;;;;
;;; xbps-install ;;;
;;;;;;;;;;;;;;;;;;;;

(defun remove-packages-by-name (&rest packages)
  (with-sudo nil
    (xbps 'remove "y" packages)))

(defun install-packages-by-name (&rest packages)
  (with-sudo nil
    (xbps 'install "y" packages)))

(defun remove-packages-and-dependencies (&rest packages)
  (with-sudo nil
    (xbps 'remove "Ry" packages)))

(defun search-for-package (name)
  (xbps 'query "Rs" name))

(defun upgrade-packages (&rest packages)
  (with-sudo nil
    (xbps 'install "Suy" packages)))

(defun reinstall-packages (&rest packages)
  (with-sudo nil
    (xbps 'install "Fy" packages)))

(defun download-packages (&rest packages)
  (with-sudo nil
    (xbps 'install "Dy" packages)))

(defun remove-orphan-dependencies ()
  (with-sudo nil
    (xbps 'remove "o")))

(defun package-information (package)
  (xbps 'query "RS" package))

(defun reverse-package-dependencies (package)
  (xbps 'query "X" package))

(defun package-dependencies (package)
  (xbps 'query "x" package))

(defun package-provides (file)
  (xbps 'query "o" file))

(defun list-installed-packages ()
  (xbps 'query "l"))

(defun clean-all-caches ()
  (with-sudo nil
    (xbps 'remove "O")))

(defun list-installation-sources ()
  (xbps 'query "L"))

(defun refresh-package-database ()
  (with-sudo nil
    (xbps 'install "S")))

(defun local-package-information (package)
  (xbps 'query nil package))

(defun remote-package-files (package)
  (xbps 'query "Rf" package))

(defun explicitly-installed-packages ()
  (xbps 'query "O"))
