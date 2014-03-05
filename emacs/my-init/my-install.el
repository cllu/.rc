;; For different computer installation.
;; seis is the 64bit CentOS 5.6 Linux, with manually compiled Emacs 23.3.2
;; On Debian, I rely on the package system to install extra package.

(provide 'my-install)
;; For color-theme
;(defvar hostname 'system-name)

;; all the extra packages are in package dir.
(add-to-list 'load-path "~/.emacs.d/package")    ; third-party package

;; SEEM IS Lab Emacs.
(when (string= (substring system-name 0 4) "seis")
  (message "emacs on seis machine."))
