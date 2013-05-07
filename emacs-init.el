;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/init.el sets up the load-path and loads modular pieces
;;;; of configuration. All the files start with zkj to make sure there
;;;; is no name-clash.
;;;;
;;;; See also http://emacswiki.org/emacs/DotEmacsDotD

(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-initialize)

;; General emacs settings.
(require 'zkj-emacs)

;; LISP/SCHEME-related settings.
(require 'zkj-lisp)

;; notmuch-specific configuration, if it is installed.
(if (require 'notmuch nil t)
    (require 'zkj-notmuch))

;; Perl-related settings.
(require 'zkj-perl)

;; Go-related settings.
(if (require 'go-mode-load nil t)
    (require 'zkj-go))

;; Settings stored by the customize interface.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
