;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/init.el sets up the load-path and loads modular pieces
;;;; of configuration. All the files start with zkj to make sure there
;;;; is no name-clash.
;;;;
;;;; See also http://emacswiki.org/emacs/DotEmacsDotD

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(add-to-list 'load-path "~/configfiles/config/emacs/lisp/")
(package-initialize)

(require 'use-package)

;; General emacs settings.
(load "zkj-emacs")

;; LISP/SCHEME-related settings.
(load "zkj-lisp")

;; notmuch-specific configuration, if it is installed.
(if (require 'notmuch nil t)
    (load "zkj-notmuch"))

;; Perl-related settings.
(load "zkj-perl")

;; Go-related settings.
(load "zkj-go")

;; protobuf-related settings.
(load "zkj-protobuf")

;; Magit gbp mode.
(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (progn
    (load "zkj-gbp")))

;; Hugo-related settings.
(load "zkj-hugo")

(if (string-prefix-p "pacna" system-name)
    (load "pacna-early"))

;; Settings stored by the customize interface.
(setq custom-file "~/configfiles/config/emacs/custom.el")
(load custom-file)

(add-to-list 'load-path "~/configfiles/config/emacs/lisp/emacs-livereload")
(if (require 'livereload nil t)
    (load "zkj-live-reload"))
