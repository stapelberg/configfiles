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
(add-to-list 'load-path "~/configfiles/config/emacs/lisp/emacs-livereload")
(package-initialize)

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

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
  :bind (("C-x g" . magit-status))
  :config
  (load "zkj-gbp")
  ;; magit: don’t restore old window configurations
  (setq magit-bury-buffer-function 'magit-mode-quit-window))

;; Hugo-related settings.
(load "zkj-hugo")

(if (string-prefix-p "pacna" system-name)
    (load "pacna-early"))

;; Settings stored by the customize interface.
(setq custom-file "~/configfiles/config/emacs/custom.el")
(load custom-file)

(add-hook
 'find-file-hook
 (lambda ()
   (when (and (stringp buffer-file-name)
	      (string-match "\\.asciidoc\\'" buffer-file-name))
     (if (require 'livereload nil t)
	 (load "zkj-live-reload")))))
