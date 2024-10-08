;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/init.el sets up the load-path and loads modular pieces
;;;; of configuration. All the files start with zkj to make sure there
;;;; is no name-clash.
;;;;
;;;; See also http://emacswiki.org/emacs/DotEmacsDotD

;; Set Garbage Collection threshold extremely high (1 GB),
;; but run Garbage Collection when Emacs loses focuses:
;; https://news.ycombinator.com/item?id=39190110
(setq gc-cons-threshold 1073741824)
(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

;; byte compilation does not make a difference with my current config
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

(add-to-list 'load-path "~/configfiles/config/emacs/lisp/")
(add-to-list 'load-path "~/configfiles/config/emacs/lisp/emacs-livereload")

(progn
  (message "initializing package")
  (package-initialize))

;; Workaround for Emacs < 26.3 (e.g. Debian stable):
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'package-archives
	     '("melpa" .
	       "https://melpa.org/packages/"))

;; https://www.reddit.com/r/emacs/comments/4fqu0a/automatically_install_packages_on_startup/d2b7g30
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; Whenever use-package statements use ensure (directly or via
;; use-package-always-ensure), we need to refresh the package contents first,
;; as installation fails otherwise:
;; https://github.com/jwiegley/use-package/issues/256#issuecomment-263313693
(defun my-package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'my-package-install-refresh-contents))

(advice-add 'package-install :before 'my-package-install-refresh-contents)

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
