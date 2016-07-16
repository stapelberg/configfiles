;;;; © 2012 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-emacs.el sets general emacs configuration
;;;; like fonts and keybindings.

(provide 'zkj-emacs)

;;;; Of course, everything is UTF-8.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; For presentations
;;(set-default-font "Monospace 16")

;; On hidpi displays, use an Xft font that scales well.
(set-default-font "Source Code Pro 8")

;;;; add marmalade package repository, it contains many more packages.
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(if (require 'smex nil t)
    (global-set-key [(meta x)] (lambda ()
				 (interactive)
				 (or (boundp 'smex-cache)
				     (smex-initialize))
				 (global-set-key [(meta x)] 'smex)
				 (smex))))

;;;; general appearance

;; Don’t ask when following a symlink into a version-controlled folder
;; (my ~/configfiles).
(setq vc-follow-symlinks t)

;; No blinking cursor.
(blink-cursor-mode 0)

;; Open the *scratch* buffer by default, not the welcome message.
(setq inhibit-startup-screen t)

;; No toolbar.
(tool-bar-mode -1)

;; Better than the default.
(load-theme 'tango-dark t)

;; Use google-chrome to open URLs.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; ido-mode has a *much* better buffer selection (and file opening) :).
(ido-mode)

;; Store backups in a single directory (/tmp/emacs-backups) so that
;; they don’t clutter up my filesystem.
(let ((backupdir "/tmp/emacs-backups/"))
  (mkdir backupdir t)
  (setq backup-directory-alist `(("." . ,backupdir))))

;;;; keybindings

;; C-z by default suspends the session, which is… incredibly useless.
(global-set-key (kbd "C-z") 'undo)

;; M-n is unbound by default, so bind it to scroll the window without
;; moving the cursor, like ^E in vim. Likewise for M-p, but in the
;; other direction.
(defun ctrl-e-in-vim ()
  (interactive)
  (scroll-up 3))

(defun ctrl-y-in-vim ()
  (interactive)
  (scroll-down 3))

(global-set-key (kbd "M-n") 'ctrl-e-in-vim)
(global-set-key (kbd "M-p") 'ctrl-y-in-vim)

;; Make C-c C-f expand filenames (like vim’s omni-complete)
(global-set-key (kbd "C-c C-f") 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;; Easy window switching with M-<direction>
(require 'windmove)

(global-set-key [(meta left)]  'windmove-left)
(global-set-key [(meta up)]    'windmove-up)
(global-set-key [(meta right)] 'windmove-right)
(global-set-key [(meta down)]  'windmove-down)

;; Automatically scroll to the end of the compilation buffer.
(setq compilation-scroll-output t)

;; Save minibuffer history (for compile command etc.)
(savehist-mode 1)
