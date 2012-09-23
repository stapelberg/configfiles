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

;;;; Set font to misc-fixed. This requires the following two entries
;;;; in ~/.Xresources:
;;;; Emacs.Font: fontset-normal
;;;; Emacs.Fontset-0: -misc-fixed-medium-r-normal--13-120-75-75-C-70-fontset-normal
(defvar my-font "-misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1")

;; i’m not sure why i have to explicitly overwrite the font for latin1 and ucs charsets :-/
(when (window-system)
  (mapc
   (lambda (charset)
     (set-fontset-font "fontset-normal" charset my-font))
   (list 'iso-8859-1 'ucs)))

;;;; general appearance

;; Open the *scratch* buffer by default, not the welcome message.
(setq initial-buffer-choice t)

;; No toolbar.
(tool-bar-mode -1)

;; Use Mod4 as meta. I probably might want to change this.
(setq x-super-keysym 'meta)

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

;; Make C-c C-f expand filenames (like vim’s omni-complete)
(global-set-key (kbd "C-c C-f") 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))
