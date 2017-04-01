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

;; C-w and C-y should use the PRIMARY selection (mouse-selected) *and*
;; the CLIPBOARD selection (copy function selected). When yanking,
;; both will be set. When inserting, the more recently changed one
;; will be used.
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;; For presentations
;;(set-default-font "Monospace 16")

;; On hidpi displays, use an Xft font that scales well.
(set-default-font "Source Code Pro 8" nil t)

;;;; add marmalade package repository, it contains many more packages.
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" .
	       "https://melpa.org/packages/"))

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

;; Don’t ask to save files before compilation, just save them.
(setq compilation-ask-about-save nil)

;; Don’t ask to create parent directories when saving files, just
;; create them.
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (not (file-exists-p dir))
                  (make-directory dir t))))))


;; Save minibuffer history (for compile command etc.)
(savehist-mode 1)

;; compilation: set urgency hint when compilation finishes.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS" source nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x100)
	      (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))
(defun compilation-finished-hook (buf status)
  (x-urgency-hint (window-frame (get-buffer-window buf)) 1))
(add-hook 'compilation-finish-functions #'compilation-finished-hook)
