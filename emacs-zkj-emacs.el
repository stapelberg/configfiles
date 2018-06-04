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

;; Contrary to global-set-key, whose effects can be shadowed by modes (e.g. the
;; GNUmakefile mode shadows C-c C-f), bind-key overwrites keys in all modes.
(require 'bind-key)

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

(setq default-frame-alist '((font . "Go Mono 8")))

;;;; add marmalade package repository, it contains many more packages.
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
	     '("melpa" .
	       "https://melpa.org/packages/"))

(if (require 'smex nil t)
    (bind-key* "M-x" (lambda ()
				 (interactive)
				 (or (boundp 'smex-cache)
				     (smex-initialize))
				 (bind-key* "M-x" 'smex)
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

;; Disable searching in other directories when there are no matches
;; (more annoying than helpful).
(setq ido-auto-merge-work-directories-length -1)

;; Store backups in a single directory (/tmp/emacs-backups) so that
;; they don’t clutter up my filesystem.
(let ((backupdir "/tmp/emacs-backups/"))
  (mkdir backupdir t)
  (setq backup-directory-alist `(("." . ,backupdir))))

;;;; keybindings

;; C-z by default suspends the session, which is… incredibly useless.
(bind-key* "C-z" 'undo)

;; M-n is unbound by default, so bind it to scroll the window without
;; moving the cursor, like ^E in vim. Likewise for M-p, but in the
;; other direction.
(defun ctrl-e-in-vim ()
  (interactive)
  (scroll-up 3))

(defun ctrl-y-in-vim ()
  (interactive)
  (scroll-down 3))

(bind-key* "M-n" 'ctrl-e-in-vim)
(bind-key* "M-p" 'ctrl-y-in-vim)

;; Make C-c C-f expand filenames (like vim’s omni-complete)

(bind-key* "C-c C-f" 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;; Easy window switching with M-<direction>
(require 'windmove)

(bind-key* "<M-left>" 'windmove-left)
(bind-key* "<M-up>" 'windmove-up)
(bind-key* "<M-right>" 'windmove-right)
(bind-key* "<M-down>" 'windmove-down)

;; Automatically scroll to the end of the compilation buffer.
(setq compilation-scroll-output t)

;; Don’t ask to save files before compilation, just save them.
(setq compilation-ask-about-save nil)

;; Don’t ask to kill currently running compilation, just kill it.
(setq compilation-always-kill t)

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
  (if (window-system)
      (x-urgency-hint (window-frame (get-buffer-window buf)) 1)))
(add-hook 'compilation-finish-functions #'compilation-finished-hook)

;; C-4 is a good choice as per “Good Key Choices” in
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html
(bind-key* "C-4" 'zkj-recompile)

(defun zkj-recompile ()
  "Interrupt current compilation and recompile"
  (interactive)
  (ignore-errors (kill-compilation))
  (recompile))

;; tell tramp that my ~/.ssh/config is already set up for master mode
;; (tramp will not use master mode otherwise).
(setq tramp-use-ssh-controlmaster-options nil)

;; org mode starts up in truncated mode (as opposed to line wrapping)
;; by default; disable that behavior:
(set 'org-startup-truncated nil)

;; winner-mode provides C-c left and C-c right to undo/redo window
;; configuration changes.
(winner-mode)

;; use 80 characters for line wrapping with M-q
(setq-default fill-column 80)

;; display line numbers and column numbers in all modes
(setq line-number-mode t)
(setq column-number-mode t)

;; automatically revert buffers when files change
(global-auto-revert-mode 1)

;; magit: bind magit-status to C-x g
(bind-key* "C-x g" 'magit-status)

;; See editorconfig.org
(if (require 'editorconfig nil t)
    (editorconfig-mode 1))

;; Always ask for y/n, never yes/no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always show the buffer name in the frame title (Emacs default is to show the
;; hostname when there is only one frame).
(setq frame-title-format
      (setq icon-title-format
	    '((:eval (if (buffer-file-name)
			 (abbreviate-file-name (buffer-file-name))
		       "%b"))
	      (:eval (if (buffer-modified-p)
			 "*"))
	      " - Emacs")
	    ))

;; Disable the menu bar by default. Use M-x menu-bar-mode to make it re-appear.
(menu-bar-mode -1)

;; Colorize compilation output (why is this not the default?!):
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Add final newlines to all files by default, not just in modes which think
;; this is useful.
(setq require-final-newline t)
