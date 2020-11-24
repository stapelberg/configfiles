;;;; © 2012 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-emacs.el sets general emacs configuration
;;;; like fonts and keybindings.

(provide 'zkj-emacs)

;; For presentations
;;(set-default-font "Monospace 16")

;; Of course, everything is UTF-8.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Contrary to global-set-key, whose effects can be shadowed by modes (e.g. the
;; GNUmakefile mode shadows C-c C-f), bind-key overwrites keys in all modes.
(require 'bind-key)

;; C-w and C-y should use the PRIMARY selection (mouse-selected) *and*
;; the CLIPBOARD selection (copy function selected). When yanking,
;; both will be set. When inserting, the more recently changed one
;; will be used.
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;; TODO: this breaks C-w in rectangle mode (C-x space)
;; Make C-w behave like in bash: delete a word when invoked without a region.
;; (defadvice kill-region (before unix-werase activate compile)
;;       "When called interactively with no active region, delete a single word
;;     backwards instead."
;;       (interactive
;;        (if mark-active (list (region-beginning) (region-end))
;;          (list (save-excursion (backward-word 1) (point)) (point)))))


(setq default-frame-alist '((font . "Go Mono 8")))

;;;; add additional package repositories
(add-to-list 'package-archives
	     '("melpa" .
	       "https://melpa.org/packages/"))

(use-package smex
  :defer t
  :init (or (boundp 'smex-cache)
	    (smex-initialize))
  :bind ("M-x" . smex))

;;;; general appearance

;; Don’t ask when following a symlink into a version-controlled folder
;; (my ~/configfiles).
(setq vc-follow-symlinks t) ;; required so that (ma)git finds ~/configfiles as GIT_DIR

;; No blinking cursor.
(blink-cursor-mode 0)

;; Open the *scratch* buffer by default, not the welcome message.
(setq inhibit-startup-screen t)

;; Better than the default.
(load-theme 'tango-dark t)

;; Use google-chrome to open URLs.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; ido-mode has a *much* better buffer selection (and file opening) :).
(defun zkj-lazy-ido-enable ()
  "since ido is loaded with Emacs, use-package cannot defer"
  (ido-mode t)
  ;; Disable searching in other directories when there are no matches
  ;; (more annoying than helpful).
  (setq ido-auto-merge-work-directories-length -1)

  (if (require 'ido-sort-mtime nil t)
      (ido-sort-mtime-mode t)))

(defun zkj-lazy-ido-switch-buffer ()
  "ibuffer wrapper"
  (interactive)
  (zkj-lazy-ido-enable)
  (call-interactively 'ido-switch-buffer))

(defun zkj-lazy-ido-find-file ()
  "find-file wrapper"
  (interactive)
  (zkj-lazy-ido-enable)
  (call-interactively 'ido-find-file))

(use-package ido
  :ensure nil ; built-in
  :bind (("C-x b" . zkj-lazy-ido-switch-buffer)
	 ("C-x C-f" . zkj-lazy-ido-find-file)))

;; Store backups and auto-save files in a single directory so that
;; they don’t clutter up my filesystem (or fail to be written on curlftpfs):
(let ((backupdir (format "%s/emacs-backups%d/" (or (getenv "XDG_RUNTIME_DIR") "/tmp") (user-uid))))
  (mkdir backupdir t)
  (setq backup-directory-alist `(("." . ,backupdir)))
  (setq auto-save-file-name-transforms
	`((".*" ,backupdir t))))

;;;; keybindings

;; C-z by default suspends the session, which is… incredibly useless.
(bind-key* "C-z" 'undo)

;; Make C-c C-f expand filenames (like vim’s omni-complete)

(bind-key* "C-c C-f" 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

;; Easy window switching with M-<direction>
(use-package windmove
  :ensure nil ; built-in
  :bind* (("<M-left>" . windmove-left)
	 ("<M-up>" . windmove-up)
	 ("<M-right>" . windmove-right)
	 ("<M-down>" . windmove-down)))

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

;; compile-parent defaults to a make command line using the closest Makefile,
;; i.e. working in any project subdirectory:
(defun compile-parent (command)
  (interactive
   (let* ((make-directory (locate-dominating-file
			   default-directory
			   "Makefile"))
          (command (concat "make -k -C "
                           (shell-quote-argument make-directory)
			   " ")))
     (list (compilation-read-command command))))
  (compile command))

(bind-key* "C-3" 'compile-parent)
;; C-4 is a good choice as per “Good Key Choices” in
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html
(bind-key* "C-4" 'zkj-recompile)

(defun zkj-recompile ()
  "Interrupt current compilation and recompile"
  (interactive)
  (ignore-errors (kill-compilation))
  (recompile))

(use-package tramp
  :defer t
  :config
  ;; does not work in https://github.com/gokrazy/breakglass
  (setq tramp-histfile-override "/dev/null")

  ;; Open e.g. /breakglass:router7:/perm/dhcp4d/leases.json
  ;;
  ;; Requires /perm/sh to be present (use e.g. static busybox) and ls to be in
  ;; $PATH (`ln -s busybox ls` will work).
  (add-to-list 'tramp-methods
	       '("breakglass"
		 (tramp-login-program        "ssh")
		 (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
					      ("-e" "none") ("%h")))
		 (tramp-async-args           (("-q")))
		 (tramp-remote-shell         "/perm/sh")
		 (tramp-remote-shell-login   ("-l"))
		 (tramp-remote-shell-args    ("-c"))))

  ;; tell tramp that my ~/.ssh/config is already set up for master mode
  ;; (tramp will not use master mode otherwise).
  (setq tramp-use-ssh-controlmaster-options nil))

(use-package org
  :defer t
  :config
  (progn
    (setq org-startup-truncated nil ; wrap lines instead of truncating
	  org-return-follows-link t ; follow hyperlinks when pressing RET
	  org-agenda-window-setup 'current-window ; open agenda in same Emacs window
	  org-startup-folded nil)   ; default to #+STARTUP: showall
    (setq org-src-window-setup 'current-window)
    ;; Make shell source blocks work in Org Mode:
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)))
    ;; follow links in the same Emacs window
    (setcdr (assoc 'file org-link-frame-setup) 'find-file)))

;; https://github.com/bmag/emacs-purpose (“window-purpose” on MELPA) allows
;; dedicating a window to a certain purpose (e.g. compilation, magit, edit,
;; …) using C-c , d.
;;
;; When happy with a layout, save using:
;; M-x purpose-save-window-layout NAME RET TAB RET
;; …and load using:
;; M-x purpose-load-window-layout NAME
(use-package window-purpose
  :defer t
  :config
  (progn
    ;; Prefer opening new buffers in the same Emacs window.
    ;;
    ;; I don’t want this to happen when not working with
    ;; purpose mode in my layout, as it results in magit
    ;; windows not popping up anymore. Instead, they just
    ;; replace the current window. To make matters worse,
    ;; the COMMITMSG buffer ends up being buried under
    ;; the diff buffer, which is really confusing.
    (setq pop-up-windows nil)

    ;; make M-x man open manpages in the same Emacs window
    (setq Man-notify-method 'pushy)

    (add-to-list 'purpose-user-mode-purposes '(compilation-mode . compile))
    (add-to-list 'purpose-user-mode-purposes '(dired-mode . edit))
    (add-to-list 'purpose-user-name-purposes '("*Go Test*" . compile))
    (add-to-list 'purpose-user-regexp-purposes '("^magit: " . magit))
    (define-key purpose-mode-map (kbd "C-x b") nil)
    (define-key purpose-mode-map (kbd "C-x C-f") nil)
    (purpose-compile-user-configuration) ;; activate changes
    ))

;; https://github.com/bmag/emacs-purpose (“window-purpose” on MELPA) allows
;; dedicating a window to a certain purpose (e.g. compilation, magit, edit,
;; …) using C-c , d.
;;
;; When happy with a layout, save using:
;; M-x purpose-save-window-layout NAME RET TAB RET
;; …and load using:
;; M-x purpose-load-window-layout NAME
(defun zkj-purpose ()
  "loads purpose and restores my typical window layout"
  (interactive)
    (progn
      (purpose-mode)
      (message "loading window layout")
      (purpose-load-window-layout "zkj")))

(use-package winner
  :ensure nil ; built-in
  :bind (("C-c <left>" . winner-undo)
	 ("C-c <right>" . winner-redo))
  ;; Initialize winner-mode immediately; it needs to record all window
  ;; configurations before the first invokation to be useful.
  :init
  (winner-mode t))

;; use 80 characters for line wrapping with M-q
(setq-default fill-column 80)

;; display line numbers and column numbers in all modes
(setq line-number-mode t)
(setq column-number-mode t)

;; automatically revert buffers when files change
(global-auto-revert-mode 1)

;; ;; See editorconfig.org
;; (if (require 'editorconfig nil t)
;;     (editorconfig-mode 1))

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

;; ;; Disable the scroll bar by default, they flicker. Use M-x scroll-bar-mode to
;; ;; make it re-appear.
;; (scroll-bar-mode -1)

;; Add final newlines to all files by default, not just in modes which think
;; this is useful.
(setq require-final-newline t)

;; Persistent desktops (which buffers are open)
(setq desktop-save t) ;; always save
;;(desktop-save-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Daemon mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'seq) ;; Emacs 25.1
(defun zkj-reload-all ()
  "reloads all running Emacs daemons"
  (interactive)
  ;; socket location when using emacsclient --socket-name:
  (let* ((socket-dir (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
	 (not-dot (lambda (x) (not (or (string= x ".")
				       (string= x "..")))))
	 (daemons (seq-filter not-dot (directory-files socket-dir))))
    (mapcar (lambda (daemon) (server-eval-at daemon '(load-file user-init-file))) daemons)))

(setq counsel-fzf-cmd "fd --type f | fzf -f \"%s\"")

(defun fzf ()
  "fuzzy find on the closest git repository"
  (interactive)
  (progn
    (require 'magit)
    (counsel-fzf nil (magit-toplevel))))

;; You can interactively overwrite this using e.g. M-:
;;   (defun zkj-ag-default-directory () "~/kinx/chibi41")
(defun zkj-ag-default-directory ()
    (progn
      (require 'magit)
      (magit-toplevel)))

(defun zkj-ag (string directory)
  "ag defaulting to the project directory"
  (interactive
   (list (ag/read-from-minibuffer "Search string")
	 (read-directory-name "Directory: " (zkj-ag-default-directory))))
  (ag/search string directory))

;; M-x website menu partial
(defun website ()
  "invokes counsel-fzf on ~/hugo"
  (interactive)
  (counsel-fzf nil "~/hugo"))

;; Emacs doesn’t know about XC
;; (https://en.wikipedia.org/wiki/XC_(programming_language)) and defaults to
;; opening .xc files as image files.
(add-to-list 'auto-mode-alist '("\\.xc\\'" . c-mode))

;; https://eklitzke.org/smarter-emacs-clang-format
(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (file-exists-p (expand-file-name ".clang-format" (magit-toplevel)))
    (clang-format-buffer)))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

(add-hook 'c-mode-hook 'clang-format-buffer-smart-on-save)
(add-hook 'c++-mode-hook 'clang-format-buffer-smart-on-save)

;; eglot is a language server protocol (LSP) package for Emacs, which is more
;; minimalist than lsp-mode: https://github.com/joaotavora/eglot
(if (version< emacs-version "26.3")
    (message "not installing eglot as emacs is too old")
    (use-package eglot
      :hook
      (c-mode . eglot-ensure)
      (c++-mode . eglot-ensure)
      (go-mode . eglot-ensure)))

;; Do not create lockfiles, they trip up e.g. hugo (because they are an
;; unreadable symlink:
;;   File: /home/michael/hugo/content/posts/.#2019-07-11-introducing-distri.markdown -> michael@xps.25964:1563787245
(setq create-lockfiles nil)

;; Make M-next and M-prior (M-PageDown and M-PageUp, respectively) preferably
;; operate on the window displaying the compilation buffer, if precisely 1.
(defun zkj-with-compilation-window (FUNC)
  (let ((compilation-mode-windows
	 (mapcan
	  (lambda (window)
	    (with-current-buffer
		(window-buffer window)
	      (if (equal major-mode 'compilation-mode)
		  (list window)
		'())))
	  (window-list))))
    (if (= (length compilation-mode-windows) 1)
	(save-selected-window
	  (select-window (car compilation-mode-windows))
	  (funcall FUNC))
      (scroll-other-window))))

(defun zkj-scroll-compilation-window-down ()
  "Scroll the compilation window down"
  (interactive)
  (zkj-with-compilation-window #'scroll-down))

(defun zkj-scroll-compilation-window-up ()
  "Scroll the compilation window up"
  (interactive)
  (zkj-with-compilation-window #'scroll-up))

(bind-key* "<M-next>" #'zkj-scroll-compilation-window-up)
(bind-key* "<M-prior>" #'zkj-scroll-compilation-window-down)
