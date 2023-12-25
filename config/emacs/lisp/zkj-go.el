;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-go.el sets go-specific
;;;; configuration.
(provide 'zkj-go)

(defun zkj-go-test-at-point ()
  (interactive)
  (let ((go-test-args "-count=1")
	(go-test-verbose t))
    (go-test-current-test)))

(defun zkj-find-definition ()
  (interactive)
  (call-interactively 'xref-find-definitions)
  (recenter-top-bottom 0))

(defun zkj-eglot-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))

(defun zkj-go-mode-hook ()
  ;; Format (previously: gofmt) and organize imports (previously: goimports).
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'zkj-eglot-organize-imports nil t)

  ;; Jump to first error. Go has no warnings.
  (setq compilation-scroll-output 'first-error)
  ;; Jump to the definition of the symbol under the cursor.
  ;; (Jump back with M-*)
  (local-set-key (kbd "M-.") 'zkj-find-definition)
  (local-set-key (kbd "C-c d") 'godoc-at-point)
  (local-set-key (kbd "M-g t") 'zkj-go-test-at-point)
  ;; One tab will be displayed as 4 spaces.
  (set 'tab-width 4))
  ;; Enable eldoc (displays function signatures in the minibuf).
;;  (go-eldoc-setup))
(add-hook 'go-mode-hook 'zkj-go-mode-hook)
(add-hook 'go-mode-hook 'eglot-ensure)
