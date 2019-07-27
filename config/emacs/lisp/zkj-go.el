;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-go.el sets go-specific
;;;; configuration.
(provide 'zkj-go)

;; Use goimports instead of gofmt so that we get automatic imports.
(set 'gofmt-command "goimports")

(let ((path "~/go/src/github.com/stapelberg/expanderr/expanderr.el"))
  (if (file-exists-p path)
      (load path)))

(defun zkj-go-test-at-point ()
  (interactive)
  (let ((go-test-args "-count=1")
	(go-test-verbose t))
    (go-test-current-test)))

(defun zkj-find-definition ()
  (interactive)
  (lsp-find-definition)
  (recenter-top-bottom 0))

(setq lsp-prefer-flymake :none)

(defun zkj-go-mode-hook ()
  (lsp)
  ;; Prevent prompts about which project a file is in when opening standard
  ;; library files and files in the module cache.
  (add-to-list 'lsp-clients-go-library-directories "/home/michael/sdk")
  (add-to-list 'lsp-clients-go-library-directories "/home/michael/go/pkg/mod")
  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
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
