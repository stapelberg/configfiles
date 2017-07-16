;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-go.el sets go-specific
;;;; configuration.
(provide 'zkj-go)

;; Use goimports instead of gofmt so that we get automatic imports.
(set 'gofmt-command "goimports")

(defun zkj-go-mode-hook ()
  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Jump to the definition of the symbol under the cursor.
  ;; (Jump back with M-*)
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; One tab will be displayed as 4 spaces.
  (set 'tab-width 4)
  ;; Enable eldoc (displays function signatures in the minibuf).
  (go-eldoc-setup)
  ;; Initialize compile-command to “go run”.
  (set (make-local-variable 'compile-command)
       (format "go run %s" (file-name-nondirectory buffer-file-name))))
(add-hook 'go-mode-hook 'zkj-go-mode-hook)
