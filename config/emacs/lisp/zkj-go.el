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

(defun zkj-go-mode-hook ()
  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Jump to the definition of the symbol under the cursor.
  ;; (Jump back with M-*)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c d") 'godoc-at-point)
  (local-set-key (kbd "M-g t") 'zkj-go-test-at-point)
  ;; One tab will be displayed as 4 spaces.
  (set 'tab-width 4))
  ;; Enable eldoc (displays function signatures in the minibuf).
;;  (go-eldoc-setup))
(add-hook 'go-mode-hook 'zkj-go-mode-hook)
