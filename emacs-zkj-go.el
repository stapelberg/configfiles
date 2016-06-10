;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-go.el sets go-specific
;;;; configuration.
(provide 'zkj-go)

(global-set-key (kbd "C-c C-c") 'compile)

;; Run gofmt before saving (this is a global hook, but
;; gofmt-before-save checks if the buffer is in the Go major mode.
(add-hook 'before-save-hook #'gofmt-before-save)

;; Use goimports instead of gofmt so that we get automatic imports.
(set 'gofmt-command "goimports")
