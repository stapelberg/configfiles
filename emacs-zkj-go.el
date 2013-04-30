(global-set-key (kbd "C-c C-c") 'compile)

;; Run gofmt before saving (this is a global hook, but
;; gofmt-before-save checks if the buffer is in the Go major mode.
(add-hook 'before-save-hook #'gofmt-before-save)
