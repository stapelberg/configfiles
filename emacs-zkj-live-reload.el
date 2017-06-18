(provide 'zkj-live-reload)

;; Pretend we’re using http://localhost/ instead of a file:// URI,
;; and pretend asciidoc was HTML.
;;
;; We should use a lambda, but livereload.el uses symbolp to check whether it
;; should call, so we need a symbol.
(defun zkj-livereload-potential-targets (url)
  (let ((url (replace-regexp-in-string "^file://" "http://localhost/" url))
	 (buffer-file-name (replace-regexp-in-string "\.asciidoc$" ".html" buffer-file-name)))
    (livereload-default-potential-targets url)))

(defun zkj-livereload-asciidoc-notify (url)
  (let ((target (concat "file://" (replace-regexp-in-string "\.asciidoc$" ".html" buffer-file-name))))
    (shell-command (concat "asciidoctor " buffer-file-name))
    (livereload-notify (list target))
    t))

(add-to-list 'auto-mode-alist
	     '("\\.asciidoc\\'" . (lambda ()
				    ;; Enable livereload-mode if not already enabled
				    (or livereload-mode	(livereload-mode))
				    (make-local-variable livereload-potential-targets)
				    (setq livereload-potential-targets 'zkj-livereload-potential-targets)
				    (add-hook 'livereload-notify-hook 'zkj-livereload-asciidoc-notify nil t))))
