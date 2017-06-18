(provide 'zkj-live-reload)

(add-to-list
 'auto-mode-alist
 '("\\.asciidoc\\'" .
   (lambda ()
     ;; Enable livereload-mode if not already enabled
     (or livereload-mode	(livereload-mode))
     ;; Pretend we’re using http://localhost/ instead of a file:// URI,
     ;; and pretend asciidoc was HTML.
     (setq-local
      livereload-potential-targets
      (lambda (url)
	(let ((url (replace-regexp-in-string "^file://" "http://localhost/" url))
	      (buffer-file-name (replace-regexp-in-string "\.asciidoc$" ".html" buffer-file-name)))
	  (livereload-default-potential-targets url))))
     (add-hook
      'livereload-notify-hook
      (lambda (url)
	(let ((target (concat "file://" (replace-regexp-in-string "\.asciidoc$" ".html" buffer-file-name))))
	  (shell-command (concat "asciidoctor " buffer-file-name))
	  (livereload-notify (list target))
	  t))
      nil
      t))))
