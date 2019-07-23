(provide 'zkj-protobuf)

(add-to-list
 'auto-mode-alist
 '("\\.textproto\\'" .
   (lambda ()
     ;; Enable protobuf mode.
     (protobuf-mode)
     ;; Pretend we’re using http://localhost/ instead of a file:// URI,
     ;; and pretend asciidoc was HTML.
     (setq-local compile-command "distri")
     )))
