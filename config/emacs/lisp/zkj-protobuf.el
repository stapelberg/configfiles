(provide 'zkj-protobuf)

;; NOTE: (setq jit-lock-defer-time 0.01) makes the distri build of
;; e.g. networkmanager hang.

(add-to-list
 'auto-mode-alist
 '("\\.textproto\\'" .
   (lambda ()
     ;; Enable protobuf mode.
     (protobuf-mode)
     (setq-local compile-command "distri <&-")
     ;; Compile without colors (faster):
     (add-hook 'compilation-mode-hook
	       (lambda()
		 ;; TODO: scope setting to only this compilation, not all
		 ;; compilations.
		 (font-lock-mode -1)))
     )))
