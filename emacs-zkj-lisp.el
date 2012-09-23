;;;; © 2012 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-lisp.el sets LISP/SCHEME specific
;;;; configuration.

(provide 'zkj-lisp)

;;;; auto-indent for lisp modes
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'emacs-lisp-mode-hook 'set-newline-and-indent)
(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'scheme-mode-hook 'set-newline-and-indent)

;;;; chicken-slime
(add-to-list 'load-path "/var/lib/chicken/6/")
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)

(add-hook 'scheme-mode-hook
  (lambda ()
    (require 'slime)
    (slime-setup '(slime-fancy))
    (slime-mode t)))
