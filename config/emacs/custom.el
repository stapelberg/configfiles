(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(message-header-to ((t (:foreground "green" :weight normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--case-sensitive" "--stats" "--follow"))
 '(clang-format-executable "clang-format")
 '(gdb-many-windows t)
 '(godoc-at-point-function 'godoc-gogetdoc)
 '(ledger-reports
   '(("accounts" "ledger ") ("equity" "ledger equity")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))

 '(notmuch-search-oldest-first nil)
 '(org-agenda-files '("~/NoName/rgb2r-orga/2019-rgb2r-orga-tasks.org"))
 '(send-mail-function 'sendmail-send-it)
 '(treesit-auto-install t))
