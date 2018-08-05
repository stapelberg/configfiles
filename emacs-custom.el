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
 '(ag-arguments (quote ("--case-sensitive" "--stats" "--follow")))
 '(clang-format-executable "clang-format-3.8")
 '(godoc-at-point-function (quote godoc-gogetdoc))
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-untracked-files magit-insert-unpulled-from-upstream magit-insert-unpulled-from-pushremote magit-insert-unpushed-to-upstream magit-insert-unpushed-to-pushremote)))
 '(package-selected-packages
   (quote
    (deadgrep bind-key go-mode eieio xelb diff-hl better-shell ssh go-rename editorconfig clang-format smex livereload websocket protobuf-mode magit gotest go-guru go-eldoc crontab-mode ag)))
 '(smex-history-length 50))
