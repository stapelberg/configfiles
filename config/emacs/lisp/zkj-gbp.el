(provide 'zkj-gbp)

(defun outdir ()
    (replace-regexp-in-string
     (regexp-quote (expand-file-name "~/d/pkg"))
     (expand-file-name "~/d/out")
     default-directory))

(defun gbp-buildpackage ()
  (interactive)
  (let ((out (outdir))
	(default-directory (magit-toplevel)))
    (compile (concat "gbp buildpackage --git-ignore-new --git-export-dir=" out))))

;; TODO: team flag
(defun gbp-dch ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (compile "gbp dch -R --commit")))

(defun gbp-tag ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (compile "gbp buildpackage --git-tag-only --git-ignore-new")))

(defun gbp-push ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (compile "gbp push")))

(defun gbp-pull ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (compile "gbp pull")))

(defun dput ()
  (interactive)
  (let ((default-directory (outdir)))
    (compile "dput" t))) ;; run compile in COMINT mode to allow input

(use-package magit-popup
  :config
  (magit-define-popup gbp-popup "git buildpackage"
    :actions '((?B "buildpackage" gbp-buildpackage)
	       (?d "dch" gbp-dch)
	       (?t "tag" gbp-tag)
	       (?F "pull" gbp-pull)
	       (?P "push" gbp-push)
	       (?U "dput" dput))
    :default-action 'gbp
    ))

(define-key magit-status-mode-map (kbd "H") 'gbp-popup)

(magit-define-popup-action 'magit-dispatch-popup
  ?H "Git buildpackage" 'gbp-popup ?!)
