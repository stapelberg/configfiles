;;;; © 2012 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-perl.el sets Perl-specific
;;;; configuration.

(provide 'zkj-perl)

(add-to-list 'load-path "~/.emacs.d/cperl-mode/")
(autoload 'cperl-mode "cperl-mode" "CPerl mode" t)

(defalias 'perl-mode 'cperl-mode)

;;;; auto-indent for perl
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'cperl-mode-hook 'set-newline-and-indent)

;;;; indentation settings
(setq
 cperl-continued-statement-offset 4
 cperl-indent-level 4
 cperl-indent-parens-as-block t
 cperl-tabs-always-indent t
 cperl-indent-subs-specially nil)

;;;; perltidy function from http://www.emacswiki.org/emacs/PerlTidyElisp
;;;; (fixed up according to (gofmt) wrt window configuration/point/mark)
(defun perltidy-dwim (arg)
  "Perltidy a region of the entire buffer"
  (interactive "P")
  (let ((old-point (point))
	(old-mark (mark t))
	(old-window-start (window-start))
	(old-window-hscroll (window-hscroll))
	(currconf (current-window-configuration))
	(buffer (generate-new-buffer "*perltidy*"))
	(start)
	(end))
    (if (and mark-active transient-mark-mode)
	(setq start (region-beginning)
	      end (region-end))
      (setq start (point-min)
	    end (point-max)))
    ;; set the PERLTIDY environment variable to the closest instance
    ;; of .perltidyrc, but keep its value if it was set before.
    (let ((old-perltidy-env (getenv "PERLTIDY")))
      (setenv "PERLTIDY" (or old-perltidy-env
			     (expand-file-name
			      (locate-dominating-file (buffer-file-name) ".perltidyrc"))))
      (shell-command-on-region start end "perltidy" buffer)
      (setenv "PERLTIDY" old-perltidy-env))
    (delete-region start end)
    (insert-buffer buffer)
    (kill-buffer buffer)
    (goto-char (min old-point (point-max)))
    (if old-mark (push-mark (min old-mark (point-max)) t))
    (set-window-configuration currconf)
    (set-window-start nil old-window-start)
    (set-window-hscroll nil old-window-hscroll)))

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c f") 'perltidy-dwim)))

;;;; function to run the testcases using "make test"
(defun zkj-perl-tests ()
  "Runs perl testcases"
  (interactive)
  (setq compilation-read-command nil)
  (setq compile-command "make test")
  (let ((default-directory (locate-dominating-file
			    (buffer-file-name)
			    "Makefile.PL")))
    (call-interactively 'compile)))

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c t") 'zkj-perl-tests)))
