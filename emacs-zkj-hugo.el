(provide 'zkj-hugo)

(defun hugo-replace-key (key val)
      (save-excursion
        (goto-char (point-min))
        ; quoted value
        (if (and (re-search-forward (concat key ": \"") nil t)
                   (re-search-forward "[^\"]+" (line-end-position) t))
            (or (replace-match val) t) ; ensure we return t
          ; unquoted value
          (when (and (re-search-forward (concat key ": ") nil t)
                     (re-search-forward ".+" (line-end-position) t))
            (or (replace-match val) t)))))

(defun hugo-enable-draft ()
  (hugo-replace-key "draft" "true"))

(add-hook 'markdown-mode-hook 'hugo-enable-draft)
