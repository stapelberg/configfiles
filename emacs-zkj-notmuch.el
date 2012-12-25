;;;; © 2012 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-notmuch.el sets notmuch-specific
;;;; configuration.

(provide 'zkj-notmuch)

(require 'notmuch)
(require 'notmuch-address)

;; Close mail buffers after sending.
(setq message-kill-buffer-on-exit t)

;; Don’t display killed threads in my inbox.
(setq notmuch-saved-searches (quote (("inbox" . "tag:inbox -tag:killed"))))

;; Use sendmail(1) to send emails.
(setq message-send-mail-function (quote message-send-mail-with-sendmail))
(setq send-mail-function (quote sendmail-send-it))

;; The default breaks some notmuch features (for example 'v' for view
;; message or 'w' for save all attachments), so use w3m to render
;; text/html MIME parts.
(setq mm-text-html-renderer 'shr)
(add-to-list 'mm-text-html-renderer-alist
	     '(true-w3m mm-inline-render-with-stdin
			nil "w3m" "-T" "text/html"))


;; Required to make mail routing work in my setup
(setq message-sendmail-envelope-from "michael@stapelberg.de")
;;(setq message-sendmail-envelope-from "p5p@zekjur.net")

;; My folder is called "Sent", not "sent".
(setq notmuch-fcc-dirs "Sent")

;; Remove messages from inbox to which I replied.
(setq notmuch-message-replied-tags '("replied" "-inbox"))

;; Show newest messages at the top.
(setq notmuch-search-oldest-first nil)

;; Hide text/html parts in multipart-messages by default.
(setq notmuch-show-all-multipart/alternative-parts nil)

;; Make the search result lines look a bit more like sup.
(setq notmuch-search-result-format
      `(("date" . "%12s ")
	("count" . "%-7s ")
	("authors" . "%-15s ")
	("tags" . "%s ")
	("subject" . "%s ")))

;; Address completion.
(setq notmuch-address-command "/home/michael/Downloads/nottoomuch-addresses.sh")
(notmuch-address-message-insinuate)

;;;; Mail body generation

;;; Returns everything before the first space (if any), or just the
;;; whole name if there are no spaces (most likely a nickname, then).
(defun shorten-realname (name)
  (if (stringp name)
      (if (string-match "\\([^ ]+\\)" name)
	  (match-string 0 name)
	name)
    ""))

(defun get-recipient-for-body ()
  (let ((to (message-fetch-field "To")))
    (if to	
	(shorten-realname (nth 0 (mail-extract-address-components to)))
      "")))

(defun my-fill-body ()
  (let ((recipient (get-recipient-for-body)))
    (save-excursion
      (goto-char (point-max))
      (insert (format "Hi %s,\n\n" recipient)))))

(add-hook 'message-signature-setup-hook 'my-fill-body)

;;;; Automatic sender address selection.

;;; returns a list of all values of the given plist
(defun plist-values (plist)
  (if (null plist)
      nil
    (append (list (nth 1 plist)) (plist-values (cdr (cdr plist))))))

;;; returns nil if PATTERN is not contained in any member of HEADERS
(defun any-value-matches (pattern headers)
  (if (null headers)
      nil
    (or
     (string-match pattern (car headers))
     (any-value-matches pattern (cdr headers)))))

(defun determine-sender-and-reply (reply-all)
  (let* ((headers (notmuch-show-get-prop :headers nil))
	 (values (plist-values headers))
	 (sender (cond
		  ((any-value-matches "debian\\.org" values) "Michael Stapelberg <stapelberg@debian.org>")
		  ((any-value-matches "i3" values) "Michael Stapelberg <michael@i3wm.org>")
		  (t nil))))
    (notmuch-mua-reply (notmuch-show-get-message-id) sender reply-all)))

(define-key notmuch-show-mode-map "r"
  (lambda ()
    (interactive)
    (determine-sender-and-reply nil)))

(define-key notmuch-show-mode-map "R"
  (lambda ()
    (interactive)
    (determine-sender-and-reply t)))

(define-key notmuch-show-mode-map "3"
  (lambda ()
    "Merge this patch into ~/i3"
    (interactive)
    (let ((id (notmuch-show-get-message-id))
	  (tempfile (make-temp-file "i3-patch"))
	  (buf (get-buffer-create "*notmuch-last-i3-merge*")))
      (switch-to-buffer buf)
      (cd "/home/michael/i3")
      (with-temp-file tempfile
	(call-process notmuch-command nil t nil "show" "--format=raw" id))
      (call-process "git" nil buf nil "am" "-3" tempfile))))

(define-key notmuch-search-mode-map "S"
  (lambda ()
    "mark messages in thread as spam"
    (interactive)
    (if (member "is-spam" (notmuch-search-get-tags))
	(notmuch-search-tag '("-is-spam" "+inbox"))
      (notmuch-search-tag '("+is-spam" "-inbox")))))
