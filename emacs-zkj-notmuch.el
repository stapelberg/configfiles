;;;; © 2012-2013 Michael Stapelberg (BSD-licensed)
;;;;
;;;; ~/.emacs.d/lisp/zkj-notmuch.el sets notmuch-specific
;;;; configuration.

(provide 'zkj-notmuch)

(require 'notmuch)
(require 'notmuch-address)

;; If this is not midna, we run notmuch on midna via remote-notmuch.sh
;; Temporarily commented out: mail is on x200 primarily for the time being.
;; (when (not (string= system-name "midna.zekjur.net"))
;;   (setq notmuch-command "/home/michael/configfiles/remote-notmuch.sh"))

;; Process PGP/MIME. Needs gpg-agent working, with pinentry-gtk.
(setq notmuch-crypto-process-mime t)

;; When stumbling upon an inline GPG message, call
;; notmuch-decrypt-inline and tell the sender to switch to PGP/MIME.
(defun notmuch-decrypt-inline ()
  (interactive)
  (epa-decrypt-armor-in-region (point-min) (point-max)))

;; Close mail buffers after sending.
(setq message-kill-buffer-on-exit t)

;; Don’t display killed threads in my inbox.
(setq notmuch-saved-searches (quote (("inbox" . "tag:inbox -tag:killed"))))

;; When starting, don’t load notmuch-hello, but jump directly to inbox.
(defun notmuch ()
  (interactive)
  (notmuch-search (cdr (first notmuch-saved-searches))))

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
;;(setq message-sendmail-envelope-from "stapelberg@debian.org")
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
(setq notmuch-address-command "/home/michael/notmuch/nottoomuch-addresses.sh")
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
      (call-process "git" nil buf nil "am" "-3" "--whitespace=fix" tempfile))))

(define-key notmuch-show-mode-map "4"
  (lambda ()
    "Merge this patch into ~/i3status"
    (interactive)
    (let ((id (notmuch-show-get-message-id))
	  (tempfile (make-temp-file "i3status-patch"))
	  (buf (get-buffer-create "*notmuch-last-i3status-merge*")))
      (switch-to-buffer buf)
      (cd "/home/michael/i3status")
      (with-temp-file tempfile
	(call-process notmuch-command nil t nil "show" "--format=raw" id))
      (call-process "git" nil buf nil "am" "-3" "--whitespace=fix" tempfile))))

(define-key notmuch-search-mode-map "S"
  (lambda ()
    "mark messages in thread as spam"
    (interactive)
    (if (member "is-spam" (notmuch-search-get-tags))
	(notmuch-search-tag '("-is-spam" "+inbox"))
      (notmuch-search-tag '("+is-spam" "-inbox")))))

;; Removes submit@bugs.debian.org from the recipients of a reply-all message.
(defun debian-remove-submit (recipients)
  (delq nil
	(mapcar (lambda (recipient)
		  (and (not (string-equal (nth 1 recipient) "submit@bugs.debian.org"))
		       recipient))
		recipients)))

(defun debian-add-bugrecipient (recipients bugnumber)
  (let* ((bugstr (format "%s" bugnumber))
	 (bugaddress (concat bugstr "@bugs.debian.org"))
	 (addresses (mapcar (lambda (x) (nth 1 x)) recipients))
	 (exists (member bugaddress addresses)))
    (if exists
	recipients
      (append (list (list (concat "Bug " bugstr) bugaddress)) recipients))))

;; TODO: msg should be made optional and it should default to the latest message in the bugreport.
;; NB: bugnumber and msg are both strings.
(defun debian-bts-reply (bugnumber msg)
  ;; Download the message to ~/mail-copy-fs/imported.
  (let ((msgpath (format "~/mail-copy-fs/imported/bts_%s_msg_%s.msg" bugnumber msg)))
    (let* ((url (format "http://bugs.debian.org/cgi-bin/bugreport.cgi?msg=%s;mbox=yes;bug=%s" msg bugnumber))
	   (download-buffer (url-retrieve-synchronously url)))
      (save-excursion
	(set-buffer download-buffer)
	(goto-char (point-min)) ; just to be safe
	(if (not (string-equal
		  (buffer-substring (point) (line-end-position))
		  "HTTP/1.1 200 OK"))
	    (error "Could not download the message from the Debian BTS"))
	;; Delete the HTTP headers and the first "From" line (in order to
	;; make this a message, not an mbox).
	(re-search-forward "^$" nil 'move)
	(forward-char)
	(forward-line 1)
	(delete-region (point-min) (point))
	;; Store the message on disk.
	(write-file msgpath)
	(kill-buffer)))
    ;; Import the mail into the notmuch database.
    (let ((msgid (with-temp-buffer
		   (call-process "~/.local/bin/notmuch-import.py" nil t nil (expand-file-name msgpath))
		   (buffer-string))))
      (notmuch-mua-reply (concat "id:" msgid) "Michael Stapelberg <stapelberg@debian.org>" t)
      ;; Remove submit@bugs.debian.org, add <bugnumber>@bugs.debian.org.
      (let* ((to (message-fetch-field "To"))
	     (recipients (mail-extract-address-components to t))
	     (recipients (debian-remove-submit recipients))
	     (recipients (debian-add-bugrecipient recipients bugnumber))
	     (recipients-str (mapconcat (lambda (x) (concat (nth 0 x) " <" (nth 1 x) ">")) recipients ", ")))
	(save-excursion
	  (message-goto-to)
	  (message-delete-line)
	  (insert "To: " recipients-str "\n")))
      ;; Our modifications don’t count as modifications.
      (set-buffer-modified-p nil))))
