;;; -*- mode:lisp; package: xyttr -*-

(require 'ldrpin)
(require 'hatebu)
(require 'googl)

(in-package "xyttr")

(defvar *shorten-url-hosts*
  (format nil "\\(~{~A~^\\|~}\\)"
    '(bit.ly t.co j.mp htn.to amzn.to
      ow.ly goo.gl icio.us p.tl)))

(defun is-shorten? (url)
  (string-match *shorten-url-hosts* url))

(defun pin-focused-url ()
  (interactive)
  (whenlet url (focused-url)
    (when (is-shorten? url)
      (setq url (expand-short-url url)))
    (w/entry (user.screen_name text)
      (ldrpin:ldrpin-add url (format nil "@~A: ~A" user.screen_name text)))))

(defun show-hatebu-focused-url ()
  (interactive)
  (whenlet url (focused-url)
    (when (is-shorten? url)
      (setq url (expand-short-url url)))
    (hatebu:show-comments url)))

(defun show-hatebu-status ()
  (interactive)
  (w/entry (user.screen_name id)
    (let ((url (format nil "http://twitter.com/~A/status/~A" user.screen_name id)))
      (hatebu:show-comments url))))

(defun copy-status-url ()
  (interactive)
  (w/entry (user.screen_name id)
    (let ((url (format nil "http://twitter.com/~A/status/~A" user.screen_name id)))
      (copy-to-clipboard url)
      (message "Copied: ~A" url))))

(defun copy&popup-url ()
  (interactive)
  (multiple-value-bind (url beg) (focused-url)
    (when url
      (when (is-shorten? url)
	(setq url (expand-short-url url)))
      (copy-to-clipboard url)
      (message "Copied: ~A" url)
      (popup-string url beg))))

(defun copy-shorten-url ()
  (interactive)
  (whenlet url (focused-url)
    (let ((shorten (if (is-shorten? url) url
		     (googl:shorten url))))
      (copy-to-clipboard shorten)
      (message "Copied: ~A  (~A)" shorten url))))

(let ((m *xyttr-timeline-keymap*))
  (define-key m #\P 'pin-focused-url)
  (define-key m '(#\H #\l) 'show-hatebu-focused-url)
  (define-key m '(#\H #\s) 'show-hatebu-status)
  (define-key m '(#\c #\t) 'copy-status-url)
  (define-key m '(#\c #\u) 'copy&popup-url)
  (define-key m '(#\c #\s) 'copy-shorten-url)
  )

