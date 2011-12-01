;;; -*- mode:lisp; package:xyttr -*-

(in-package "xyttr")

(defun expand&copy-url ()
  (interactive)
  (let ((url (expand-focused-url)))
    (copy-to-clipboard url)
    (message "Copied: ~A" url)))

(defun open-client-url ()
  (interactive)
  (w/entry (source)
    (whenlet url (and (string-match "<a href=\"\\([^\\\"]+\\)\"" source)
		      (match-string 1))
      (shell-execute url t))))
