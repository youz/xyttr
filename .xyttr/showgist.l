;;; -*- mode:lisp; package:xyttr -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "xml-http-request")
  (require "json"))

(in-package "xyttr")

(defun show-gist ()
  (interactive)
  (whenlet url (expand-focused-url)
    (whenlet no (and (string-match "https://gist\\.github\\.com/\\([0-9]+\\)\\(#.+\\)?$" url)
		     (match-string 1))
      (whenlet res (xhr:xhr-get (concat "https://api.github.com/gists/" no)
				:key #'xhr:xhr-response-text)
	(w/json (files user.login updated_at html_url) (json:json-decode res)
	  (when files
	    (w/popupbuf ((format nil "*gist:~A*" no) t)
	      (format t "# ~A~%# ~A [~A]~%" html_url (or user.login "anonymous") updated_at)
	      (dolist (f files)
		(w/json (filename content size) (cdr f)
		  (format t "# ~A [~A bytes]~%~40@{-~}~%~A~%~%" filename size content))))))))))
