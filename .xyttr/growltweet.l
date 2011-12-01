;;; -*- mode:lisp; package:xyttr -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "growl")) ; https://github.com/youz/xl-growl/

(in-package "xyttr")

(defun growl-tweet ()
  (interactive)
  (w/entry (user.screen_name user.profile_image_url text)
    (growl:notify (concat "@" user.screen_name) text
		  :name "xyttr"
		  :icon user.profile_image_url
		  :sticky t)))

