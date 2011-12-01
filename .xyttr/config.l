;;; -*- mode:lisp; package: xyttr -*-

(in-package "xyttr")

(setq *default-user* "xyttr"
      *auto-reload* 600
      *favorite-star-style* '(:foreground 11))

(load-plugin "window")
(load-plugin "dump")
(load-plugin "urlutil")
(load-plugin "showgist")
(load-plugin "growltweet")

(defun njslyr ()
  (interactive)
  (select-pseudo-frame
   (or (find-pseudo-frame *frame-name*)
       (new-pseudo-frame *frame-name*)))
  (when (= (count-windows) 1)
    (split-window-vertically))
  (user::xyttr-user "NJSLYR")
  (other-window)
  (user::xyttr-search "#NJSLYR -RT"))

(let ((m *xyttr-timeline-keymap*))
  (define-key m '(#\c #\c) 'expand&copy-url)
  (define-key m '(#\c #\l) 'open-client-url)
  (define-key m #\T 'show-jsondata)
  (define-key m '(#\g #\i) 'show-gist)
  (define-key m '(#\g #\r) 'growl-tweet)
  (define-key m #\N 'njslyr)
  )

