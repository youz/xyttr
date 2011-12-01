;;; -*- mode:lisp; package:xyttr -*-

(in-package "xyttr")

(defmacro w/popupbuf ((name &optional read-only) &body body)
  (w/uniq (km wc closer)
    `(let ((,km (make-sparse-keymap))
	   (,wc (current-window-configuration)))
       (labels ((,closer ()
		  (interactive)
		  (delete-buffer ,name)
		  (set-window-configuration ,wc)))
	 (define-key ,km #\q #',closer))
       (with-output-to-temp-buffer (,name t)
	 ,@body)
       (setq buffer-read-only ,read-only)
       (use-keymap ,km))))

(setf (get 'w/popupbuf 'ed::lisp-indent-hook) 1)

