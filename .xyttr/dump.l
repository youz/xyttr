;;; -*- mode:lisp; package: xyttr -*-

(in-package "xyttr")

(defun show-jsondata ()
  (interactive)
  (multiple-value-bind (s json) (entry-point)
    (w/popupbuf ("*tw:jsondata*")
      (write json)
      (lisp-mode)
      (while (scan-buffer ")+" :regexp t :tail t)
	(unless (eq (parse-point-syntax) :string)
	  (insert "\n")))
      (beginning-of-buffer)
      (indent-region (point-min) (point-max)))))
