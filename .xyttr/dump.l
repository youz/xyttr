;;; -*- mode:lisp; package: xyttr -*-

(in-package "xyttr")

(defun format-json (json str)
  (labels
      ((rec (data indent)
	 (cond ((consp data)
		(format str "{~%")
		(dolist (kv data)
		  (format str "  ~V@T~A: " (* indent 4) (car kv))
		  (if (consp (cdr kv))
		      (rec (cdr kv) (1+ indent))
		    (format str "~S~%" (cdr kv))))
		(format str "~V@T}~%" (* indent 4)))
	       (t (format str "~V@T~S~%") (* indent 4) data))))
    (rec json 0)))

(defun show-jsondata ()
  (interactive)
  (multiple-value-bind (s json) (entry-point)
    (w/popupbuf ("*tw:jsondata*")
      (format-json json t))))
