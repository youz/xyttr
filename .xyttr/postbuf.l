;;; -*- mode:lisp; package:xyttr.post-buffer -*-

;; post-buffer plugin
;; ref. http://d.hatena.ne.jp/youz/20111227/1325003463

(defpackage :xyttr.post-buffer
  (:use :lisp :ed :xyttr)
  (:import-from :xyttr
   "w/json" "w/entry" "whenlet" "timeline-reload"))

(in-package :xyttr.post-buffer)

(export '(setup-keymap
	  popup-tweet-buffer
	  popup-mention-buffer
	  popup-reply-to-buffer
	  popup-tweet-with-quote-buffer
	  popup-reply-with-quote-buffer
	  popup-send-message-buffer))

(defvar *post-buffer-keymap* (make-sparse-keymap))
(defvar *post-buffer-mode-hook* nil)
(defvar-local post-buffer-args nil)

(define-key *post-buffer-keymap* '(#\C-c #\C-c) 'post-from-buffer)
(define-key *post-buffer-keymap* '(#\C-x #\k) 'close-post-buffer)

(defun post-buffer-count-chars ()
  (let ((c (length (buffer-substring (point-min) (point-max)))))
    (setq mode-line-format
	  (format nil "~A (~A) écÇË ~D ï∂éö"
		  (getf post-buffer-args :name)
		  mode-name (- 140 c)))))

(defun post-buffer-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'post-buffer-mode
	mode-name "xyttr-post-buffer"
	need-not-save t
	kept-undo-information 1000)
  (make-local-variable 'mode-line-format)
  (use-keymap *post-buffer-keymap*)
  (make-local-variable '#0=regexp-keyword-list)
  (setq #0# (append #0# (xyttr::make-regexp-keyword-list)
		    (compile-regexp-keyword-list
		     '(("^-\\{20,\\}" nil (:color 14))))))
  (make-local-variable 'ed:*post-command-hook*)
  (set-buffer-fold-type-window)
  (add-hook 'ed:*post-command-hook* 'post-buffer-count-chars)
  (run-hooks '*post-buffer-mode-hook*))


(defun popup-post-buffer (&key name params default
			       reload ref cursor-top
			       (api #'api-update-async)
			       (status-field :status))
  (let ((buf (get-buffer-create name))
	(wc (current-window-configuration))
	(w (window-columns)))
    (split-window -8 nil)
    (set-buffer buf)
    (erase-buffer buf)
    (post-buffer-mode)
    (setq post-buffer-args
	  (list :name name :params params :reload reload :ref ref
		:api api :status-field status-field))
    (with-output-to-buffer (buf)
      (format t "ìäçe:C-c C-c   ï¬Ç∂ÇÈ:C-x k~%~@[~A~%~]" ref)
      (format t "~V@{-~}~%" (1- w) t))
    (narrow-to-region #0=(point-max) #0#)
    (when default
      (if cursor-top (save-excursion #1=(insert default)) #1#))
    (make-local-variable #2='ed:*before-delete-buffer-hook*)
    (post-buffer-count-chars)
    (flet ((restore-wc (buf)
	     (ignore-errors (set-window-configuration wc)) t))
      (add-hook #2# #'restore-wc))))

(defun close-post-buffer ()
  (interactive)
  (when (eq buffer-mode 'post-buffer-mode)
    (delete-buffer (selected-buffer))))

(defun post-from-buffer ()
  (interactive)
  (let ((text (buffer-substring (point-min) (point-max)))
	(args post-buffer-args))
    (apply (getf post-buffer-args :api)
	   (getf post-buffer-args :status-field) text
	   :onsuccess
	   (lambda (res)
	     (xyttr::whenlet tlbuf (getf args :reload)
	       (start-timer 1 (lambda () (timeline-reload tlbuf)) t))
	     (message "Done."))
	   :onfailure
	   (lambda (res status headers)
	     (message "Failed: ~A ~A" status res)
	     (apply #'popup-post-buffer :default text :cursor-top t args)
	     (refresh-screen))
	   (getf args :params))
    (close-post-buffer)))


(defun popup-tweet-buffer ()
  (interactive)
  (popup-post-buffer :name "*tweet*" :reload (selected-buffer)))

(defun popup-mention-buffer ()
  (interactive)
  (w/entry (user.screen_name)
    (popup-post-buffer
     :name "*tweet*"
     :default (format nil "@~A " user.screen_name)
     :reload (selected-buffer))))

(defun popup-tweet-with-quote-buffer ()
  (interactive)
  (w/entry (id user.screen_name text)
    (popup-post-buffer
     :name "*tweet*"
     :default (format nil *quote-format* user.screen_name text)
     :cursor-top t
     :reload (selected-buffer))))

(defun popup-reply-to-buffer ()
  (interactive)
  (w/entry (id user.screen_name text)
    (popup-post-buffer
     :name "*reply-to*"
     :params (list :in_reply_to_status_id id)
     :ref (format nil "ï‘êMêÊ @~A: ~A" user.screen_name text)
     :default (format nil "@~A " user.screen_name)
     :reload (selected-buffer))))

(defun popup-reply-with-quote-buffer ()
  (interactive)
  (w/entry (id user.screen_name text)
    (popup-post-buffer
     :name "*reply-to*"
     :params (list :in_reply_to_status_id id)
     :ref (format nil "ï‘êMêÊ @~A: ~A" user.screen_name text)
     :default (format nil " RT @~A: ~A" user.screen_name text)
     :cursor-top t
     :reload (selected-buffer))))

(defun popup-send-message-buffer ()
  (interactive)
  (w/entry (user.id user.screen_name sender.id sender.screen_name)
    (let* ((name (or sender.screen_name user.screen_name))
	   (id (or sender.id user.id)))
      (popup-post-buffer
       :name "*DM*"
       :params (list :user_id id)
       :api #'api-direct-messages-new-async
       :status-field :text
       :ref (format nil "DirectMessage to @~A" name)
       :reload (selected-buffer)))))

(defun setup-keymap ()
  (interactive)
  (let ((m *xyttr-timeline-keymap*))
    (define-key m #\u 'popup-tweet-buffer)
    (define-key m #\@ 'popup-mention-buffer)
    (define-key m #\` 'popup-reply-to-buffer)
    (define-key m '(#\r #\u) 'popup-tweet-with-quote-buffer)
    (define-key m '(#\r #\r) 'popup-reply-with-quote-buffer)
    (define-key m '(#\d #\m) 'popup-send-message-buffer)
    ))

(setup-keymap)
