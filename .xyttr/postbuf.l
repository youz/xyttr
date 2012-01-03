;;; -*- mode:lisp; package:xyttr -*-

;; post-buffer plugin
;; ref. http://d.hatena.ne.jp/youz/20111227/1325003463

(in-package :xyttr)

(defvar *post-buffer-keymap* (make-sparse-keymap))
(defvar *post-buffer-mode-hook* nil)
(defvar-local post-buffer-args nil)

(define-key *post-buffer-keymap* '(#\C-c #\C-c) 'post-from-buffer)
(define-key *post-buffer-keymap* '(#\C-x #\k) 'close-post-buffer)

(defun post-buffer-count-chars ()
  (let ((c (length (buffer-substring (point-min) (point-max))))
	(m (mark t)))
    (save-excursion
      (save-restriction
	(widen)
	(beginning-of-buffer)
	(replace-buffer "ï∂éöêî:[0-9]+" (format nil "ï∂éöêî:~D" c)
			:regexp t :once t)))
    (when m (set-mark m))))

(defun post-buffer-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'post-buffer-mode
	mode-name "xyttr-post-buffer"
	need-not-save t)
  (use-keymap *post-buffer-keymap*)
  (make-local-variable '#0=regexp-keyword-list)
  (setq #0# (append #0# (make-regexp-keyword-list)
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
      (format t "ìäçe:C-c C-c   ï¬Ç∂ÇÈ:C-x k  ï∂éöêî:0~%~@[~A~%~]" ref)
      (format t "~V@{-~}~%" (1- w) t))
    (narrow-to-region #0=(point-max) #0#)
    (when default
      (if cursor-top (save-excursion #1=(insert default)) #1#))
    (make-local-variable #2='ed:*before-delete-buffer-hook*)
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
	     (whenlet tlbuf (getf args :reload)
	       (timeline-reload tlbuf))
	     (message "Done."))
	   :onfailure
	   (lambda (res status headers)
	     (message "Failed: ~A ~A" status res)
	     (apply #'popup-post-buffer :default text :cursor-top t args)
	     (refresh-screen))
	   (getf args :params))
    (close-post-buffer)))


(defun tweet2 ()
  (interactive)
  (popup-post-buffer :name "*tweet*" :reload (selected-buffer)))

(defun mention2 ()
  (interactive)
  (w/entry (user.screen_name)
    (popup-post-buffer
     :name "*tweet*"
     :default (format nil "@~A " user.screen_name)
     :reload (selected-buffer))))

(defun tweet-with-quote2 ()
  (interactive)
  (w/entry (id user.screen_name text)
    (popup-post-buffer
     :name "*tweet*"
     :default (format nil *quote-format* user.screen_name text)
     :cursor-top t
     :reload (selected-buffer))))

(defun reply-to2 ()
  (interactive)
  (w/entry (id user.screen_name text)
    (popup-post-buffer
     :name "*reply-to*"
     :params (list :in_reply_to_status_id id)
     :ref (format nil "ï‘êMêÊ @~A: ~A" user.screen_name text)
     :default (format nil "@~A " user.screen_name)
     :reload (selected-buffer))))

(defun reply-with-quote2 ()
  (interactive)
  (w/entry (id user.screen_name text)
    (popup-post-buffer
     :name "*reply-to*"
     :params (list :in_reply_to_status_id id)
     :ref (format nil "ï‘êMêÊ @~A: ~A" user.screen_name text)
     :default (format nil " RT @~A: ~A" user.screen_name text)
     :cursor-top t
     :reload (selected-buffer))))

(defun send-message2 ()
  (interactive)
  (w/entry (user.id user.screen_name)
    (popup-post-buffer
     :name "*DM*"
     :params `(:user ,user.screen_name)
     :api #'api-direct-messages-new-async
     :status-field :text
     :ref (format nil "DirectMessage to @~A" user.screen_name))))


(let ((m *xyttr-timeline-keymap*))
  (define-key m #\u 'tweet2)
  (define-key m #\@ 'mention2)
  (define-key m #\` 'reply-to2)
  (define-key m '(#\r #\u) 'tweet-with-quote2)
  (define-key m '(#\r #\r) 'reply-with-quote2)
  (define-key m '(#\d #\m) 'send-message2)
  )
