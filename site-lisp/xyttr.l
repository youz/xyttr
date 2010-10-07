;;; -*- mode:Lisp; package:xyttr -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'xml-http-request)
  (require 'json)
  (require 'oauth))

(defpackage :xyttr
  (:use :lisp :editor))


(in-package "xyttr")
(export '(*xyttr-timeline-keymap*
	  *auto-reload*
	  *open-url-by*
	  *search-lang*
	  *quote-format*))


(defvar *api-url* "http://api.twitter.com")
(defvar *search-url* "http://search.twitter.com")
(defvar *X-Twitter-Client* "xyttr")
(defvar *X-Twitter-Client-URL* "http://github.com/youz/xyttr")
(defvar *X-Twitter-Client-Version* "0.1.0")

(defvar *consumer-key*    "9tO9vX1QgwWPqoPNnhcMw")
(defvar *consumer-secret* "asn1tSaaQbs1wCm5gOVUoNdfHPGO4j3WuFKfsNKW8I")
(defvar *token* nil)
(defvar *token-secret* nil)

(defvar *auto-reload* nil
  "auto-reload interval (in seconds)")
(defvar *open-url-by* nil)
(defvar *search-lang* "ja")
(defvar *quote-format* " RT @~A: ~A")

;(defvar *count/page* 50)
;(defvar *page-max* 4)

(defvar *xyttr-timeline-keymap* (make-sparse-keymap))
;(set-extended-key-translate-table exkey-S-tab #\S-F20)

(let ((m *xyttr-timeline-keymap*))
  (define-key m #\C 'copy-status-url)
  (define-key m #\D 'destroy-status)
  (define-key m #\h 'previous-link)
  (define-key m #\j 'forward-entry)
  (define-key m #\k 'backward-entry)
  (define-key m #\l 'next-link)
  (define-key m #\M-l 'recenter)
  (define-key m #\M 'user::xyttr-mentions)
  (define-key m #\Q '(lambda () (interactive) (kill-buffer (selected-buffer))))
  (define-key m #\R 'reload)
  (define-key m #\M-r 'toggle-auto-reload)
  (define-key m '(#\r #\o) 'retweet)
  (define-key m '(#\r #\t) 'tweet-with-quote)
  (define-key m '(#\r #\r) 'reply-with-quote)
  (define-key m #\t 'test)
  (define-key m #\u 'tweet)
  (define-key m #\@ 'mention)
  (define-key m #\` 'reply-to)
  (define-key m #\/ 'user::xyttr-search)
  (define-key m #\RET 'open-link)
  (define-key m #\TAB 'next-link)
  (define-key m #\M-Down 'next-link)
  (define-key m #\M-Up 'previous-link)
)

(defvar *url-regexp* "http://[-0-9A-Za-z_.!~*'();/?:@&=+$,%#|]+")
(defvar *hashtag-regexp* "#\\([_a-zA-Z0-9]+\\)")
(defvar *user-regexp* "@\\([_a-zA-Z0-9]+\\)")

(defvar *regexp-keyword-list*
  (compile-regexp-keyword-list
   `((,*hashtag-regexp* t (:keyword 1))
     (,*user-regexp* t (:keyword 0)))))

(defstruct apiconf
  apifunc auto-reload params)



;;; local-variables
(defvar-local reload-conf nil)
(defvar-local unread-count 0)



;;; utility
(defun symb (&rest args)
  (values (intern (format nil "~{~A~}" args))))

(defmacro w/uniq (names &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (make-symbol ,(symbol-name s))))
		 (if (consp names) names (list names)))
     ,@body))

(defmacro whenlet (var expr &body body)
  `(let ((,var ,expr)) (when ,var ,@body)))

(defmacro json-value (obj key)
  (reduce #'(lambda (o k) `(cdr (assoc ,k ,o :test #'string=)))
	  (split-string (symbol-name key) #\.)
	  :initial-value obj))

(defmacro w/json (keys obj &body body)
  (w/uniq (gobj)
    `(let* ((,gobj ,obj)
	    ,@(mapcar #'(lambda (k) `(,k (json-value ,gobj ,k))) keys))
       ,@body)))

(defun entity2char (str)
  (reduce #'(lambda (s pair) (apply #'substitute-string s pair))
	  '(("&amp;" "&") ("&gt;" ">") ("&lt;" "<") ("&quot;" "\""))
	  :initial-value str))

(defun parse-date (str)
  (let* ((l (split-string str #\SPC t #\,))
	 (m '("Jan""Feb""Mar""Apr""May""Jun""Jul""Aug""Sep""Oct""Nov""Dec"))
	 (ut (ed:parse-date-string
	      (format nil "~A/~A/~A ~A" (nth 5 l)
		(1+ (position (nth 1 l) m :test #'string=))
		(nth 2 l) (nth 3 l))))
	 (tz (+ (parse-integer (subseq #1=(nth 4 l) 2 3))
		(/ (parse-integer (subseq #1# 3)) 60))))
    (+ ut (* (- 9 tz) 3600))))

(defun expand-short-url (url)
  (if (not (string-match "http://\\([^/]+\\)/\\([^/]+\\)$" url))
      url
    (let ((host (match-string 1))
	  (path (match-string 2)))
      (with-open-stream (cn (connect host 80))
	(format cn "GET /~A HTTP/1.1\nHost: ~A\n\n" path host)
	(let* ((res (read-line cn nil))
	       (m (string-match "^HTTP/[0-9.]+ \\([0-9]+\\) .+$" res))
	       (status (match-string 1)))
	  (if (not (and m (find status '("301" "302") :test #'string=)))
	      (values url res)
	    (loop
	      for buf = (read-line cn nil)
	      when (string-match "^Location: \\(.+\\)$" buf)
	      do (return (match-string 1))
	      while buf)))))))


;;; auth
(defun load-token (user)
  (let ((tokenfile (concat "~/.xyttr/token_" user)))
    (if (file-exist-p tokenfile)
	(with-open-file (is tokenfile :direction :input)
	  (setq *token* (read-line is nil)
		*token-secret* (read-line is nil))
	  t))))

(defun get-token (&optional tokenfile)
  (multiple-value-bind (token token-secret res)
      (oauth:get-access-token
       *consumer-key* *consumer-secret*
       (concat *api-url* "/oauth/request_token")
       (concat *api-url* "/oauth/authorize")
       (concat *api-url* "/oauth/access_token"))
    (when (and token token-secret)
      (setq *token* token *token-secret* token-secret)
      (when save
	(let ((screen-name (and (string-match "screen_name=\\([^&]+\\)" res)
				(match-string 1))))
	  (unless (or tokenfile screen-name)
	    (setq screen-name (read-string "Save Username: ")))
	  (unless (file-directory-p "~/.xyttr")
	    (create-directory "~/.xyttr"))
	  (with-open-file (os (or tokenfile (concat "~/.xyttr/token_" screen-name))
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	    (format os "~A~%~A~%" token token-secret))
	  (message "token saved: ~A" tokenfile)))
      t)))



;;; api
(defun api-request (method host path params &key raw)
  (let* ((url (concat host path))
	 (cred (list :consumer-key *consumer-key*
		     :consumer-secret *consumer-secret*
		     :token *token*
		     :token-secret *token-secret*))
	 (auth (oauth:auth-header cred method url params)))
    (multiple-value-bind (res status header)
	(if (eq method 'get)
	    (xhr:xhr-get url :query params .
			 #1=(:headers `(:Authorization ,auth)
			     :encoding *encoding-utf8n*
			     :key #'xhr:xhr-response-values))
	  (xhr:xhr-request method url params . #1#))
      (if (= status 200)
	  (if raw res
	    (json:json-decode res))
	(message "xyttr::request faild: ~A" status)))))

#|
(defun api-request-async (method host path params callback)
  )
|#

(defmacro define-api (name &key (auth t) (method 'get) (apiurl '*api-url*)
			   path params key raw)
  (unless path (error "define-api: missing 'path'"))
  `(progn
     (defun ,(symb "api-" name) (&key ,@params)
       (funcall
	,(or key '#'identity)
	(funcall #'api-request ',method ,apiurl ,path
		 (append
		  ,@(loop for p in params
		      for n = (symbol-name p)
		      collect
		      `(if ,#1=(intern n)
			 ,(list 'list (intern n "keyword") #1#))))
		 ,@(when raw (list :raw t))
		 )))
     ))


(define-api public-timeline
  :auth nil
  :path   "/1/statuses/public_timeline.json"
  :params (trim_user include_entities))

(define-api home-timeline
  :path   "/1/statuses/home_timeline.json"
  :params (since_id max_id count page trim_user include_entities))

(define-api user-timeline
  :path   "/1/statuses/user_timeline.json"
  :params (user_id screen_name
	   since_id max_id count page trim_user include_entities))

(define-api mentions
  :path "/1/statuses/mentions.json"
  :params (since_id max_id count page trim_user include_rts include_entities))

(define-api retweeted-by-me
  :path "/1/statuses/retweeted_by_me.json"
  :params (since_id max_id count page trim_user include_entities))

(define-api retweeted-to-me
  :path "/1/statuses/retweeted_to_me.json"
  :params (since_id max_id count page trim_user include_entities))

(define-api retweets-of-me
  :path "/1/statuses/retweets_of_me.json"
  :params (since_id max_id count page trim_user include_entities))

(define-api show-status
  :params (id)
  :path   (format nil "/1/statuses/show/~D.json" id))

(define-api update
  :method post
  :path   "/1/statuses/update.json"
  :params (status in_reply_to_status_id
	   lat long place_id display_coordinates))

(define-api destroy
  :method post
  :params (id)
  :path   (format nil "/1/statuses/destroy/~D.json" id))

(define-api retweet
  :method post
  :params (id)
  :path   (format nil "/1/statuses/retweet/~D.json" id))

(define-api retweets
  :method post
  :params (id count)
  :path   (format nil "/1/statuses/retweets/~D.json" id))

(define-api retweeted_by
  :params (id count page)
  :path   (format nil "/1/statuses/~D/retweeted_by.json" id))

(define-api retweeted_by/ids
  :params (id count page)
  :path   (format nil "/1/statuses/~D/retweeted_by/ids.json" id))


(define-api favorites
  :params (id page)
  :path   "/1/statuses/favorites.json")

(define-api favorites-create
  :params (id)
  :path   (format nil "/1/favorites/create/~D.json" id))

(define-api favorites-destroy
  :params (id)
  :path   (format nil "/1/favorites/destroy/~D.json" id))

(define-api test
  :params ()
  :path   "/1/help/test.json"
  :raw t)


;;; search api
(define-api search
  :apiurl *search-url*
  :path "/search.json"
  :params (q lang rpp page max_id since_id since #+:nil until
	   geocode show_ser result_type)
  :key #'search-result-to-statuses)

(defun search-result-to-statuses (res)
  (labels
      ((fixdate (d)
	 (let ((elms (split-string (remove #\, d) #\SPC)))
	   (format nil "~{~A~^ ~}"
	     (mapcar #'(lambda (n) (nth n elms)) '(0 2 1 4 5 3)))))
       (convert (e)
	 (w/json (id from_user created_at text) e
	   `(("id" . ,id)
	     ("user" ("screen_name" . ,from_user))
	     ("created_at" . ,(fixdate created_at))
	     ("text" . ,text)))))
    (mapcar #'convert (json-value res :results))))


#|
(defun get-pages (api params)
  (let (statuses)
    (if (getf params :since_id)
	(do* ((page 1 (1+ page))
	      (res #1=(apply api :page page :count *count/page* params) #1#))
	    ((or (= page *page-max*)
		 (= (length res) 0))
	     #2=(setq statuses (append statuses res)))
	  #2#)
      (setq statuses (apply api :count *count/page* params)))
    (message "~D tweets" (length statuses))
    statuses))
|#


;;; timeline
(defun draw-statuses (buf statuses)
  (setq buffer-read-only nil)
  (let ((wc (1- (window-columns))))
    (with-output-to-buffer (buf 0)
      (terpri)
      (dolist (st statuses)
	(w/json (user.name user.screen_name created_at text source) st
	  (let ((start (buffer-stream-point *standard-output*)))
	    (format t "~V@{-~}~%@~A ~@[/ ~A~] [~A]~A~%~A~%" wc
	      user.screen_name user.name
	      (format-date-string "%y/%m/%d (%a) %H:%M:%S"
				  (parse-date created_at))
	      (if source
		  (concat "  from "
			  (if (string-match ">\\([^<]+\\)<" source)
			      (match-string 1)
			    source))
		"")
	      (entity2char text))
	    (set-text-attribute start (+ start wc) (cons :entry st) :foreground 14)
	    )))
      (format t "~V@{=~}~%~D new tweets" wc (length statuses))))
  (setq buffer-read-only t))

(defun entry-point (&optional (p (point)))
  (multiple-value-bind (start end tag)
      (find-text-attribute :entry :key #'safe-car :end (1+ p) :from-end t)
    (when start
      (values start end (cdr tag)))))

(defun forward-entry ()
  (interactive)
  (whenlet start (find-text-attribute :entry :key #'safe-car :start (1+ (point)))
    (goto-char start)
    (forward-line 2)
    (recenter)))

(defun backward-entry ()
  (interactive)
  (whenlet c (entry-point)
    (whenlet p (entry-point (1- c))
      (goto-char p)
      (forward-line 2)
      (recenter))))

(eval-when (:load-toplevel)
  (macrolet
      ((define-link (item re &optional (match-index 0))
	 `(defun ,(symb 'focused- item) (&optional (p (point)))
	    (save-excursion
	      (goto-char p)
	      (when (scan-buffer ,re :reverse t :regexp t)
		(let ((beg (match-beginning 0))
		      (end (match-end 0)))
		  (when (<= beg p end)
		    (values (match-string ,match-index) beg end))))))))
    (define-link url *url-regexp*)
    (define-link hashtag *hashtag-regexp* 1)
    (define-link user *user-regexp* 1)))

(defun next-link ()
  (interactive)
  (scan-buffer "\\(http://\\)\\|\\([@#][_a-zA-Z0-9]\\)" :regexp t :no-dup t))

(defun previous-link ()
  (interactive)
  (scan-buffer "\\(http://\\)\\|\\([@#][_a-zA-Z0-9]\\)" :regexp t :no-dup t :reverse t))

(defun open-link ()
  (interactive)
  (let ((user (focused-user))
	(tag (focused-hashtag))
	(url (focused-url)))
    (cond (user (when (y-or-n-p "open *tw: @~A* :" user)
		  (user::xyttr-tl-user user)))
	  (tag (when (y-or-n-p "open *tw? #~A* :" tag)
		 (user::xyttr-search (concat "#" tag))))
	  (url
	   (setq url (user::expand-short-url url))
	   (when (y-or-n-p "open ~A :" url)
	     (if *open-url-by*
		 (funcall *open-url-by* url)
	       (shell-execute url t)))))))

;;; reload
(defun reload ()
  (interactive)
  (when (eq buffer-mode 'xyttr-timeline-mode)
    (let* ((buf (selected-buffer))
	   (name  (substitute-string (buffer-name buf) " ([0-9]+)" ""))
	   counts)
      (whenlet statuses (apply (apiconf-apifunc reload-conf)
			       #1=(apiconf-params reload-conf))
	(setq counts (length statuses))
	(draw-statuses (selected-buffer) statuses)
	(recenter)
	(setf (getf (apiconf-params reload-conf) :since_id)
	      (json-value (car statuses) :id)))
      (rename-buffer (format nil "~A~@[ (~D)~]" name counts) buf)
      t)))

(defun set-auto-reload (buf interval)
  (whenlet rc (buffer-local-value buf 'reload-conf)
    (whenlet ar #1=(apiconf-auto-reload rc)
      (stop-timer ar))
    (setf #1# `(lambda ()
		 (save-window-excursion
		   (set-buffer ,buf)
		   (reload))
		 (refresh-screen)))
    (start-timer interval #1#)
    t))

(defun disable-auto-reload (buf)
  (whenlet rc (buffer-local-value buf 'reload-conf)
    (whenlet ar #1=(apiconf-auto-reload reload-conf)
      (stop-timer ar)
      (setf #1# nil))))

(defun toggle-auto-reload ()
  (interactive)
  (message "auto reload: ~:[off~;on (~A sec)~]"
	   (if (apiconf-auto-reload reload-conf)
	       (disable-auto-reload (selected-buffer))
	     (when *auto-reload*
	       (set-auto-reload (selected-buffer) *auto-reload*)))
	   *auto-reload*))



;;; mode
(defun xyttr-timeline-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'xyttr-timeline-mode
	mode-name "xyttr"
	kept-undo-information nil
	need-not-save t
	buffer-read-only t
	auto-save nil)
  (unless (local-variable-p 'regexp-keyword-list)
    (make-local-variable 'regexp-keyword-list))
  (setf regexp-keyword-list
	(append regexp-keyword-list *regexp-keyword-list*))
  (set-buffer-fold-type-window)
  (use-keymap *xyttr-timeline-keymap*)

  ; delete timer
  (add-hook 'ed:*before-delete-buffer-hook*
	    '(lambda (buf)
	      (when (eq buffer-mode 'xytt-timeline-mode)
		(disable-auto-reload buf))
	      t))

  (unless (find "xyttr/config" *modules* :test #'string=)
    (let ((config (merge-pathnames ".xyttr/config.l" (si:system-root))))
      (when (file-exist-p config)
	(load-file config)))))


;;; user command
(defmacro define-tl-command (name params &key interactive buffer-name api-func api-params hook auto-reload)
  `(defun ,(intern (symbol-name name) "user") ,params
     (interactive ,interactive)
     (let ((buf (get-buffer-create ,buffer-name)))
       (set-buffer buf)
       (unless (eq buffer-mode 'xyttr-timeline-mode)
	 (xyttr-timeline-mode)
	 (setq reload-conf
	       (make-apiconf :apifunc ,api-func
			     :params (list ,@api-params)))
	 (whenlet interval (or ,auto-reload *auto-reload*)
	   (set-auto-reload buf interval)))
       (reload)
       ,(when hook `(run-hooks ,hook))
       t)))

(define-tl-command xyttr ()
  :buffer-name "*tw: home*"
  :api-func #'api-home-timeline
  :api-params (:count 50))

(define-tl-command xyttr-user (user)
  :interactive "sUser: @"
  :buffer-name (format nil "*tw: ~:[@~A~;mine~]*" (string= "" user) user)
  :api-func #'api-user-timeline
  :api-params (:screen_name user :count 50))

(define-tl-command xyttr-mentions ()
  :buffer-name "*tw: mentions*"
  :api-func #'api-mentions
  :api-params (:count 50))

(define-tl-command xyttr-retweeted-by-me ()
  :buffer-name "*tw: my RTs*"
  :api-func #'api-retweeted-by-me
  :api-params (:count 20))

(define-tl-command xyttr-retweeted-to-me ()
  :buffer-name "*tw: friend RTs*"
  :api-func #'api-mentions
  :api-params (:count 20))

(define-tl-command xyttr-retweeted-of-me ()
  :buffer-name "*tw: my RTed*"
  :api-func #'api-retweets-of-me
  :api-params (:count 20))


(define-tl-command xyttr-search (q)
  :interactive "sSearch Twitter:"
  :buffer-name (format nil "*tw? ~A*" q)
  :api-func #'api-search
  :api-params (:q q :rpp 50 :lang *search-lang*))


(defun user::tweet (status)
  (interactive "sTweet: ")
  (when (string/= status "")
    (api-update :status status)))


;;; actions
(defmacro w/entry (keys &body body)
  `(multiple-value-bind (#:s #:e #1=#:tag) (entry-point)
     (when #1# (w/json ,keys #1# ,@body))))

(defun tweet (status)
  (interactive "sTweet: ")
  (when (string/= status "")
    (api-update :status status)
    (reload)))

(defun mention () 
  (interactive)
  (w/entry (user.screen_name)
    (let ((status (read-string "Tweet: "
			       :default (format nil "@~A " user.screen_name))))
      (when (string/= "" status)
	(api-update :status status)
	(reload)))))

(defun tweet-with-quote ()
  (interactive)
  (w/entry (id user.screen_name text)
    (let* ((qt (format nil *quote-format* user.screen_name text))
	   (status (read-string "Tweet: " :default qt)))
      (when (string/= "" status)
	(api-update :status status)
	(reload)))))

(defun reply-to ()
  (interactive)
  (w/entry (id user.screen_name text)
    (let ((status (read-string "Reply: "
			       :default (format nil "@~A " user.screen_name))))
      (when (string/= "" status)
	(api-update :status status
		    :in_reply_to_status_id id)
	(reload)))))

(defun reply-with-quote ()
  (interactive)
  (w/entry (id user.screen_name text)
    (let* ((qt (format nil " RT @~A: ~A" user.screen_name text))
	   (status (read-string "Reply: " :default qt)))
      (when (string/= "" status)
	(api-update :status status
		    :in_reply_to_status_id id)
	(reload)))))

(defun retweet ()
  (interactive)
  (w/entry (id user.screen_name text)
    (when (y-or-n-p "Retweet? @~A:~A ~:[...~;~]"
		    user.screen_name
		    (subseq text 0 (min #1=(length text) 50))
		    (< #1# 50))
      (api-retweet :id id))))


(defun destroy-status ()
  (interactive)
  (w/entry (id)
    (api-destroy :id id)))

(defun copy-status-url ()
  (interactive)
  (w/entry (id user.screen_name)
    (let ((url (format nil "http://twitter.com/~A/status/~A" user.screen_name id)))
      (copy-to-clipboard url)
      (message "Copied: ~A" url))))


(defun test ()
  (interactive)
  (msgbox "api   : ~S~%params: ~S~%reload: ~S"
	  (apiconf-apifunc reload-conf)
	  (apiconf-params reload-conf)
	  (apiconf-auto-reload reload-conf)))


;;; for edit
(setf (get 'w/uniq #1='ed:lisp-indent-hook) 1
      (get 'whenlet #1#) 2
      (get 'w/json #1#) 2
      (get 'w/entry #1#) 1
      (get 'define-api #1#) 1
      (get 'define-tl-command #1#) 2)

