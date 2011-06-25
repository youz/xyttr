;;; -*- mode:Lisp; package:xyttr -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'xml-http-request)
  (require 'json)
  (require 'oauth))

(defpackage :xyttr
  (:use :lisp :editor))


(in-package "xyttr")
(export '(*xyttr-timeline-keymap*
	  *default-user*
	  *auto-reload*
	  *open-url-by*
	  *search-lang*
	  *quote-format*
	  *use-frame*
	  *frame-name*))

(defvar *default-user* nil)
(defvar *auto-reload* nil
  "auto-reload interval (sec)")
(defvar *open-url-by* nil)
(defvar *search-lang* "ja")
(defvar *quote-format* " RT @~A: ~A")
(defvar *use-frame* t)
(defvar *frame-name* "xyttr")


;;; const
(defconstant *api-url*    "http://api.twitter.com")
(defconstant *search-url* "http://search.twitter.com")
(defconstant *X-Twitter-Client* "xyttr")
(defconstant *X-Twitter-Client-URL* "http://github.com/youz/xyttr")
(defconstant *X-Twitter-Client-Version* "0.1.0")

;;; var
(defvar *consumer-key*    "9tO9vX1QgwWPqoPNnhcMw")
(defvar *consumer-secret* "asn1tSaaQbs1wCm5gOVUoNdfHPGO4j3WuFKfsNKW8I")
(defvar *token* nil)
(defvar *token-secret* nil)

(defvar *xyttr-timeline-keymap* (make-sparse-keymap))

(let ((m *xyttr-timeline-keymap*))
  ;; open timeline
  (define-key m #\U 'user::xyttr-user)
  (define-key m #\M 'user::xyttr-mentions)
  (define-key m #\F 'user::xyttr-favorites)
  (define-key m #\s 'user::xyttr-search)
  (define-key m #\S 'user::xyttr-search-global)
  (define-key m #\/ 'user::xyttr-search-global)
  (define-key m #\L 'user::xyttr-list)

  ;; cursor
  (define-key m #\j 'forward-entry)
  (define-key m #\k 'backward-entry)
  (define-key m #\l 'next-link)
  (define-key m #\h 'previous-link)
  (define-key m #\RET 'open-link)
  (define-key m #\TAB 'next-link)
  (define-key m #\M-Down 'next-link)
  (define-key m #\M-Up 'previous-link)

  ;; post
  (define-key m #\u 'tweet)
  (define-key m #\@ 'mention)
  (define-key m #\` 'reply-to)
  (define-key m #\f 'toggle-favorite)
  (define-key m '(#\r #\t) 'retweet)
  (define-key m '(#\r #\u) 'tweet-with-quote)
  (define-key m '(#\r #\r) 'reply-with-quote)
  (define-key m #\D 'destroy-status)

  ;; etc
  (define-key m #\R 'timeline-reload)
  (define-key m #\M-r 'toggle-auto-reload)
  (define-key m #\J 'timeline-append-page)
  (define-key m #\Q 'close-timeline-buffer)
  (define-key m #\C 'copy-status-url)
  (define-key m #\p 'show-irt-status)
  )

(defvar *url-regexp* "https?://[-0-9A-Za-z_.!~*'();/?:@&=+$,%#|]+")
(defvar *hashtag-regexp* "#\\([_a-zA-Z0-9]+\\)")
(defvar *user-regexp* "@\\([_a-zA-Z0-9]+\\)")

(defvar *link-regexps*
  (list *url-regexp*
	*hashtag-regexp*
	*user-regexp*))

(defvar *regexp-keyword-list*
  (compile-regexp-keyword-list
   `((,*hashtag-regexp* t (:keyword 1))
     (,*user-regexp* t (:keyword 0)))))


(defstruct modeconf
  "xyttr mode config (buffer-local)"
  user        ; username (= screen name)
  mode        ; timeline mode name (:home-timeline, :user-timeline, etc)
  apifunc     ; #'api-home-timeline, #'api-user-timeline, etc
  params      ; parameters passed to apifunc
  auto-reload ; auto-reload interval (number of seconds, or nil)
  (unread 0)  ; unread counter
  )

(defstruct timeline
  "timeline data (buffer-local)"
  alldata last-id first-id page)

;;; local-variables
(defvar-local buffer-modeconf nil)
(defvar-local buffer-timeline nil)


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
	  '(("&gt;" ">") ("&lt;" "<") ("&quot;" "\"") ("&amp;" "&"))
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


(defmacro w/buffer-modifying ((&optional buf) &body body)
  `(save-excursion
     (set-buffer ,(or buf '(selected-buffer)))
     (setq ed:buffer-read-only nil)
     ,@body
     (set-buffer-modified-p nil)
     (setq ed:buffer-read-only t)))


;;; auth
(defun load-token (user)
  (let ((tokenfile (concat "~/.xyttr/token_" user)))
    (if (file-exist-p tokenfile)
	(with-open-file (is tokenfile :direction :input)
	  (setq *token* (read-line is nil)
		*token-secret* (read-line is nil))
	  t))))

(defun get-access-token (&optional tokenfile)
  (unless (eq (message-box "ブラウザ経由でOAuth認証を行います。" "xyttr"
			   '(:ok-cancel :information)) :ok)
    (quit))
  (multiple-value-bind (tokens res)
      (oauth:get-access-token
       *consumer-key* *consumer-secret*
       (concat *api-url* "/oauth/request_token")
       (concat *api-url* "/oauth/authorize")
       (concat *api-url* "/oauth/access_token"))
    (when tokens
      (setq *token* (getf tokens :token)
	    *token-secret* (getf tokens :token-secret))
      (whenlet screen-name (and (string-match "screen_name=\\([^&]+\\)" res)
				(match-string 1))
	(unless (file-directory-p "~/.xyttr")
	  (create-directory "~/.xyttr"))
	(with-open-file (os (or tokenfile (concat "~/.xyttr/token_" screen-name))
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
	  (format os "~A~%~A~%" *token* *token-secret*)
	  (message "token saved: ~A" tokenfile))
	t))))


;;; api
(defun api-request-sync (method host path params &key raw)
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
	(let ((msg (format nil "http status: ~A~%~A" status res)))
	  (message-box msg "request failed" '(:exclamation :ok))
	  nil)))))


(defmacro define-api (name params
		      &key (auth t) (method 'get) (apiurl '*api-url*)
			   path key raw)
  (unless path (error "define-api: missing 'path'"))
  `(progn
     (defun ,(symb "api-" name) (&key ,@params)
       (funcall
	,(or key '#'identity)
	(funcall #'api-request-sync ',method ,apiurl ,path
		 (append
		  ,@(loop for p in params
		      for n = (symbol-name p)
		      collect
		      `(if ,#1=(intern n)
			 ,(list 'list (intern n "keyword") #1#))))
		 ,@(when raw (list :raw t))
		 )))
     ))


;;; timeline api
(define-api public-timeline
  (trim_user include_entities)
  :auth nil
  :path "/1/statuses/public_timeline.json")

(define-api home-timeline
  (since_id max_id count page trim_user include_entities)
  :path "/1/statuses/home_timeline.json")

(define-api user-timeline
  (user_id screen_name since_id max_id count page
   trim_user include_rts include_entities)
  :path "/1/statuses/user_timeline.json")

(define-api mentions
  (since_id max_id count page trim_user include_rts include_entities)
  :path "/1/statuses/mentions.json")

(define-api retweeted-by-me
  (since_id max_id count page trim_user include_entities)
  :path "/1/statuses/retweeted_by_me.json")

(define-api retweeted-to-me
  (since_id max_id count page trim_user include_entities)
  :path "/1/statuses/retweeted_to_me.json")

(define-api retweets-of-me
  (since_id max_id count page trim_user include_entities)
  :path "/1/statuses/retweets_of_me.json")

(define-api show-status (id)
  :path (format nil "/1/statuses/show/~D.json" id))


;;; post api
(define-api update
  (status in_reply_to_status_id lat long place_id display_coordinates)
  :method post
  :path "/1/statuses/update.json")

(define-api destroy (id)
  :method post
  :path (format nil "/1/statuses/destroy/~D.json" id))


;;; retweet api
(define-api retweet (id)
  :method post
  :path (format nil "/1/statuses/retweet/~D.json" id))

(define-api retweets (id count)
  :method post
  :path (format nil "/1/statuses/retweets/~D.json" id))

(define-api retweeted_by (id count page)
  :path (format nil "/1/statuses/~D/retweeted_by.json" id))

(define-api retweeted_by/ids (id count page)
  :path (format nil "/1/statuses/~D/retweeted_by/ids.json" id))

;;; favorites api
(define-api favorites (id page)
  :path (format nil "/1/favorites~:[~;/~:*~A~].json" id))

(define-api favorites-create (id)
  :method post
  :path (format nil "/1/favorites/create/~D.json" id))

(define-api favorites-destroy (id)
  :method post
  :path (format nil "/1/favorites/destroy/~D.json" id))

(define-api test ()
  :path "/1/help/test.json"
  :raw t)


;;; list api
(define-api list-create (user name mode description)
  :method post
  :path (format nil "/1/~A/lists.json" user))

(define-api list-update (user list_id name mode description)
  :method post
  :path (format nil "/1/~A/lists/~A.json" user list_id))

(define-api list-index (user cursor)
  :path (format nil "/1/~A/lists.json" user))

(define-api list-info (user list_id)
  :path (format nil "/1/~A/lists/~A.json" user list_id))

(define-api list-delete (user list_id)
  :method post
  :path (format nil "/1/~A/lists/~A.json" user list_id))

(define-api list-statuses (user list_id since_id max_id per_page page)
  :path (format nil "/1/~A/lists/~A/statuses.json" user list_id))

(define-api list-memberships (user list_id)
  :path (format nil "/1/~A/lists/memberships.json" user list_id))

(define-api list-subscriptions (user cursor)
  :path (format nil "/1/~A/lists/subscriptions.json" user))


;;; search api
(define-api search
  (q lang rpp page max_id since_id since #+:nil until
   geocode show_ser result_type)
  :apiurl *search-url*
  :path "/search.json"
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
    (mapcar #'convert (json-value res results))))


;;; timeline
(defun timeline-draw-statuses (buf statuses &key (point 0) (display-count nil))
  (save-excursion
    (set-buffer buf)
    (w/buffer-modifying (buf)
      (let ((wc (max 20 (1- (window-columns)))))
	(with-output-to-buffer (buf point)
	  (terpri)
	  (dolist (st statuses)
	    (w/json (user.name user.screen_name created_at text source favorited
			       retweeted_status retweet_count) st
	      (let ((start #1=(buffer-stream-point *standard-output*)))
		(format t "~V@{-~}~%@~A ~@[/ ~A~] [~A] ~:[ ~;★~]"
			wc user.screen_name user.name
			(format-date-string "%y/%m/%d (%a) %H:%M:%S"
					    (parse-date created_at))
			favorited)
		(set-text-attribute (1- #1#) #1# (cons :fav favorited) :foreground 11)
		(format t "~A~%~A~%"
			(if source
			    (concat " from "
				    (if (string-match ">\\([^<]+\\)<" source)
					(match-string 1)
				      source))
			  "")
			(if retweeted_status
			    (w/json (user.screen_name text) retweeted_status
			      (format nil "RT~@[(~A)~] @~A: ~A"
				      retweet_count user.screen_name (entity2char text)))
			  (entity2char text)))
		(set-text-attribute start (+ start wc) (cons :entry st) :foreground 14))))
	  (when display-count
	    (format t "~V@{=~}~%~D new tweets" wc (length statuses)))
	  )))
    (recenter)))


(defun entry-point (&optional (p (point)))
  (multiple-value-bind (start end tag)
      (find-text-attribute :entry :key #'safe-car :end (1+ p) :from-end t)
    (when start
      (values start (cdr tag)))))

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

(defun get-link (point pattern &optional (match-index 0))
  (save-excursion
    (goto-char point)
    (when (scan-buffer pattern :reverse t :regexp t)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(when (<= beg point end)
	  (values (match-string match-index) beg end))))))
  
(defun focused-url ()
  (get-link (point) *url-regexp*))

(defun focused-hashtag ()
  (get-link (point) *hashtag-regexp* 1))

(defun focused-user ()
  (get-link (point) *user-regexp* 1))

(defun next-link ()
  (interactive)
  (scan-buffer (format nil "~{~A~^\\|~}" *link-regexps*) :regexp t :no-dup t))

(defun previous-link ()
  (interactive)
  (scan-buffer (format nil "~{~A~^\\|~}" *link-regexps*) :regexp t :no-dup t :reverse t))

(defun open-link ()
  (interactive)
  (let ((user (focused-user))
	(tag (focused-hashtag))
	(url (focused-url)))
    (cond (user (when (y-or-n-p "open *tw: @~A* :" user)
		  (user::xyttr-user user)))
	  (tag (when (y-or-n-p "open *tw? #~A* :" tag)
		 (user::xyttr-search-global (concat "#" tag))))
	  (url
	   (setq url (expand-short-url url))
	   (when (y-or-n-p "open ~A :" url)
	     (if *open-url-by*
		 (funcall *open-url-by* url)
	       (shell-execute url t)))))))


;;; reload
(defun timeline-reload (&optional (buf (selected-buffer)))
  (interactive)
  (when (eq (buffer-local-value buf 'ed:buffer-mode) 'xyttr-timeline-mode)
    (let ((name  (substitute-string (buffer-name buf) " ([0-9]+)$" ""))
	  (conf (buffer-local-value buf 'buffer-modeconf))
	  (tl (buffer-local-value buf 'buffer-timeline)))
      (whenlet statuses
	  (apply (modeconf-apifunc conf)
		 :since_id #1=(timeline-last-id tl)
		 (modeconf-params conf))
	(incf #2=(modeconf-unread conf) (length statuses))
	(timeline-draw-statuses buf statuses)
	(setf #1# (json-value (car statuses) id)
	      #3=(timeline-alldata tl) (append statuses #3#))
	(unless #4=(timeline-first-id tl)
	  (setf #4# (json-value (car (last statuses)) id))))
      (rename-buffer (format nil "~A~[~:; (~D)~]" name #2# #2#) buf)
      (refresh-screen)
      t)))

(defun set-auto-reload (buf interval)
  (whenlet rc (buffer-local-value buf 'buffer-modeconf)
    (whenlet ar #1=(modeconf-auto-reload rc)
      (stop-timer ar))
    (setf #1# `(lambda ()
		 (timeline-reload ,buf)))
    (start-timer (max 30 interval) #1#)
    t))

(defun disable-auto-reload (buf)
  (whenlet rc (buffer-local-value buf 'buffer-modeconf)
    (whenlet ar #1=(modeconf-auto-reload buffer-modeconf)
      (stop-timer ar)
      (setf #1# nil))))

(defun toggle-auto-reload ()
  (interactive)
  (message "auto reload: ~:[off~;on (~A sec)~]"
	   (if (modeconf-auto-reload buffer-modeconf)
	       (disable-auto-reload (selected-buffer))
	     (when *auto-reload*
	       (set-auto-reload (selected-buffer) *auto-reload*)))
	   *auto-reload*))



(defun timeline-append-page ()
  (interactive)
  (whenlet id (and (eq buffer-mode 'xyttr-timeline-mode)
		   #1=(timeline-first-id buffer-timeline))
    (let* ((buf (selected-buffer))
	   (name  (substitute-string (buffer-name buf) " ([0-9]+)$" "")))
      (whenlet statuses
	  (apply (modeconf-apifunc buffer-modeconf)
		 :max_id (1- id) (modeconf-params buffer-modeconf))
	(timeline-draw-statuses (selected-buffer) statuses
				:point (point-max)
				:display-count nil)
	(recenter)
	(setf #1# (json-value (car (last statuses)) id))
	(nconc (timeline-alldata buffer-timeline) statuses)))
      t))



;;; major mode
(defun xyttr-init-user ()
  (if (and *token* *token-secret*) t
    (if *default-user*
	(or (load-token *default-user*)
	    #1=(get-access-token))
      (let ((users (mapcar #'(lambda (f) (and (string-match "token_\\(.+\\)$" f)
					      (match-string 1)))
			   (directory "~/.xyttr/" :wild "token_*"))))
	(if users
	    (let ((user (ed:completing-read "username: " users)))
	      (if (find user users :test #'string=)
		  (load-token user)
		#1#))
	  #1#)))))

(defun timeline-on-activate ()
  (when (eq buffer-mode 'xyttr-timeline-mode)
    (setf (modeconf-unread buffer-modeconf) 0)))

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

  (setq buffer-modeconf (make-modeconf)
	buffer-timeline (make-timeline))
  
  (add-hook 'ed:*post-command-hook*
	    'timeline-on-activate)
  
  ; delete timer
  (add-hook 'ed:*before-delete-buffer-hook*
	    '(lambda (buf)
	      (when (eq buffer-mode 'xytt-timeline-mode)
		(disable-auto-reload buf))
	      t))

  (unless (find "xyttr/config" *modules* :test #'string=)
    (let* ((config (merge-pathnames ".xyttr/config.l" (si:system-root)))
	   (lc (compile-file-pathname config)))
      (when (file-exist-p config)
	(if (file-exist-p lc)
	    (if (file-newer-than-file-p config lc)
		(load-file config)
	      (load-file lc))
	  (load-file config))))))


;;; user command
(defmacro define-tl-command (name params &key interactive buffer-name api-func api-params hook auto-reload)
  `(defun ,(intern (symbol-name name) "user") ,params
     (interactive ,interactive)
     (unless (xyttr-init-user) (quit))
     (let ((buf (get-buffer-create ,buffer-name)))
       (when *use-frame*
	 (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
				  (new-pseudo-frame *frame-name*))))
       (set-buffer buf)
       (unless (eq buffer-mode 'xyttr-timeline-mode)
	 (xyttr-timeline-mode)
	 (setf (modeconf-mode buffer-modeconf) ,(intern (symbol-name name) (find-package "keyword"))
	       (modeconf-apifunc buffer-modeconf) ,api-func
	       (modeconf-params buffer-modeconf) (list ,@api-params))
	 (whenlet interval (or ,auto-reload *auto-reload*)
	   (set-auto-reload buf interval)))
       (timeline-reload)
       (setf (modeconf-unread buffer-modeconf) 0)
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
  :api-params (:screen_name user :count 50 :include_rts t))

(define-tl-command xyttr-mentions ()
  :buffer-name "*tw: mentions*"
  :api-func #'api-mentions
  :api-params (:count 50 :include_rts t))

(define-tl-command xyttr-retweeted-by-me ()
  :buffer-name "*tw: RTs by me*"
  :api-func #'api-retweeted-by-me
  :api-params (:count 20))

(define-tl-command xyttr-retweeted-to-me ()
  :buffer-name "*tw: RTs to me*"
  :api-func #'api-retweeted-to-me
  :api-params (:count 20))

(define-tl-command xyttr-retweeted-of-me ()
  :buffer-name "*tw: RT of me*"
  :api-func #'api-retweets-of-me
  :api-params (:count 20))

(define-tl-command xyttr-favorites ()
  :buffer-name "*tw: favs*"
  :api-func #'api-favorites
  :api-params (:page 1)
  :auto-reload nil)


(define-tl-command xyttr-search (q)
  :interactive "sSearch Twitter: "
  :buffer-name (format nil "*tw? ~A*" q)
  :api-func #'api-search
  :api-params (:q q :rpp 50 :lang *search-lang*))

(define-tl-command xyttr-search-global (q)
  :interactive "sSearch Twitter (global):"
  :buffer-name (format nil "*tw? ~A*" q)
  :api-func #'api-search
  :api-params (:q q :rpp 50))


;;; list timeline
(defvar *list-subscriptions* nil)

(defun subscriptions (&optional (user *default-user*))
  (mapcar #'(lambda (l)
	      (w/json (full_name id_str user.screen_name) l
		(list full_name user.screen_name id_str)))
	  (json-value (api-list-subscriptions :user user) lists)))

(defun user::xyttr-list ()
  (interactive)
  (unless (xyttr-init-user) (quit))
  (unless *list-subscriptions*
    (setq *list-subscriptions* (subscriptions)))
  (let* ((lname (completing-read "List: " (mapcar #'car *list-subscriptions*)
				 :must-match t))
	 (l (assoc lname *list-subscriptions* :test #'string=))
	 (user (cadr l))
	 (listid (caddr l))
	 (buf (get-buffer-create (format nil "*tw: ~A" lname))))
    (when *use-frame*
      (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
			       (new-pseudo-frame *frame-name*))))
    (set-buffer buf)
    (unless (eq buffer-mode 'xyttr-timeline-mode)
      (xyttr-timeline-mode)
      (setf (modeconf-mode buffer-modeconf) :xyttr-list
	    (modeconf-apifunc buffer-modeconf) #'api-list-statuses
	    (modeconf-params buffer-modeconf) (list :user user :list_id listid))
      (whenlet *auto-reload*
	  (set-auto-reload buf *auto-reload*)))
    (timeline-reload)
    (setf (modeconf-unread buffer-modeconf) 0)
    t))


;;; actions
(defmacro w/entry (keys &body body)
  `(multiple-value-bind (#:s #1=#:tag) (entry-point)
     (when #1# (w/json ,keys #1# ,@body))))

(defun show-input-length ()
  (let ((chars (- (point-max) (point-min))))
    (message "~:[!! ~;~]~D 文字 (残り ~D)" (<= chars 140) chars (- 140 chars))))

(defun read-status (prompt &key default)
  (add-hook '*post-command-hook* #'show-input-length)
  (labels ((remover (buffer history)
	     (delete-hook '*post-command-hook* #'show-input-length)
	     (delete-hook '*exit-minibuffer-hook* #'remover)))
    (add-hook '*exit-minibuffer-hook* #'remover))
  (read-string prompt :default default))

(defun tweet ()
  (interactive)
  (let ((status (read-status "Tweet: ")))
    (when (string/= status "")
      (api-update :status status)
      (timeline-reload))))

(defun mention ()
  (interactive)
  (w/entry (user.screen_name)
    (let ((status (read-status "Tweet: "
			       :default (format nil "@~A " user.screen_name))))
      (when (string/= "" status)
	(api-update :status status)
	(timeline-reload)))))

(defun tweet-with-quote ()
  (interactive)
  (w/entry (id user.screen_name text)
    (let* ((qt (format nil *quote-format* user.screen_name text))
	   (status (read-status "Tweet: " :default qt)))
      (when (string/= "" status)
	(api-update :status status)
	(timeline-reload)))))

(defun reply-to ()
  (interactive)
  (w/entry (id user.screen_name text)
    (let ((status (read-status "Reply: "
			       :default (format nil "@~A " user.screen_name))))
      (when (string/= "" status)
	(api-update :status status
		    :in_reply_to_status_id id)
	(timeline-reload)))))

(defun reply-with-quote ()
  (interactive)
  (w/entry (id user.screen_name text)
    (let* ((qt (format nil " RT @~A: ~A" user.screen_name text))
	   (status (read-status "Reply: " :default qt)))
      (when (string/= "" status)
	(api-update :status status
		    :in_reply_to_status_id id)
	(timeline-reload)))))

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


(defun toggle-favorite ()
  (interactive)
  (multiple-value-bind (from status) (entry-point)
    (multiple-value-bind (sfrom sto stag)
	(find-text-attribute :fav :start from :key #'safe-car)
      (when sfrom
	(w/json (id) status
	  (let ((state (cdr stag)))
	    (if state (api-favorites-destroy :id id)
	      (api-favorites-create :id id))
	    (setf state (not state))
	    (w/buffer-modifying ()
	      (narrow-to-region sfrom sto)
	      (delete-region (point-min) (point-max))
	      (insert (if state "★" " "))
	      (set-text-attribute (point-min) (point-max) (cons :fav state)
				:foreground 11)
	      (widen))))))))

(defun status-url ()
  (w/entry (id user.screen_name)
    (format nil "http://twitter.com/~A/status/~A" user.screen_name id)))

(defun copy-status-url ()
  (interactive)
  (w/entry (id user.screen_name)
    (let ((url (status-url)))
      (copy-to-clipboard url)
      (message "Copied: ~A" url))))

(defun show-irt-status ()
  (interactive)
  (w/entry (id in_reply_to_status_id)
    (whenlet irtid
	(case (modeconf-mode buffer-modeconf)
	  ((:xyttr-search :xyttr-search-global)
	   (whenlet st (api-show-status :id id)
	     (json-value st in_reply_to_status_id)))
	  (t in_reply_to_status_id))
      (whenlet irtst (api-show-status :id irtid)
	(w/json (user.screen_name text) irtst
	  (popup-string (format nil "@~A: ~A" user.screen_name text)
			(point)))))))


(defun timeline-buffer-p (buf)
  (eq (buffer-local-value buf 'ed:buffer-mode) 'xyttr-timeline-mode))

(defun close-timeline-buffer (&optional (buf (selected-buffer)))
  (interactive)
  (when (timeline-buffer-p buf)
    (disable-auto-reload buf)
    (kill-buffer buf)
    (when (and *use-frame* (notany #'timeline-buffer-p (buffer-list)))
      (delete-pseudo-frame (find-pseudo-frame *frame-name*)))))


;;; for edit
(setf (get 'w/uniq #1='ed:lisp-indent-hook) 1
      (get 'whenlet #1#) 2
      (get 'w/json #1#) 2
      (get 'w/entry #1#) 1
      (get 'w/buffer-modifying #1#) 1
      (get 'define-api #1#) 1
      (get 'define-tl-command #1#) 2)

