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
	  *username-style*
	  *hashtag-style*
	  *favorite-star-style*
	  *separater-style*
	  *use-frame*
	  *frame-name*
	  *timeline-filter*
	  *timeline-reload-hook*))

(defvar *default-user* nil)
(defvar *auto-reload* nil
  "auto-reload interval (sec)")
(defvar *open-url-by* nil)
(defvar *search-lang* "ja")
(defvar *quote-format* " RT @~A: ~A")
(defvar *use-frame* t)
(defvar *frame-name* "xyttr")

(defvar *timeline-filter* nil
  "list of filtering functions for timeline")

(defvar *timeline-reload-hook* nil)

;;; const
(defconstant *api-url*    "https://api.twitter.com")
(defconstant *search-url* "http://search.twitter.com")
(defconstant *client-info*
  '(:X-Twitter-Client         "xyttr"
    :X-Twitter-Client-URL     "http://github.com/youz/xyttr"
    :X-Twitter-Client-Version "1.1.2"))


;;; auth
(defvar *consumer-key*    "9tO9vX1QgwWPqoPNnhcMw")
(defvar *consumer-secret* "asn1tSaaQbs1wCm5gOVUoNdfHPGO4j3WuFKfsNKW8I")
(defvar *token* nil)
(defvar *token-secret* nil)

;;; keymap
(defvar *xyttr-timeline-keymap* (make-sparse-keymap))

(let ((m *xyttr-timeline-keymap*))
  ;; open timeline
  (define-key m #\H 'user::xyttr)
  (define-key m #\U 'user::xyttr-user)
  (define-key m #\M 'user::xyttr-mentions)
  (define-key m #\M-M 'user::xyttr-messages)
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
  (define-key m '(#\d #\m) 'send-message)
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
  (define-key m #\e 'expand-focused-url)
  )

;;; styles
(defvar *username-style* '(:keyword 0))
(defvar *hashtag-style* '(:keyword 1))
(defvar *favorite-star-style*  '(:foreground 3))
(defvar *separater-style* '(:foreground 14))


;;; link elements
(defvar *url-regexp* "https?://[-0-9A-Za-z_.!~*';/?:@&=+$,%#|]+")
(defvar *hashtag-regexp* "\\(^\\|[ 　,.､｡，．、。]\\)\\([#＃]\\([_a-zA-Z0-9ｦ-ﾟ０-ヶヽ-々ー亜-黑]+\\)\\)")
(defvar *user-regexp* "@\\([_a-zA-Z0-9]+\\)")

(defvar *link-regexps*
  (list *url-regexp*
	*hashtag-regexp*
	*user-regexp*))

(defun make-regexp-keyword-list ()
  (compile-regexp-keyword-list
   `((,*hashtag-regexp* t ,*hashtag-style* nil 2)
     (,*user-regexp* t ,*username-style*))))


;;; buffer local data
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct timeline
    "timeline settings and data"
    user        ; username (= screen name)
    tokens      ; reserved
    mode        ; timeline mode name (:home-timeline, :user-timeline, etc)
    apifunc     ; #'api-home-timeline, #'api-user-timeline, etc
    params      ; parameters passed to apifunc
    auto-reload ; auto-reload interval (number of seconds, or nil)
    (unread 0)  ; unread counter
    request     ; xhr-cancel-ticket
    ;; timeline data
    alldata     ; list of all tweets
    last-id     ; status id of the newest tweet
    first-id    ; status id of the oldest tweet
    (page 0)    ; reserved
    ))

(defvar-local buffer-timeline nil)


;;; utility
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (values (intern (format nil "~{~A~}" args))))

  (defun kwd (&rest args)
    (values (intern (format nil "~{~A~}" args) "keyword")))

  (defmacro w/uniq (names &body body)
    `(let ,(mapcar #'(lambda (s) `(,s (make-symbol ,(symbol-name s))))
		   (if (consp names) names (list names)))
       ,@body))

  (defmacro whenlet (var expr &body body)
    `(let ((,var ,expr)) (when ,var ,@body)))

  (defmacro whilet (var test &body body)
    `(do ((,var ,test ,test))
	 ((not ,var))
       ,@body))
  )

(defmacro json-value (obj key)
  (reduce #'(lambda (o k) `(cdr (assoc ,k ,o :test #'string=)))
	  (split-string (symbol-name key) #\.)
	  :initial-value obj))

(defmacro w/json (keys obj &body body)
  (w/uniq gobj
    (flet ((mkbind (k)
	     (if (consp k)
		 `(,(cadr k) (json-value ,gobj ,(car k)))
	       `(,k (json-value ,gobj ,k)))))
      `(let* ((,gobj ,obj) ,@(mapcar #'mkbind keys))
	 ,@body))))

(defun filter (filter-list xs)
  (flet ((filter1 (x)
	   (reduce #'(lambda (x f) (if x (funcall f x) nil))
		   filter-list
		   :initial-value x)))
    (delete-if-not #'filter1 xs)))

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

(defun expand-short-url (url &optional depth)
  (while (and (string-match "http://\\([^/]+\\)/\\(.+\\)$" url)
	      (or (null depth) (>= (decf depth) 0)))
    (let ((host (match-string 1))
	  (path (match-string 2)))
      (with-open-stream (cn (connect host 80))
	(format cn "GET /~A HTTP/1.1\nHost: ~A\n\n" path host)
	(let* ((res (read-line cn nil))
	       (status (and (string-match "^HTTP/[0-9.]+ \\([0-9]+\\) .+$" res)
			    (match-string 1))))
	  (unless (string-match "^30." status)
	    (return))
	  (whilet l (read-line cn t)
	    (when (or (string-match "^Location: \\(.+\\)$" l) (string= l ""))
	      (return)))
	  (whenlet m (match-string 1)
	    (setq url (if (char= (char m 0) #\/) (concat "http://" host m) m)))))))
    url)

(defmacro w/buffer-modifying ((&optional buf) &body body)
  `(save-excursion
     (set-buffer ,(or buf '(selected-buffer)))
     (setq ed:buffer-read-only nil)
     ,@body
     (set-buffer-modified-p nil)
     (setq ed:buffer-read-only t)))


;;; plugin
(defvar *plugins* nil "loaded plugins")

(defun load-plugin (plugin &optional force)
  (when (or force (not (find plugin *plugins* :test #'string=)))
    (let* ((name (string plugin))
	   (path (let ((*load-path* '("~/.xyttr/")))
		   (find-load-path name))))
      (unless path
	(error "plugin not found: ~A" name))
      (load-file path)
      (push name *plugins*)
      t)))


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
(define-condition request-error (network-error)
  (host path method status response)
  (:report
   (lambda (c s)
     (format s "xyttr request failed~%~
                host: ~A~%~
                path: ~A~%~
                method: ~A~%~
                status: ~A~%~
                response: ~A"
	     (request-error-host c)
	     (request-error-path c)
	     (request-error-method c)
	     (request-error-status c)
	     (request-error-response c)
	     ))))

(defun api-request (method host path params)
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
	  (json:json-decode res)
	(error 'request-error
	       :host host :path path :method method
	       :status status :response res)))))

(defun api-request-async (method host path params
			  &key raw onsuccess onfailure oncomplete handler)
  (let* ((url (concat host path))
	 (cred (list :consumer-key *consumer-key*
		     :consumer-secret *consumer-secret*
		     :token *token*
		     :token-secret *token-secret*))
	 (auth (oauth:auth-header cred method url params))
	 (succ (lambda #0=(res status header)
		 (if onsuccess
		     (funcall onsuccess (json:json-decode res))
		   (message "request ok: ~A ~A" status path))))
	 (fail (or onfailure
		   (lambda #0#
		     (error 'request-error
			    :host host :path path :method method
			    :status status :response response)))))
    (if (eq method 'get)
	(xhr:xhr-get-async url :query params .
			   #1=(:headers `(:Authorization ,auth)
			       :encoding *encoding-utf8n*
			       :key #'xhr:xhr-response-values
			       :onsuccess succ
			       :onfailure fail
			       :oncomplete oncomplete
			       :handler handler))
      (xhr:xhr-request-async method url params . #1#))))

(defmacro define-api (name params
		      &key (auth t) (method 'get) (apiurl '*api-url*)
			   path key)
  (unless path (error "define-api: missing 'path'"))
  (let ((sync (symb "api-" name))
	(async (symb "api-" name "-async")))
    `(progn
       (defun ,sync (&key ,@params)
	 ,(if (null key)
	      `#0=(api-request
		  ',method ,apiurl ,path
		   #1=(append
		       ,@(mapcar #'(lambda (p)
				     `(if ,p ,(list 'list (kwd p) p)))
				 params)))
	    `(funcall ,key #0#)))

       (defun ,async (&key ,@params
			   onsuccess onfailure oncomplete handler)
	 (api-request-async
	  ',method ,apiurl ,path #1#
	  :onsuccess ,(if (null key) 'onsuccess
			`(lambda (#2=#:json)
			   (funcall onsuccess
				    (funcall ,key #2#))))
	  :onfailure onfailure
	  :oncomplete oncomplete
	  :handler handler))
       (export '(,sync ,async)))
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


;;; direct message api
(defun messages-to-statuses (res)
  (flet
      ((convert (e)
	 (w/json (recipient.name recipient.screen_name
		  sender.name sender.screen_name) e
	   `(,@e ("user"
		  ("name" . ,(format nil "~A -> ~A" sender.name recipient.name))
		  ("screen_name" . ,(format nil "~A -> @~A" sender.screen_name recipient.screen_name)))))))
    (mapcar #'convert res)))

(define-api direct-messages (since_id max_id count page)
  :path   "/1/direct_messages.json"
  :key #'messages-to-statuses)

(define-api direct-messages-sent (since_id max_id count page)
  :path   "/1/direct_messages/sent.json"
  :key #'messages-to-statuses)

(define-api direct-messages-new (user text)
  :method post
  :path   "/1/direct_messages/new.json")

(define-api direct-messages-destroy (id)
  :method post
  :path   (format nil "/1/direct_messages/destroy/~D.json" id))

(defun api-direct-messages-all-async (&key since_id max_id count page
					   onsuccess onfailure)
  (let* ((params (list :since_id since_id :max_id max_id :count count :page page))
	 (received (apply #'api-direct-messages params))
	 (sent (apply #'api-direct-messages-sent params)))
    (sort (append received sent) #'>
	  :key #'(lambda (e) (json-value e id)))))

(defun api-direct-messages-all-async (&key since_id max_id count page
					   onsuccess onfailure)
  (let* ((params (list :since_id since_id :max_id max_id :count count :page page)))
    (apply #'api-direct-messages-async
	   :onfailure onfailure
	   :onsuccess
	   (lambda (received)
	     (apply #'api-direct-messages-sent-async
		    :onfailure onfailure
		    :onsuccess
		    (lambda (sent)
		      (funcall onsuccess
			       (sort (append received sent) #'>
				     :key #'(lambda (e) (json-value e id)))))
		    params))
	   params)))


;;; search api
(defun search-result-to-statuses (res)
  (labels
      ((fixdate (d)
	 (let ((elms (split-string (remove #\, d) #\SPC)))
	   (format nil "~{~A~^ ~}"
		   (mapcar #'(lambda (n) (nth n elms)) '(0 2 1 4 5 3)))))
       (convert (e)
	 (w/json (id from_user created_at text
		     in_reply_to_status_id profile_image_url) e
	   `(("id" . ,id)
	     ("user"
	      ("screen_name" . ,from_user)
	      ("profile_image_url" . ,profile_image_url))
	     ("created_at" . ,(fixdate created_at))
	     ("in_reply_to_status_id" . ,in_reply_to_status_id)
	     ("text" . ,text)))))
    (mapcar #'convert (json-value res results))))

(define-api search
  (q lang rpp page max_id since_id since until
   geocode show_user result_type include_entities)
  :apiurl *search-url*
  :path "/search.json"
  :key #'search-result-to-statuses)


;;; timeline buffer
(defun timeline-draw-statuses (buf statuses &key (point 0) (display-count nil))
  (save-excursion
    (set-buffer buf)
    (w/buffer-modifying (buf)
      (let ((wc (max 20 (1- (window-columns)))))
	(with-output-to-buffer (buf point)
	  (dolist (st statuses)
	    (let ((start #1=(buffer-stream-point *standard-output*)))
	      (format t " ~V@{-~}~%" (1- wc) t)
	      (w/json (user.name user.screen_name created_at text source favorited
				 retweeted_status retweet_count) st
		(let ((date (format-date-string "%y/%m/%d (%a) %H:%M:%S" (parse-date created_at)))
		      (from (if (and source (string-match ">\\([^<]+\\)<" source))
				(match-string 1)
			      source))
		      (text (if retweeted_status
				(w/json (user.screen_name text) retweeted_status
				  (format nil "RT~@[(~A)~] @~A: ~A"
					  retweet_count user.screen_name (entity2char text)))
			      (entity2char text))))
		  (format t "@~A ~@[/ ~A~] [~A] ~:[ ~;★~]"
			  user.screen_name user.name date favorited)
		  (apply #'set-text-attribute (1- #1#) #1# (cons :fav favorited) *favorite-star-style*)
		  (format t "~@[ from ~A~]~%~A~%" from text)))
	      (apply #'set-text-attribute (1+ start) (+ start wc)
		     (cons :entry st) *separater-style*)))
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
  (get-link (point) *hashtag-regexp* 3))

(defun focused-user ()
  (get-link (point) *user-regexp* 1))

(defun next-link ()
  (interactive)
  (when (and (scan-buffer (format nil "~{~A~^\\|~}" *link-regexps*) :regexp t :no-dup t)
	     (equal "#" (buffer-substring (1+ (point)) (+ (point) 2))))
    (forward-char)))

(defun previous-link ()
  (interactive)
  (when (focused-hashtag)
    (goto-char (match-beginning 0)))
  (when (and (scan-buffer (format nil "~{~A~^\\|~}" *link-regexps*) :regexp t :no-dup t :reverse t)
	     (equal "#" (buffer-substring (1+ (point)) (+ (point) 2))))
    (forward-char)))

(defun expand-focused-url (&optional d)
  (interactive "p")
  (multiple-value-bind (url beg end) (focused-url)
    (whenlet url (and url (expand-short-url url d))
      (w/buffer-modifying ()
	(delete-region beg end)
	(with-output-to-buffer ((selected-buffer) beg)
	  (format t "~A" url)))
      url)))

(defun open-link ()
  (interactive)
  (let ((user (focused-user))
	(tag (focused-hashtag)))
    (cond (user (when (y-or-n-p "open *tw: @~A* :" user)
		  (user::xyttr-user user)))
	  (tag (when (y-or-n-p "open *tw? #~A* :" tag)
		 (user::xyttr-search-global (concat "#" tag))))
	  (t
	   (whenlet url (expand-focused-url)
	     (when (y-or-n-p "open ~A :" url)
	       (if *open-url-by*
		   (funcall *open-url-by* url)
		 (shell-execute url t))))))))


;;; reload
(defun timeline-reload (&optional (buf (selected-buffer)))
  (interactive)
  (when (eq (buffer-local-value buf 'ed:buffer-mode) 'xyttr-timeline-mode)
    (let* ((name  (substitute-string (buffer-name buf) " ([0-9]+)$" ""))
	   (tl (buffer-local-value buf 'buffer-timeline)))
      (whenlet req #0=(timeline-request tl)
	(xhr:xhr-abort req))
      (setf #0#
	    (apply (timeline-apifunc tl)
		   :since_id #1=(timeline-last-id tl)
		   :onsuccess
		   (lambda (res)
		     (when res
		       (setf #1# (json-value (car res) id))
		       (unless #2=(timeline-first-id tl)
			 (setf #2# (json-value (car (last res)) id)))
		       (whenlet statuses (filter *timeline-filter* res)
			 (incf #3=(timeline-unread tl) (length statuses))
			 (setf #4=(timeline-alldata tl) (append statuses #4#))
			 (timeline-draw-statuses buf statuses)))
		     (setf #0# nil)
		     (rename-buffer (format nil "~A~[~:; (~D)~]" name #3# #3#) buf)
		     (save-excursion
		       (set-buffer buf)
		       (run-hook-with-args '*timeline-reload-hook* res))
		     (refresh-screen))
		   :onfailure
		   (lambda (res status header)
		     (setf #0# nil)
		     (message "HTTP ~D: reload failed" status))
		   (timeline-params tl)))
      t)))

(defun set-auto-reload (buf interval)
  (whenlet tl (buffer-local-value buf 'buffer-timeline)
    (whenlet ar #1=(timeline-auto-reload tl)
      (stop-timer ar))
    (setf #1# `(lambda ()
		 (unless (minibuffer-window-p (selected-window))
		   (timeline-reload ,buf))))
    (start-timer (max 30 interval) #1#)
    t))

(defun disable-auto-reload (buf)
  (whenlet tl (buffer-local-value buf 'buffer-timeline)
    (whenlet ar #1=(timeline-auto-reload tl)
      (stop-timer ar)
      (setf #1# nil))))

(defun toggle-auto-reload ()
  (interactive)
  (message "auto reload: ~:[off~;on (~A sec)~]"
	   (if (timeline-auto-reload buffer-timeline)
	       (disable-auto-reload (selected-buffer))
	     (when *auto-reload*
	       (set-auto-reload (selected-buffer) *auto-reload*)))
	   *auto-reload*))

(defun timeline-append-page ()
  (interactive)
  (when (eq buffer-mode 'xyttr-timeline-mode)
    (let* ((buf (selected-buffer))
	   (tl buffer-timeline)
	   (params (timeline-params tl)))
      (whenlet req #0=(timeline-request tl)
	(xhr:xhr-abort req))
      (if #2=(getf params :page)
	(incf #2#)
	(whenlet id #1=(timeline-first-id tl)
	  (setq params `(:max_id ,(1- id) ,@params))))
      (setf #0#
	    (apply (timeline-apifunc tl)
		   :onsuccess
		   (lambda (res)
		     (whenlet statuses (filter *timeline-filter* res)
		       (save-excursion
			 (set-buffer buf)
			 (timeline-draw-statuses buf statuses
						 :point (point-max)
						 :display-count nil)
			 (recenter)
			 (refresh-screen))
		       (setf #1# (json-value (car (last statuses)) id)
			     #0# nil)
		       (nconc (timeline-alldata tl) statuses)))
		   :onfailure
		   (lambda (res status header)
		     (message "HTTP ~D: reload failed" status))
		   params)))
    t))


;;; major mode
(defun xyttr-init ()
  (when (file-exist-p (merge-pathnames ".xyttr/config.l" (si:system-root)))
    (load-plugin "config"))
  (when
      (if (and *token* *token-secret*) t
	(if *default-user*
	    (or (load-token *default-user*) #1=(get-access-token))
	  (let ((users (mapcar #'(lambda (f) (subseq f 6))
			       (directory "~/.xyttr/" :wild "token_*"))))
	    (if users
		(let ((user (ed:completing-read "username: " users)))
		  (if (find user users :test #'string=)
		      (load-token user)
		    #1#))
	      #1#))))
    (get-lists-async)
    (get-subscriptions-async)
    t))

(defun timeline-on-activate ()
  (when (eq buffer-mode 'xyttr-timeline-mode)
    (setf (timeline-unread buffer-timeline) 0)))

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
  (setq regexp-keyword-list
	(append regexp-keyword-list (make-regexp-keyword-list)))
  (set-buffer-fold-type-window)
  (use-keymap *xyttr-timeline-keymap*)
  (setq buffer-timeline (make-timeline))
  (add-hook 'ed:*post-command-hook* 'timeline-on-activate)
  ; delete timer
  (add-hook 'ed:*before-delete-buffer-hook*
	    '(lambda (buf)
	      (when (eq buffer-mode 'xytt-timeline-mode)
		(disable-auto-reload buf))
	      t)))


(defun timeline-buffers ()
  (delete-if-not #'(lambda (buf) (eq (buffer-local-value buf 'ed:buffer-mode)
				     'xyttr-timeline-mode))
		 (buffer-list)))

(defun get-timeline-buffer-create (name)
  (or (find-if #'(lambda (buf) (string-match (concat "^" (regexp-quote name)) (buffer-name buf)))
	       (timeline-buffers))
      (create-new-buffer name)))


;;; user command
(defmacro define-tl-command (name params &key interactive buffer-name api-func api-params hook auto-reload)
  `(defun ,(intern (symbol-name name) "user") ,params
     (interactive ,interactive)
     (unless (xyttr-init) (quit))
     (let ((buf (get-timeline-buffer-create ,buffer-name)))
       (when *use-frame*
	 (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
				  (new-pseudo-frame *frame-name*))))
       (set-buffer buf)
       (unless (eq buffer-mode 'xyttr-timeline-mode)
	 (xyttr-timeline-mode)
	 (setf (timeline-mode buffer-timeline) ,(kwd name)
	       (timeline-apifunc buffer-timeline) ,api-func
	       (timeline-params buffer-timeline) (list ,@api-params))
	 (whenlet interval (or ,auto-reload *auto-reload*)
	   (set-auto-reload buf interval)))
       (timeline-reload)
       ,(when hook `(run-hooks ,hook))
       t)))

(define-tl-command xyttr ()
  :buffer-name "*tw: home*"
  :api-func #'api-home-timeline-async
  :api-params (:count 50))

(define-tl-command xyttr-user (user)
  :interactive "sUser: @"
  :buffer-name (format nil "*tw: ~:[@~A~;mine~]*" (string= "" user) user)
  :api-func #'api-user-timeline-async
  :api-params (:screen_name user :count 50 :include_rts t))

(define-tl-command xyttr-mentions ()
  :buffer-name "*tw: mentions*"
  :api-func #'api-mentions-async
  :api-params (:count 50 :include_rts t))

(define-tl-command xyttr-retweeted-by-me ()
  :buffer-name "*tw: RTs by me*"
  :api-func #'api-retweeted-by-me-async
  :api-params (:count 20))

(define-tl-command xyttr-retweeted-to-me ()
  :buffer-name "*tw: RTs to me*"
  :api-func #'api-retweeted-to-me-async
  :api-params (:count 20))

(define-tl-command xyttr-retweeted-of-me ()
  :buffer-name "*tw: RT of me*"
  :api-func #'api-retweets-of-me-async
  :api-params (:count 20))

(define-tl-command xyttr-favorites ()
  :buffer-name "*tw: favs*"
  :api-func #'api-favorites-async
  :api-params (:page 1)
  :auto-reload nil)

(define-tl-command xyttr-messages ()
  :buffer-name "*tw: messages*"
  :api-func #'api-direct-messages-all-async
  :api-params (:count 20))

(define-tl-command xyttr-search (q)
  :interactive "sSearch Twitter: "
  :buffer-name (format nil "*tw? ~A*" q)
  :api-func #'api-search-async
  :api-params (:q q :rpp 50 :lang *search-lang*
	       :include_entities "true"))

(define-tl-command xyttr-search-global (q)
  :interactive "sSearch Twitter (global):"
  :buffer-name (format nil "*tw? ~A*" q)
  :api-func #'api-search-async
  :api-params (:q q :rpp 50 :include_entities "true"))


;;; list timeline
(defvar *list-index* nil)
(defvar *list-subscriptions* nil)

(defun all-lists (&optional (user *default-user*))
  (mapcar #'(lambda (l)
	      (w/json (full_name id_str user.screen_name) l
		(list full_name user.screen_name id_str)))
	  (append
	   (json-value (api-list-index :user user) lists)
	   (json-value (api-list-subscriptions :user user) lists))))

(defun get-lists-async (&optional (user *default-user*))
  (api-list-index-async
   :user user
   :onsuccess
   (lambda (res)
     (setq *list-index*
	   (mapcar #'(lambda (l)
		       (w/json (full_name id_str user.screen_name) l
			 (list full_name user.screen_name id_str)))
		   (json-value res lists))))))

(defun get-subscriptions-async (&optional (user *default-user*))
  (api-list-subscriptions-async
   :user user
   :onsuccess
   (lambda (res)
     (setq *list-subscriptions*
	   (mapcar #'(lambda (l)
		       (w/json (full_name id_str user.screen_name) l
			 (list full_name user.screen_name id_str)))
		   (json-value res lists))))))

(defun user::xyttr-list (list-name)
  (interactive
      (let ((lists (or (append *list-index* *list-subscriptions*)
		       (if (xyttr-init) (all-lists) (quit)))))
	(list (completing-read "List: " (mapcar #'car lists) :must-match t))))
  (let* ((l (assoc list-name *list-subscriptions* :test #'string=))
	 (user (cadr l))
	 (listid (caddr l))
	 (buf (get-timeline-buffer-create (format nil "*tw: ~A" list-name))))
    (when *use-frame*
      (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
			       (new-pseudo-frame *frame-name*))))
    (set-buffer buf)
    (unless (eq buffer-mode 'xyttr-timeline-mode)
      (xyttr-timeline-mode)
      (setf (timeline-mode buffer-timeline) :xyttr-list
	    (timeline-apifunc buffer-timeline) #'api-list-statuses-async
	    (timeline-params buffer-timeline) (list :user user :list_id listid))
      (whenlet *auto-reload*
	  (set-auto-reload buf *auto-reload*)))
    (timeline-reload)
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

(defun send-message ()
  (interactive)
  (w/entry (user.id user.screen_name)
    (let ((status (read-status (format nil "DM to @~A: " user.screen_name))))
      (when (string/= "" status)
	(api-direct-messages-new :user user.screen_name
				 :text status)))))


(defun destroy-status ()
  (interactive)
  (w/entry (id sender recipient)
    (if (and sender recipient)
	(api-direct-messages-destroy-async :id id)
      (api-destroy-async :id id))))


(defun toggle-favorite ()
  (interactive)
  (multiple-value-bind (from status) (entry-point)
    (multiple-value-bind (sfrom sto stag)
	(find-text-attribute :fav :start from :key #'safe-car)
      (when sfrom
	(let ((state (cdr stag))
	      (mk (make-marker)))
	  (set-marker mk sfrom)
	  (labels ((toggle (s)
		     (w/buffer-modifying ()
		       (goto-char (marker-point mk))
		       (delete-char)
		       (insert (if s "★" " "))
		       (apply #'set-text-attribute (1- (point)) (point)
			      (cons :fav s) (if s *favorite-star-style*))
		       (delete-marker mk))
		     (refresh-screen)))
	    (funcall (if state #'api-favorites-destroy-async #'api-favorites-create-async)
		     :id (json-value status :id)
		     :onsuccess (lambda (res) (toggle (not state)))
		     :onfailure
		     (lambda (res status headers)
		       (when (= status 403)
			 (toggle (not state)))))))))))


(defun status-url ()
  (w/entry (id user.screen_name)
    (format nil "http://twitter.com/~A/status/~A" user.screen_name id)))

(defun copy-status-url ()
  (interactive)
  (let ((url (status-url)))
    (copy-to-clipboard url)
    (message "Copied: ~A" url)))

(defun show-irt-status ()
  (interactive)
  (w/entry (id . #0=(in_reply_to_status_id retweeted_status.in_reply_to_status_id))
    (whenlet irtid (or . #0#)
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
      (get 'whilet #1#) 2
      (get 'w/json #1#) 2
      (get 'w/entry #1#) 1
      (get 'w/buffer-modifying #1#) 1
      (get 'define-api #1#) 1
      (get 'define-tl-command #1#) 2)


(provide "xyttr")
