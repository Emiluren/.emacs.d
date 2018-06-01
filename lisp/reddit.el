(defun make-reddit-authentication-request ()
  (let* ((url-request-method "POST")
	 (url-request-data (concat "grant_type=password&username="
				   reddit-username
				   "&password="
				   reddit-password))
	 (url-request-extra-headers
          `(("User-Agent" . ,reddit-bot-name)
            ("Authorization" .
             ,(concat "Basic "
		      (base64-encode-string
		       (concat reddit-client-id ":" reddit-client-secret)))))))
    (url-retrieve-synchronously "https://www.reddit.com/api/v1/access_token" nil t)))

(defun read-reddit-oauth-token ()
  (alist-get 'access_token
	     (with-current-buffer
		 (make-reddit-authentication-request)
	       (goto-char url-http-end-of-headers)
	       (json-read))))

(cl-defun make-reddit-oauth-request (path token)
  (let* ((url-request-method "GET")
	 (url-request-extra-headers
          `(("User-Agent" . "Emmatipate Emacs bot 0.1")
            ("Authorization" . ,(concat "bearer " token))))
	 (url (concat "https://oauth.reddit.com" path)))
    (url-retrieve-synchronously url nil t)))

(defun read-subscriptions (token)
  (with-current-buffer
      (make-reddit-oauth-request "/subreddits/mine.json?limit=100" token)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun alist-get-in (keys alist)
  (while keys
    (setq alist (alist-get (pop keys) alist)))
  alist)

(defun sub->subname (sub)
  (alist-get-in '(data display_name) sub))

(defun subscriptions->subreddits (subs)
  (let ((sub-datas (alist-get-in '(data children) subs)))
    (seq-map #'sub->subname sub-datas)))

;; TODO: get "after value" from response to receive all subreddits instead of just 100
(defun list-all-my-subreddits ()
  (let ((access-token (read-reddit-oauth-token)))
    (subscriptions->subreddits (read-subscriptions access-token))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
