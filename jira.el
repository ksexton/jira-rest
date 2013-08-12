;;; jira.el --- Connect to JIRA issue tracking software

;; Copyright © 2012 Atlassian.
;; Copyright © 2009 Brian Zwahr
;; original Copyright © 2007  Dave Benjamin

;; Authors:
;; Hugh Giddens <hgiddens@atlassian.com>
;; Brian Zwahr <echosa@gmail.com>
;; Dave Benjamin <dave@ramenlabs.com>
;; Version: 0.3.3

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License version 2 as published
;; by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary

;; Provides a major mode for interacting with a JIRA server via the JIRA 5
;; REST API. This is based on the XMLRPC-based mode created by Brian Zwahr
;; and Dave Benjamin.

;; Documentation for the REST API is available at
;; https://developer.atlassian.com/static/rest/jira/5.0.html

;;; Getting started

;; Add the directory containing this file to your load path, customise
;; `jira-instances' (e.g. (setf jira-instances '(("jira.atlassian.com"
;; "https://jira.atlassian.com")))), then M-x
;; jira-list-projects-and-filters.

;;; Configuration

;; The `jira' customisation group has all the configurable settings,
;; including faces. In particular, the `jira-instances' variable
;; controls which JIRA servers will be used, and the credentials for
;; accessing them.

(require 'cl)
(require 'cl-lib)
(require 'json)
(require 'mail-parse)
(require 'rx)
(require 'url)

(defgroup jira nil
  "JIRA customisation group."
  :prefix "jira-"
  :tag "JIRA"
  :group 'applications)

(defgroup jira-faces nil
  "Faces for displaying JIRA information."
  :prefix "jira-"
  :tag "JIRA Faces"
  :group 'jira
  :group 'faces)

(defface jira-issue-info-header-face
  '((t :bold t))
  "Face for issue field names when looking at an issue."
  :group 'jira-faces)

(defface jira-issue-summary-face
  '((t :bold t))
  "Face for displaying the summary when looking at an issue."
  :group 'jira-faces)

(defface jira-comment-header-face
  '((t :bold t))
  "Face for comment headers when looking at an issue."
  :group 'jira-faces)

(defface jira-title-face
  '((t :bold t :foreground "white" :background "#003466" :height 1.5))
  "Face for titles in jira-mode buffers."
  :group 'jira-faces)

(defface jira-heading-face
  '((t :bold t :foreground "#003466" :height 1.2))
  "Face for headings in jira-mode buffers."
  :group 'jira-faces)

(defface jira-zebra-stripe-face
  '((t :background "#f0f0f0"))
  "Face for the overlay for zebra striped rows."
  :group 'jira-faces)

(defvar jira-instance)
(put 'jira-instance 'variable-documentation "The JIRA instance for the current buffer.")
(make-variable-buffer-local 'jira-instance)

(defcustom jira-instances nil
  "Information on known JIRA instances.

Should be a list of instances, where each element is a list of
four values: a description of the server (used in buffer names),
the URL of the JIRA instance (possibly including port), your
username, and your password - e.g. '(\"My instance\"
\"https://foo:8000/jira\" \"user\" \"pass\")."
  :type '(repeat
          (list (string :tag "Description")
                (string :tag "Host name")
                (choice (string :tag "User name") (const :tag "No user name" nil))
                (choice (string :tag "Password") (const :tag "No password" nil))))
  :group 'jira)

(defcustom jira-display-images nil
  "Whether icons (for statuses, users, etc.) should be downloaded
and displayed."
  :type '(choice (const :tag "Display images." t)
                 (const :tag "Do not display images." nil))
  :group 'jira)

(defcustom jira-mode-hook nil
  "Run at the very end of `jira-mode'."
  :group 'jira
  :type 'hook)

(defcustom jira-zebra-stripe-rows nil
  "Whether rows in lists should be zebra-striped."
  :group 'jira
  :type 'boolean)

(defvar jira-request-level 0
  "Used to enforce ordering on actioning async responses.

This stops processing of responses from user actions that are not
the most recent.")
(make-variable-buffer-local 'jira-request-level)

(defvar jira-default-issue nil
  "The 'current' JIRA issue, for some value of current.

This is set buffer-locally by `jira-mode' functions that
e.g. display an issue.")

(defvar jira-revert-buffer nil
  "The function, run by `jira-revert-buffer', to revert the current buffer.")

(define-derived-mode jira-mode special-mode "JIRA"
  "Mode for interacting with JIRA servers.

See also `jira-issue-mode'.

\\{jira-mode-map}"
  :group 'jira

  (set (make-local-variable 'revert-buffer-function) 'jira-revert-buffer)
  (set (make-local-variable 'jira-revert-buffer) nil)
  (set (make-local-variable 'jira-default-issue) nil))

(suppress-keymap jira-mode-map t)
(define-key jira-mode-map "l" 'jira-list-projects-and-filters)
(define-key jira-mode-map "si" 'jira-search-issues)
(define-key jira-mode-map "sp" 'jira-search-project-issues)
(define-key jira-mode-map "i" 'jira-show-issue)
(define-key jira-mode-map "c" 'jira-create-issue)
(define-key jira-mode-map "o" 'jira-comment-issue)
(define-key jira-mode-map "a" 'jira-assign-issue)
(define-key jira-mode-map "w" 'jira-watch-issue)

(define-derived-mode jira-issue-mode jira-mode "JIRA Issue"
  "Mode derived from `jira-mode' for JIRA issues.

\\{jira-issue-mode-map}"
  :group 'jira)

(define-key jira-issue-mode-map "n" 'jira-next-comment)
(define-key jira-issue-mode-map "p" 'jira-previous-comment)
(define-key jira-issue-mode-map "u" 'jira-update-issue-summary)
(define-key jira-issue-mode-map "t" 'jira-update-issue-status)

(defun jira-instance-name (instance)
  (first instance))

(defun jira-instance-url (instance)
  (second instance))

(defun jira-instance-username (instance)
  (third instance))

(defun jira-instance-password (instance)
  (fourth instance))

(defun jira-endpoint-for-instance (instance endpoint)
  (concat (jira-instance-url instance)
          (if (eql (elt instance (1- (length instance))) ?/)
                       (substring endpoint 1)
                     endpoint)))

(defun jira-project-endpoint ()
  "/rest/api/latest/project")

(defun jira-filter-endpoint (filter-id)
  (format "/rest/api/latest/filter/%s" filter-id))

(defun jira-search-endpoint ()
  "/rest/api/latest/search")

(defun jira-issue-endpoint (id-or-key)
  (format "/rest/api/latest/issue/%s" id-or-key))

(defun jira-create-issue-metadata-endpoint ()
  "/rest/api/latest/issue/createmeta")

(defun jira-create-issue-endpoint ()
  "/rest/api/latest/issue")

(defun jira-comment-endpoint (id-or-key)
  (format "/rest/api/latest/issue/%s/comment" id-or-key))

(defun jira-worklog-endpoint (id-or-key)
  (format "/rest/api/latest/issue/%s/worklog" id-or-key))

(defun jira-assignable-user-endpoint (issue-key user-search)
  (format "/rest/api/latest/user/assignable/search?issueKey=%s&username=%s" issue-key user-search))

(defun jira-issue-transitions-endpoint (id-or-key)
  (format "/rest/api/latest/issue/%s/transitions" id-or-key))

(defun jira-server-info-endpoint ()
  "/rest/api/latest/serverInfo")

(defun jira-resolutions-endpoint ()
  "/rest/api/latest/resolution")

(defun jira-watchers-endpoint (id-or-key &optional user-to-delete)
  (format "/rest/api/latest/issue/%s/watchers%s"
          id-or-key
          (if user-to-delete (concat "?username=" user-to-delete) "")))

(defun jira-auth-token (instance)
  (concat "Basic "
          (base64-encode-string (concat (jira-instance-username instance)
                                        ":"
                                        (jira-instance-password instance)))))

(defmacro with-jira-response (&rest body)
  (let ((buffer (gensym)))
    `(let ((,buffer (current-buffer)))
       (unwind-protect
           (progn
             (set-buffer-multibyte t)
             ,@body)
         (kill-buffer ,buffer)))))

(defun jira-skip-header ()
  (goto-char (point-min))
  (while (not (= (point) (line-end-position)))
    (forward-line))
  (forward-line))

(defun jira-parse-document ()
  (let ((json-array-type 'list))
    (json-read)))

(defun jira-parse-response (status)
  (declare (special url-http-response-status))
  (jira-skip-header)
  (when (not (eql (/ url-http-response-status 100) 2))
    (error "Unexpected response code %d" url-http-response-status))
  (unless (= url-http-response-status 204) ;; no content
    (jira-parse-document)))

(defun jira-retrieve-async (url callback args)
  "Wraps url-retrieve to handle Emacs 23 vs. 24 arglist change."
  (apply 'url-retrieve url callback args (if (eql emacs-major-version 23) nil (list t))))

(defun jira-retrieve-sync (url callback args)
  "Allows synchronous requests to be made with the same interface as async."
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((url-http-response-status (url-http-parse-response)))
      (apply callback
             (if (eql (/ url-http-response-status 100) 2)
                 nil
               (list :error (list 'error 'http url-http-response-status)))
             args))))

(defun jira-request (instance url-parameters callback args synchronous)
  "Submits a request to JIRA.

URL-PARAMETERS should be a list of four elements, containing as
its first element the URL to visit and as its second, third, and
fourth elements, values to bind to `url-request-method',
`url-request-extra-headers', and `url-request-data'.

Upon completion of the request, CALLBACK will be applied
to (status . ARGS), with the current buffer containing the
response body.

If SYNCHRONOUS is true, the request is synchronous."
  (destructuring-bind (url url-request-method url-request-extra-headers url-request-data)
      url-parameters
    (when (and (jira-instance-username instance) (jira-instance-password instance))
      (push (cons "Authorization" (jira-auth-token instance)) url-request-extra-headers))
    (funcall (if synchronous 'jira-retrieve-sync 'jira-retrieve-async)
             (jira-endpoint-for-instance instance url)
             (lambda (status callback args)
               (with-jira-response
                (destructuring-bind (&optional error-symbol . data) (plist-get status :error)
                  (when error-symbol
                    (signal error-symbol data)))
                (apply callback (jira-parse-response status) args)))
             (list callback args))))

(defun jira-get (instance url callback &optional args synchronous)
  (jira-request instance (list url nil nil nil) callback args synchronous))

(defun jira-post (instance url data callback &optional args synchronous)
  (jira-request instance (list url "POST" '(("Content-Type" . "application/json")) (json-encode data))
                     callback args synchronous))

(defun jira-put (instance url data callback &optional args synchronous)
  (jira-request instance (list url "PUT" '(("Content-Type" . "application/json")) (json-encode data))
                     callback args synchronous))

(defun jira-delete (instance url data callback &optional args synchronous)
  (jira-request instance (list url "DELETE" '(("Content-Type" . "application/json")) (json-encode data))
                     callback args synchronous))

(defun jira-required-read (prompt &optional completions initial-input)
  (let ((initial (concat prompt ": "))
        (subsequent (concat prompt ": (required): ")))
    (do ((str (if completions
                  (completing-read initial completions nil t initial-input)
                (read-string initial initial-input))
              (if completions
                  (completing-read subsequent completions nil t initial-input)
                (read-string subsequent initial-input))))
        ((not (equal str "")) str))))

(put 'jira-issue
     'thing-at-point
     (lambda ()
       (let ((symbol (thing-at-point 'symbol)))
         (when (and symbol (string-match "^[[:alpha:]]+-[[:digit:]]+$" symbol))
           (substring-no-properties symbol)))))

(defun jira-infer-issue ()
  (or (bound-and-true-p jira-default-issue)
      (thing-at-point 'jira-issue)))

(defun jira-read-issue ()
  (jira-required-read "Issue key"))

(defun jira-infer-instance ()
  (or (bound-and-true-p jira-instance)
      (and (eql (length jira-instances) 1) (first jira-instances))))

(defun jira-read-instance ()
  (assoc (jira-required-read "Instance" (mapcar 'first jira-instances))
         jira-instances))

(defun jira-infer-project-key ()
  (let ((issue (jira-infer-issue)))
    (when issue
      (first (split-string issue "-")))))

(defun jira-compare-versions (left right)
  "Compares two JIRA versions, LEFT and RIGHT."
  ;; TODO: Should handle stuff like Maven.
  ;; TODO: Terrible name.
  (when (stringp left)
    (setf left (mapcar 'read (split-string left "\\."))))
  (when (stringp right)
    (setf right (mapcar 'read (split-string right "\\."))))
  (let ((cl (or (car left) 0))
        (cr (or (car right) 0)))
    (cond ((< cl cr) t)
          ((> cl cr) nil)
          ((or (cdr left) (cdr right)) (jira-compare-versions (cdr left) (cdr right)))
          (t nil))))

(defun jira-list-projects-and-filters (instance)
  "Lists the projects and favourited filters of INSTANCE."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))))
  (switch-to-buffer (format "*%s*" (jira-instance-name instance)))
  (unless (eq major-mode 'jira-mode)
    (jira-mode))
  (incf jira-request-level)
  (setf jira-revert-buffer (list 'jira-list-projects-and-filters instance)
        jira-instance instance)
    (jira-get instance
              (jira-server-info-endpoint)
              'got-server-info
              (list (current-buffer))))

;;; BEGIN
(defun got-server-info (info target-buffer)
  "Got server info"
  (with-current-buffer target-buffer
               (jira-get jira-instance
                         (jira-project-endpoint)
                         'got-projects
                         (list info target-buffer))))

(defun got-projects (projects info target-buffer)
  "Got projects"
             (let ((version-string (cdr (assq 'version info))))
               (if (jira-compare-versions (first (split-string version-string "-")) "5.0")
                   (got-filters nil projects info target-buffer)
                 (with-current-buffer target-buffer
                   (jira-get jira-instance
                             (jira-filter-endpoint "favourite")
                             'got-filters
                             (list projects info target-buffer))))))

(defun got-filters (filters projects info target-buffer)
  "Got filters"
             (let ((row-count 0)
                   avatar-requests)
               (with-jira-buffer target-buffer
                (insert
                 (propertize (concat (cdr (assoc 'serverTitle info)) "\n") 'face 'jira-title-face)
                 "\n"
                 (propertize "Projects:\n" 'face 'jira-heading-face)
                 "\n")
                (dolist (project projects)
                  (let ((row-start (point)))
                    (insert
                     (propertize "  " 'invisible t)
                     (format "%-12s" " "))
                    (beginning-of-line)
                    (when jira-display-images
                      (push (list (cdr (assoc '16x16 (cdr (assoc 'avatarUrls project)))) (point)) avatar-requests))
                    (forward-char 2)
                    (insert-text-button (cdr (assoc 'key project))
                                        'action (lambda (button)
                                                  (jira-search-project-issues
                                                   jira-instance
                                                   (button-get button 'jira-project-key)
                                                   ""))
                                        'follow-link 'mouse-face
                                        'jira-project-key (cdr (assoc 'key project)))
                    (beginning-of-line)
                    (forward-char 12)
                    (insert (cdr (assoc 'name project)) "\n")
                    (when (and jira-zebra-stripe-rows (oddp row-count))
                      (overlay-put (make-overlay row-start (point)) 'face 'jira-zebra-stripe-face)))
                  (incf row-count))

                (insert "\n\n")
                (setf row-count 0)
                (insert (propertize "Filters:\n" 'face 'jira-heading-face) "\n")

                (dolist (filter filters)
                  (let ((row-start (point)))
                    (insert (format "%-8s" " "))
                    (beginning-of-line)
                    (insert-text-button (cdr (assoc 'id filter))
                                        'action (lambda (button)
                                                  (jira-list-issues
                                                   jira-instance
                                                   (button-get button 'jira-filter-key)))
                                        'follow-link 'mouse-face
                                        'jira-filter-key (cdr (assoc 'id filter)))
                    (beginning-of-line)
                    (forward-char 8)
                    (insert (format " %s\n" (cdr (assoc 'name filter))))
                    (when (and jira-zebra-stripe-rows (oddp row-count))
                      (overlay-put (make-overlay row-start (point)) 'face 'jira-zebra-stripe-face)))
                  (incf row-count))
                (jira-load-images-async (nreverse avatar-requests)))))

;;; END




(defun jira-load-images-async (requests)
  (cl-labels ((after-timeout (target-buffer requests request-level)
             (with-current-buffer target-buffer
               (dolist (request requests)
                 (destructuring-bind (url . insertion-points) request
                   (let ((url-request-extra-headers `(("Authorization" . ,(jira-auth-token jira-instance)))))
                     (jira-retrieve-async url 'got-avatar (list target-buffer insertion-points request-level)))))))
           (got-avatar (status target-buffer insertion-points request-level)
             (let ((buffer (current-buffer)))
               (unwind-protect
                   (when (and (null (plist-get status :error))
                              (buffer-live-p target-buffer))
                     (jira-skip-header)
                     (let ((image (condition-case _ (create-image (buffer-substring (point) (point-max)) nil t)
                                    ((error nil)))))
                       (when image
                         (destructuring-bind (_ . props) image
                           (with-current-buffer target-buffer
                             (when (eql request-level jira-request-level)
                               (let ((buffer-read-only nil)
                                     (inhibit-point-motion-hooks t))
                                 (dolist (insertion-point insertion-points)
                                   (goto-char insertion-point)
                                   (remove-text-properties insertion-point (+ insertion-point 2)
                                                           '(invisible nil))
                                   (put-text-property insertion-point (1+ insertion-point)
                                                      'display `(image :ascent center ,@props))
                                   (end-of-line)
                                   ;; XXX This should be smarter. Not decrease the line height if it's
                                   ;; already set larger, and to not have a hardcoded height
                                   (add-text-properties (point) (1+ (point)) '(line-height 17))))))))))
                 (kill-buffer buffer)))))
    (let (optimised)
      (dolist (request requests)
        (destructuring-bind (url position) request
          (let ((record (assoc url optimised)))
            (if record
                (push position (cdr record))
              (push request optimised)))))
      (run-at-time 0 nil 'after-timeout (current-buffer) optimised jira-request-level))))

(defmacro with-jira-buffer (target-buffer &rest body)
  `(with-current-buffer target-buffer
     (let ((buffer-read-only nil)
           (original (point)))
       (delete-region (point-min) (point-max))
       ,@body
       (goto-char (point-max))
       (delete-horizontal-space)
       (goto-char original))))

(defun jira-jql-text-search (query)
  (if (zerop (length query))
      nil
    (let* ((text-search-encoded (replace-regexp-in-string "\\([][+&|!(){}^~*?\\-]\\)" "\\\\\\1" query))
           (backslash-escaped (replace-regexp-in-string "\\\\" "\\\\\\\\" text-search-encoded))
           (quote-escaped (replace-regexp-in-string "\"" "\\\\\"" backslash-escaped)))
      (format "summary ~ \"%s\" OR description ~ \"%s\" OR comment ~ \"%s\""
              quote-escaped quote-escaped quote-escaped))))

(defun jira-issue-needs-enrichment-p (issue)
  (null (assoc 'fields issue)))

(defun jira-enrich-issue (issue instance outstanding callback callback-args)
  ;; We should be using the self link.
  (jira-get instance
            (jira-issue-endpoint (cdr (assoc 'key issue)))
            (lambda (issue-details issue outstanding callback callback-args)
              (unwind-protect
                  (let* ((required-fields '(status priority assignee summary))
                         (field-list (loop for field in (cdr (assoc 'fields issue-details))
                                           when (memq (car field) required-fields)
                                           collect field)))
                    (setf (nthcdr (length issue) issue) `((fields . ,field-list))))
                (when (zerop (decf (car outstanding)))
                  (apply callback callback-args))))
            (list issue outstanding callback callback-args)))

(defun jira-enrich-issues (issue-list instance callback &optional callback-args)
  ;; 4.4 search results are just '((key . "key-1") (self
  ;; . "https://url/to/key-1")); Here we enrich them so they match the
  ;; cool search results we get from 5. There doesn't seem to be a way
  ;; to coerce 4.4 to do this for us with the expand parameter.
  (let ((outstanding (list (length issue-list))))
    (if (not (find-if 'jira-issue-needs-enrichment-p issue-list))
        (apply callback callback-args)
      (dolist (issue issue-list)
        (jira-enrich-issue issue instance outstanding callback callback-args)))))

;;; XXX We seem to be getting back much richer documents here than the documentation says we should be
(defun jira-search-issues (instance text)
  "Displays a list of issues matching TEXT on INSTANCE.

Summary, description, and comment fields are searched."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (read-string "Search: ")))
  (switch-to-buffer (format "*%s Search*" (jira-instance-name instance)))
  (unless (eq major-mode 'jira-mode)
    (jira-mode))
  (incf jira-request-level)
  (setf jira-instance instance
        jira-revert-buffer
        (list 'jira-post
              instance
              (jira-search-endpoint)
              `((jql . ,(or (jira-jql-text-search text) ""))
                (maxResults . 20))
              (lambda (search-results instance text target-buffer)
                (let ((issue-list (cdr (assoc 'issues search-results))))
                  (jira-enrich-issues issue-list
                                      instance
                                      (lambda (issue-list text target-buffer)
                                        (with-jira-buffer target-buffer
                                          (insert (propertize "Issue search\n" 'face 'jira-title-face) "\n")
                                          (unless (zerop (length text))
                                            (insert (propertize "Query text\n" 'face 'jira-heading-face) text "\n\n"))
                                          (jira-display-issues issue-list)))
                                      (list issue-list text target-buffer))))
              (list instance text (current-buffer))))
  (jira-revert-buffer))

(defun jira-search-project-issues (instance project text)
  "Displays a list of issues matching TEXT in PROJECT on INSTANCE.

Summary, description, and comment fields are searched."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (jira-required-read "Project" nil (jira-infer-project-key))
                     (read-string "Query: ")))
  ;; TODO should only request the fields needed?
  ;; TODO should do a completing-read for projects.
  (switch-to-buffer (format "*%s Search*" (jira-instance-name instance)))
  (unless (eq major-mode 'jira-mode)
    (jira-mode))
  (incf jira-request-level)
  (setf jira-instance instance
        jira-revert-buffer
        (list 'jira-post
              instance
              (jira-search-endpoint)
              `((jql . ,(let ((text-query (jira-jql-text-search text)))
                          (if (null text-query)
                              (format "project = \"%s\"" project)
                            (format "project = \"%s\" AND (%s)" project text-query))))
                (maxResults . 20))
              (lambda (search-results project instance text target-buffer)
                (let ((issue-list (cdr (assoc 'issues search-results))))
                  (jira-enrich-issues issue-list
                                      instance
                                      (lambda (issue-list project text target-buffer)
                                        (with-jira-buffer target-buffer
                                          (insert (propertize (concat (upcase project) " project issue search\n")
                                                              'face 'jira-title-face) "\n")
                                          (unless (zerop (length text))
                                            (insert (propertize "Query text\n" 'face 'jira-heading-face) text "\n\n"))
                                          (jira-display-issues issue-list)))
                                      (list issue-list project text target-buffer))))
              (list project instance text (current-buffer))))
  (jira-revert-buffer))

(defmacro def-jira-issue-field (name &optional field-name)
  (when (null field-name)
    (setf field-name name))
  (let ((fun-name (intern (concat "jira-issue-" (symbol-name name)))))
    `(defun ,fun-name (issue)
       (let* ((fields (cdr (assoc 'fields issue)))
              (field (cdr (assoc ',field-name fields)))
              (value (and (listp field) (assoc 'value field))))
         (if value
             (cdr value)
           field)))))

(def-jira-issue-field summary)
(def-jira-issue-field priority)
(def-jira-issue-field status)
(def-jira-issue-field assignee)

;; TODO: look at tabulated-list-mode for this stuff in info node
;; (elisp)Basic Major Modes
(defun jira-display-issues (issues)
  (insert (propertize (format "%d issues found:\n" (length issues)) 'face 'jira-heading-face))
  (let ((row-count 0)
        icon-requests)
    (dolist (issue issues)
      (let* ((key (cdr (assoc 'key issue)))
             (status (jira-issue-status issue))
             (priority (jira-issue-priority issue))
             (assignee (jira-issue-assignee issue))
             (summary (jira-issue-summary issue))
             (row-start (point)))
        (insert (format "%-16s" " "))
        (beginning-of-line)
        (make-text-button (point)
                          (progn
                            (insert key)
                            (point))
                          'action (lambda (button)
                                    (jira-show-issue
                                     jira-instance
                                     (button-get button 'jira-issue-key)))
                          'follow-link 'mouse-face
                          'jira-issue-key key)
        (beginning-of-line)
        (forward-char 16)
        (insert (format "%-15s" (if (null assignee)
                                    ""
                                  (cdr (assoc 'name assignee))))
                " ")
        (when jira-display-images
          (push (list (cdr (assoc 'iconUrl status)) (point))
                icon-requests))
        (insert (format "%-14s" (concat "  " (cdr (assoc 'name status)))) " ")
        (put-text-property (- (point) 15) (- (point) 13) 'invisible t)
        (when (and jira-display-images priority)
          (push (list (cdr (assoc 'iconUrl priority)) (point))
                icon-requests))
        (insert (format  "%-12s" (concat "  "
                                         (if (null priority)
                                             ""
                                           (cdr (assoc 'name priority))))))
        (put-text-property (- (point) 12) (- (point) 10) 'invisible t)
        (insert "\n" summary "\n")
        (when (and jira-zebra-stripe-rows (oddp row-count))
          (overlay-put (make-overlay row-start (point)) 'face 'jira-zebra-stripe-face)))
      (incf row-count))
    (jira-load-images-async (nreverse icon-requests))))

;;; BEGIN
(defun got-filter (filter filter-id instance target-buffer)
             (jira-post instance
                        (jira-search-endpoint)
                        `((jql . ,(format "filter = %s" filter-id)))
                        'got-search-results
                        (list filter filter-id instance target-buffer)))

(defun got-search-results (search-results filter filter-id instance target-buffer)
             (let ((issue-list (cdr (assoc 'issues search-results))))
               (jira-enrich-issues issue-list
                                   instance
                                   'got-enriched-results
                                   (list issue-list filter filter-id target-buffer))))

(defun got-enriched-results (issue-list filter filter-id target-buffer)
             (with-jira-buffer target-buffer
               (insert (propertize (format "Filter: %s (%s)\n"
                                           (cdr (assoc 'name filter))
                                           filter-id)
                                   'face 'jira-title-face)
                       "\n")
               (when (cdr (assoc 'description filter))
                 (insert (propertize "Description\n" 'face 'jira-heading-face)
                         (cdr (assoc 'description filter)) "\n\n"))
               (jira-display-issues issue-list)))

;;; END



(defun jira-list-issues (instance filter-id)
  "Displays a list of issues matching the filter specified by FILTER-ID on INSTANCE."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (read-number "Filter ID: ")))
  ;; TODO should do a completing read for the filter id
  (switch-to-buffer (format "*%s Search*" (jira-instance-name instance)))
  (unless (eq major-mode 'jira-mode)
    (jira-mode))
  (incf jira-request-level)
    ;; TODO: Setting jira-instance should be done as part of the end of the
    ;;       chain of requests rather than their beginning.
    (setf jira-instance instance
          jira-revert-buffer
          (list 'jira-get
                instance
                (jira-filter-endpoint filter-id)
                'got-filter
                (list filter-id instance (current-buffer))))
    (jira-revert-buffer))

(defun jira-format-date (date)
  (let ((r (rx string-start
               (group (= 4 digit)) ?- (group (= 2 digit)) ?- (group (= 2 digit))
               ?T
               (group (= 2 digit)) ?: (group (= 2 digit)) ?: (group (= 2 digit))
               ?. (0+ digit)
               (group (or ?+ ?-)) (group (= 2 digit)) (group (= 2 digit))
               string-end)))
    (if (string-match r date)
        (replace-regexp-in-string
         (rx (or (seq string-start ?0) " 0"))
         (lambda (match) (if (eql (length match) 2) " " ""))
         (format-time-string
          "%I:%M %p %Z, %d %b '%y"
          (encode-time (string-to-number (match-string 6 date))
                       (string-to-number (match-string 5 date))
                       (string-to-number (match-string 4 date))
                       (string-to-number (match-string 3 date))
                       (string-to-number (match-string 2 date))
                       (string-to-number (match-string 1 date))
                       (let ((tz-hours (string-to-number (match-string 8 date)))
                             (tz-minutes (string-to-number (match-string 9 date))))
                         (* (+ (* tz-hours 60) tz-minutes)
                            (if (equal (match-string 7 date) "+") 60 -60))))))
      date)))

(defun jira-item-name (item)
  ;; XXX terrible name
  (cdr (assoc 'name item)))

(def-jira-issue-field components)
(def-jira-issue-field versions)
(def-jira-issue-field fix-versions fixVersions)
(def-jira-issue-field created)
(def-jira-issue-field updated)
(def-jira-issue-field labels)
(def-jira-issue-field description)
(def-jira-issue-field type issuetype)
(def-jira-issue-field resolution)
(def-jira-issue-field reporter)

(defun jira-issue-watch-count (issue)
  (let* ((fields (cdr (assoc 'fields issue)))
         (watches (assoc 'watches fields)))
    (cdr (assoc 'watchCount (cdr (or watches (assoc 'value (cdr (assoc 'watcher fields)))))))))

(defun jira-issue-comments (issue)
  (let* ((fields (cdr (assoc 'fields issue)))
         (comment (cdr (assoc 'comment fields)))
         (comments (assoc 'comments comment)))
    (cdr (if comments
             comments
           (assoc 'value comment)))))

(defun jira-show-issue (instance issue-key)
  "Displays the issue with the specified ISSUE-KEY on INSTANCE."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (jira-required-read "Issue Key" nil (let ((project-key (jira-infer-project-key)))
                                                                (when project-key (concat project-key "-"))))))
  (setf issue-key (upcase issue-key))
  (switch-to-buffer (format "*%s %s*" (jira-instance-name instance) issue-key))
  (unless (eq major-mode 'jira-issue-mode)
    (jira-issue-mode))
  (setf jira-default-issue issue-key)

  ;; TODO It might be cool to do a completing read here, maybe on just the project.
  ;; TODO How does this handle non-existant keys?
  (incf jira-request-level)
  (setf jira-instance instance
        jira-revert-buffer
        (list 'jira-get
              instance
              (jira-issue-endpoint issue-key)
              (lambda (issue issue-key target-buffer)
                (let ((issue-type (jira-issue-type issue))
                      (status (jira-issue-status issue))
                      (priority (jira-issue-priority issue))
                      (assignee (jira-issue-assignee issue))
                      (reporter (jira-issue-reporter issue))
                      (comments (jira-issue-comments issue))
                      (components (mapcar 'jira-item-name (jira-issue-components issue)))
                      (label (jira-issue-labels issue))
                      (affect-versions (mapcar 'jira-item-name (jira-issue-versions issue)))
                      (fix-versions (mapcar 'jira-item-name (jira-issue-fix-versions issue)))
                      icon-requests)
                  (with-jira-buffer target-buffer
                    (insert (propertize (format "%s: %s\n"
                                                (cdr (assoc 'key issue))
                                                (jira-issue-summary issue))
                                        'face 'jira-title-face)
                            "\n")
                    (let* ((fields (list (list "Type"
                                               (jira-item-name issue-type)
                                               (cdr (assoc 'iconUrl issue-type)))
                                         (list "Status" (jira-item-name status) (cdr (assoc 'iconUrl status)))
                                         (list "Resolution"
                                               (jira-item-name (jira-issue-resolution issue)) ;; XXX icon not in json?
                                               )
                                         (list "Priority"
                                               (jira-item-name priority)
                                               (cdr (assoc 'iconUrl priority)))
                                         (list "Assignee"
                                               (cdr (assoc 'displayName assignee))
                                               (cdr (assoc '16x16 (cdr (assoc 'avatarUrls assignee)))))
                                         (list "Reporter"
                                               (cdr (assoc 'displayName reporter))
                                               (cdr (assoc '16x16 (cdr (assoc 'avatarUrls reporter)))))
                                         (list "Created"
                                               (jira-format-date (jira-issue-created issue)))
                                         (list "Updated"
                                               (jira-format-date (jira-issue-updated issue)))
                                         (list "Watchers"
                                               (number-to-string (jira-issue-watch-count issue)))
                                         (list "Components" (mapconcat 'identity components ", "))
                                         (list "Labels" (mapconcat 'identity label ", "))
                                         (list "Affects Versions"
                                               (mapconcat 'identity affect-versions ", "))
                                         (list "Fix Versions"
                                               (mapconcat 'identity fix-versions ", "))))
                           (populated-fields (delete-if (lambda (c) (zerop (length c))) fields
                                                        :key 'second))
                           (max-header-length (loop for field in populated-fields
                                                    maximize (length (car field)))))
                      (dolist (field populated-fields)
                        (destructuring-bind (header content &optional icon) field
                          (insert (propertize header 'face 'jira-issue-info-header-face) ": ")
                          (insert-char ?  (- max-header-length (length header)))
                          (when (and jira-display-images icon)
                            (push (list icon (point)) icon-requests))
                          (insert (propertize "  " 'invisible t)
                                  content "\n"))))
                    (insert "\n" (or (jira-strip-cr (jira-issue-description issue)) "") "\n\n")
                    (let ((count 1))
                      (dolist (comment comments)
                        (let* ((author (cdr (assoc 'author comment)))
                               (icon (cdr (assoc '16x16 (cdr (assoc 'avatarUrls author))))))
                          (insert (propertize
                                   (concat "Comment #" (int-to-string count)
                                           " - ")
                                   'face 'jira-comment-header-face))
                          (when (and jira-display-images icon)
                            (push (list icon (point)) icon-requests))
                          (insert (propertize "  " 'invisible t)
                                  (propertize
                                   (concat (cdr (assoc 'displayName author))
                                           " - "
                                           (jira-format-date (cdr (assoc 'created comment)))
                                           "\n")
                                   'face 'jira-comment-header-face)
                                  (jira-strip-cr (cdr (assoc 'body comment)))
                                  "\n\n"))
                        (incf count)))
                    (jira-load-images-async (nreverse icon-requests)))))
              (list issue-key (current-buffer))))
  (jira-revert-buffer))

(defun jira-strip-cr (string)
  (when string (replace-regexp-in-string "\r" "" string)))

(defun jira-next-comment ()
  "Jumps to the next comment."
  (interactive)
  (let ((p (point)))
    (when (search-forward "Comment #" nil t)
      (when (= p (- (point) 9))
        (search-forward "Comment #" nil t))
      (recenter 0)
      (beginning-of-line))))

(defun jira-previous-comment ()
  "Jumps to the previous comment."
  (interactive)
  (if (search-backward "Comment #" nil t)
      (progn
        (recenter 0)
        (beginning-of-line))
    (goto-char 0)))

(defun jira-create-issue (instance project-id issue-type-id summary description)
  "Creates a new issue on INSTANCE."
  (interactive (let* ((instance (or (jira-infer-instance) (jira-read-instance)))
                      (issue-creation-metadata (jira-get instance
                                                         (jira-create-issue-metadata-endpoint)
                                                         'identity
                                                         nil 'synchronous))
                      ;; XXX I want this to take a project key or a project id.
                      (projects (cdr (assoc 'projects issue-creation-metadata)))
                      (project-key-list (mapcar (lambda (project) (cdr (assoc 'key project))) projects))
                      (project-key (jira-required-read "Project key" project-key-list (jira-infer-project-key)))
                      (project (find project-key projects :test 'equal :key (lambda (project) (cdr (assoc 'key project)))))
                      (project-id (cdr (assoc 'id project)))

                      ;; XXX Same as with project
                      (issue-type-id (let* ((issue-types (cdr (assoc 'issuetypes project)))
                                            (issue-type-names (mapcar (lambda (issue-type) (cdr (assoc 'name issue-type))) issue-types))
                                            (issue-type-name (jira-required-read "Issue type" issue-type-names))
                                            (issue-type (find issue-type-name issue-types :test 'equal :key (lambda (issue-type) (cdr (assoc 'name issue-type))))))
                                       (cdr (assoc 'id issue-type)))))
                 (list instance project-id issue-type-id (jira-required-read "Summary") (read-string "Description: "))))
  ;; TODO: we should have a much more magit commit message style thing here
  (jira-post instance
             (jira-create-issue-endpoint)
             `((fields . ((project . ((id . ,project-id)))
                          (issuetype . ((id . ,issue-type-id)))
                          (summary . ,summary)
                          ,@(unless (equal description "")
                              `((description . ,description))))))
             (lambda (result instance)
               (let ((key (cdr (assoc 'key result))))
                 (jira-show-issue instance key)
                 (message "Created issue %s" key)))
             (list instance)))

(defun jira-revert-buffer (&optional ignore-auto noconfirm)
  "Reverts a jira buffer.

Ignore the arguments as they don't really make sense for us."
  (apply 'funcall jira-revert-buffer))

(defun jira-comment-issue (instance key comment)
  "Adds a new comment with text COMMENT to the issue with the specified KEY."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (or (jira-infer-issue) (jira-read-issue))
                     (jira-required-read "Comment")))
  (jira-post instance
             (jira-comment-endpoint key)
             `((body . ,comment))
             (lambda (_ instance key)
               (jira-show-issue instance key))
             (list instance key)))


(defun jira-worklog-issue (instance key comment time)
  "Adds a new comment with text COMMENT to the issue with the specified KEY."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (or (jira-infer-issue) (jira-read-issue))
                     (jira-required-read "Comment")
		     (jira-required-read "Time (ex: 3h 20m)")))

  (jira-post instance
             (jira-worklog-endpoint key)
	     `((comment . ,comment)
	       (timeSpent . ,time))

             (lambda (_ instance key)
               (jira-show-issue instance key))
             (list instance key)))

(defun jira-assign-issue (instance key assignee)
  "Assigns the issue with specified KEY to ASSIGNEE."
  ;; TODO: assignee name completion shouldn't error when try to complete
  ;;       empty string, despite the api not letting us do that request.
  ;; TODO: assignee completing is case sensitive (can we get around this)
  ;;       (sending a non-case matching user (via confirm) works fine)?
  (interactive (let* ((instance (or (jira-infer-instance) (jira-read-instance)))
                      (key (or (jira-infer-issue) (jira-read-issue)))
                      (completer (completion-table-dynamic
                                  (lambda (prefix)
                                    (jira-get instance
                                              (jira-assignable-user-endpoint key prefix)
                                              (lambda (result)
                                                (loop for user in result as name = (cdr (assoc 'name user))
                                                      when (and (<= (length prefix) (length name))
                                                                (string= prefix (subseq name 0 (length prefix))))
                                                      collect name))
                                              nil 'synchronous)))))
                 (list instance key (completing-read "Assignee: " completer nil 'confirm))))
  (jira-put instance
            (jira-issue-endpoint key)
            `((update . ((assignee . [((set . ((name . ,assignee))))]))))
            (lambda (_ instance key)
              (jira-show-issue instance key))
            (list instance key)))

(defun jira-update-issue-summary (instance key summary)
  "Changes the summary of the issue with KEY to SUMMARY."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (or (jira-infer-issue) (jira-read-issue))
                     (jira-required-read "Summary")))
  (jira-put instance
            (jira-issue-endpoint key)
            `((update . ((summary . [((set . ,summary))]))))
            (lambda (_ instance key)
              (jira-show-issue instance key))
            (list instance key)))

(defun jira-get-issue-transitions (instance issue-key)
  "Finds the transitions that can be performed on ISSUE-KEY in INSTANCE.

This function performs a synchronous request."
  (jira-get instance
            (jira-issue-transitions-endpoint issue-key)
            (lambda (result)
              (let ((transitions (assoc 'transitions result)))
                (if transitions
                    (cdr transitions)
                  result)))
            nil 'synchronous))

;; TODO this and functions like this should accept transition names and transition ids
(defun jira-update-issue-status (instance key transition-name &optional resolution-name)
  "Changes the status of the issue with key KEY via the transition named by TRANSITION-NAME.

If specified, the resolution for the issue is set to
RESOLUTION-NAME. Currently, this function doesn't attempt to
detect which transitions will require setting a resolution, and
instead requires a resolution for the `Close Issue' and `Resolve
Issue' transitions."
  (interactive (let* ((instance (or (jira-infer-instance) (jira-read-instance)))
                      (key (or (jira-infer-issue) (jira-read-issue)))
                      (transition-completer
                       (completion-table-dynamic
                        (lambda (prefix)
                          (loop for transition in (jira-get-issue-transitions instance key)
                                as name = (jira-item-name transition)
                                when (and (<= (length prefix) (length name))
                                          (string= prefix (subseq name 0 (length prefix))))
                                collect name))))
                      (transition-name (jira-required-read "Transition" transition-completer))
                      (resolution
                       (jira-get instance
                                 (jira-issue-endpoint key)
                                 (lambda (issue)
                                   (jira-issue-resolution issue))
                                 nil 'synchronous))
                      (resolution-completer
                       (completion-table-dynamic
                        (lambda (prefix)
                          (condition-case e
                              (jira-get instance
                                        (jira-resolutions-endpoint)
                                        (lambda (result)
                                          (loop for resolution in result as name = (cdr (assoc 'name resolution))
                                                when (and (<= (length prefix) (length name))
                                                          (string= prefix (subseq name 0 (length prefix))))
                                                collect name))
                                        nil 'synchronous)
                            (error
                             (if (equal (cdr e) '(http 405))
                                 ;; We're talking to 4.4 and the endpoint doesn't exist.
                                 nil
                               (signal e)))))))
                      (resolution-name
                       (if (and (null resolution)
                                ;; TODO Presumably there's a better way to do this but the REST
                                ;; API doesn't make clear what it might be.
                                (member transition-name '("Close Issue" "Resolve Issue")))
                           (jira-required-read "Resolution" resolution-completer)
                         nil)))
                 (list instance key transition-name resolution-name)))
  (cl-labels ((got-issue-transitions (transitions instance key transition-name resolution-name)
             (let* ((transition (find transition-name (cdr (assoc 'transitions transitions))
                                      :test 'string=
                                      :key (lambda (transition) (cdr (assoc 'name transition)))))
                    (transition-id (cdr (assoc 'id transition))))
               (if resolution-name
                   (jira-get instance
                             (jira-resolutions-endpoint)
                             'got-resolutions
                             (list instance key transition-id resolution-name))
                 (send-post instance key `((transition . ((id . ,transition-id))))))))
           (got-resolutions (resolutions instance key transition-id resolution-name)
             (let* ((resolution (find resolution-name resolutions
                                      :test 'string=
                                      :key (lambda (transition) (cdr (assoc 'name transition)))))
                    (resolution-id (cdr (assoc 'id resolution))))
               (send-post instance key `((transition . ((id . ,transition-id)))
                                         (fields . ((resolution . ((id . ,resolution-id)))))))))
           (send-post (instance key data)
             (jira-post instance
                        (jira-issue-transitions-endpoint key)
                        data
                        'got-post-response
                        (list instance key)))
           (got-post-response (_ instance key)
             (jira-show-issue instance key)))
    (jira-get instance
              (jira-issue-transitions-endpoint key)
              'got-issue-transitions
              (list instance key transition-name resolution-name))))

(defun jira-watch-issue (instance key &optional stop-watching)
  "Starts watching the issue specified by KEY.

If STOP-WATCHING is true (interactively, if this is called with a
prefix arg), stops watching the issue instead."
  (interactive (list (or (jira-infer-instance) (jira-read-instance))
                     (or (jira-infer-issue) (jira-read-issue))
                     current-prefix-arg))
  (let ((username (jira-instance-username instance)))
    (funcall (if stop-watching 'jira-delete 'jira-post)
             instance
             (if stop-watching
                 (jira-watchers-endpoint key username)
               (jira-watchers-endpoint key))
             username
             (lambda (_ instance key)
               (jira-show-issue instance key))
             (list instance key))))

(defun jira-send-region-as-comment (start end instance issue-key)
  "Adds the contents of the buffer between START and END as a comment on ISSUE-KEY.

Interactively, uses the current region."
  (interactive (list (region-beginning)
                     (region-end)
                     (or (jira-infer-instance) (jira-read-instance))
                     (or (jira-infer-issue) (jira-read-issue))))
  (jira-comment-issue instance issue-key (buffer-substring start end)))


;;; Done

(provide 'jira)
