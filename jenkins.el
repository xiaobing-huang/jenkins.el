;;; jenkins.el --- Minimalistic Jenkins client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Rustem Muslimov

;; Author: Rustem Muslimov <r.muslimov@gmail.com>
;; Keywords: jenkins, convenience
;; Package-Requires: ((dash "2.12") (emacs "24.3") (json "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To set up, please use "M-x customize-group" to customize the
;; "jenkins" options, or just directly define variables as shown below:
;;
;; (setq jenkins-api-token "<api token can be found on user's configure page>")
;; (setq jenkins-url "<jenkins url>")
;; (setq jenkins-username "<your user name>")
;; (setq jenkins-viewname "<viewname>")

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'json)

(defconst jenkins-buffer-name
  "*jenkins: status*"
  "Name of jenkins buffer.")

(defvar jenkins-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'jenkins--call-build-job-from-main-screen)
    (define-key map (kbd "r") 'jenkins--call-rebuild-job-from-main-screen)
    (define-key map (kbd "v") 'jenkins--visit-job-from-main-screen)
    (define-key map (kbd "RET") 'jenkins-enter-job)
    map)
  "Jenkins main screen status mode keymap.")

(defvar jenkins-job-view-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "1") 'jenkins-job-details-toggle)
    (define-key keymap (kbd "g") 'jenkins--refresh-job-from-job-screen)
    (define-key keymap (kbd "b") 'jenkins--call-build-job-from-job-screen)
    (define-key keymap (kbd "r") 'jenkins--call-rebuild-job-from-job-screen)
    (define-key keymap (kbd "v") 'jenkins--visit-job-from-job-screen)
    (define-key keymap (kbd "$") 'jenkins--show-console-output-from-job-screen)
    keymap)
  "Jenkins jobs status mode keymap.")

(defvar jenkins-console-output-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'kill-this-buffer)
    (define-key keymap (kbd "g") 'jenkins--refresh-console-output)
    keymap)
  "Jenkins jobs console output mode keymap.")

(defgroup jenkins nil
  "Interact with a Jenkins CI server."
  :prefix "jenkins-"
  :group 'jenkins)

;; Set up these variables get proper working jenkins.el
(defcustom jenkins-api-token nil
  "API token on user's configure page."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-url nil
  "Jenkins URL. Example http://jenkins.company.com:80/ "
  :type 'string
  :group 'jenkins)

(defcustom jenkins-hostname nil
  "DEPRECATED. Please use jenkins-url instead."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-username nil
  "Username for Jenkins."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-viewname nil
  "View name."
  :type 'string
  :group 'jenkins)

(defcustom jenkins-colwidth-id 3
  "Id column's width on main view."
  :type 'integer
  :group 'jenkins)

(defcustom jenkins-colwidth-name 35
  "Name column's width on main view."
  :type 'integer
  :group 'jenkins)

(defcustom jenkins-colwidth-last-status 20
  "Status column's width on main view."
  :type 'integer
  :group 'jenkins)

(defun jenkins-list-format ()
  "List of columns for main jenkins jobs screen."
  (apply 'vector
         `(("#" ,jenkins-colwidth-id f :pad-right 2 :right-align t :col-source jenkins--render-indicator)
	   ("Name" ,jenkins-colwidth-name t :col-source jenkins--render-name)
	   ("Last success" ,jenkins-colwidth-last-status f :col-source :last-success)
	   ("Last failed" ,jenkins-colwidth-last-status f :col-source :last-failed))
         ))

(defun get-jenkins-url ()
  "This function is for backward compatibility."
  (let ((url (or jenkins-url jenkins-hostname)))
    ;; Ensure URL ends with /.
    (if (string-match-p (rx "/" string-end) url)
        url
      (concat url "/"))))


(defvar *jenkins-jobs-list*
  nil
  "Data retrieved from jenkins for main jenkins screen.")

(defvar jenkins-local-parameters nil)
(defvar jenkins-local-parameter-choices nil)
(defvar jenkins-local-jobs-shown nil)
(defvar jenkins-local-build-number)
(defvar jenkins-local-jobname)

(defun jenkins--render-name (item)
  "Render jobname for main jenkins job ITEM screen."
  (let ((jobname (plist-get item :name))
        (progress (plist-get item :progress)))
    (if progress
        (format "%s %s"
                (propertize (format "%s%%" progress) 'font-lock-face 'warning)
                jobname)
      (format "%s" jobname))))

(defun jenkins-jobs-view-url ()
  "Jenkins url for get list of jobs in queue and their summaries."
  (format (concat
           "%s"
           (if jenkins-viewname "view/%s/" jenkins-viewname "")
           "api/json?depth=2&tree=name,jobs[name,"
           "lastSuccessfulBuild[result,timestamp,duration,id],"
           "lastFailedBuild[result,timestamp,duration,id],"
           "lastBuild[result,executor[progress]],"
           "lastCompletedBuild[result]]"
           )
          (get-jenkins-url) jenkins-viewname))

(defun jenkins-job-url (jobname)
  "JOBNAME url in jenkins."
  (format (concat
           "%sjob/%s/"
           "api/json?depth=1&tree=builds"
           "[number,timestamp,result,url,building,"
           "culprits[fullName]]")
          (get-jenkins-url) jobname))

(defun jenkins--setup-variables ()
  "Ask from user required variables if they not defined yet."
  (unless (or jenkins-hostname jenkins-url)
    (setq jenkins-url (read-from-minibuffer "Jenkins URL: ")))
  (unless jenkins-username
    (setq jenkins-username (read-from-minibuffer "Jenkins username: ")))
  (unless jenkins-api-token
    (setq jenkins-api-token (read-from-minibuffer "Jenkins API Token: "))))

;; models

(defun jenkins--make-job (name result progress last-success last-failed)
  "Define regular jenkins job here."
  (list :name name
        :result result
        :progress progress
        :last-success last-success
        :last-failed last-failed))

(defun jenkins--get-proper-face-for-result (result)
  "Simple function returning proper 'face for jenkins RESULT."
  (let ((facemap (list '("SUCCESS" . 'success)
                       '("FAILURE" . 'error)
                       '("ABORTED" . 'warning))))
    (cdr (assoc result facemap))))

(defun jenkins--render-indicator (job)
  "Special indicator for each JOB on main jenkins window."
  (propertize
   "●" 'font-lock-face
   (jenkins--get-proper-face-for-result
    (plist-get job :result))))

(defun jenkins--convert-jobs-to-tabulated-format ()
  "Use global jenkins-jobs-list prepare data from table."
  (--map
   (list
    (plist-get it :name)
    (apply 'vector
           (-map
            (lambda (column)
              (let* ((args (nthcdr 3 column))
                     (col-source (plist-get args :col-source)))
                (if (functionp col-source)
                    (funcall col-source it)
                  (plist-get it col-source))))
            (jenkins-list-format))))
   (mapcar 'cdr *jenkins-jobs-list*)))

;; parameter handling functions

(defun jenkins--create-parameter-table-row (param)
  "Generate a table row for PARAM with links for choice parameters."
  (let* ((param-name (cdr (assoc 'name param)))
         (param-type (cdr (assoc 'type param)))
         (description (cdr (assoc 'description param)))
         (default-val (cdr (assoc 'value (assoc 'defaultParameterValue param))))
         (choices (cdr (assoc 'choices param)))
         (type-display (cond
                        ((string= param-type "StringParameterDefinition") "string")
                        ((string= param-type "BooleanParameterDefinition") "boolean")
                        ((string= param-type "ChoiceParameterDefinition") "choice")
                        ((string= param-type "TextParameterDefinition") "text")
                        (t "other")))
         ;; (description-with-link (if (and (string= param-type "ChoiceParameterDefinition") choices)
         ;;                            (format "[[*%s-choices][%s (click for choices)]]" param-name description)
         ;;                          description))
         )
    (format "| %s | %s | %s | %s |       |"
            param-name
            type-display
            description
            (or default-val ""))))

(defun jenkins--parameters-buffer-name (jobname)
  "Generate buffer name for JOBNAME parameters."
  (format "*Jenkins Parameters: %s*" jobname))

(defun jenkins--create-parameters-buffer (jobname parameters)
  "Create org-mode buffer for JOBNAME with PARAMETERS input table."
  (let ((buffer-name (jenkins--parameters-buffer-name jobname)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (org-mode)
      (insert (format "# Edit Jenkins Build Parameters for %s\n" jobname))
      (insert "# Press C-c C-c to build with these parameters, C-c C-e to edit parameter value, C-c C-k to cancel.\n#\n\n")
      (insert "| Parameter | Type | Description | Default | Value |\n")
      (insert "|-----------+------+-------------+---------+-------|\n")
      (dolist (param parameters)
        (insert (jenkins--create-parameter-table-row param))
        (insert "\n"))
      (goto-char (point-min))
      (forward-line 4)
      (end-of-line)
      (backward-char 1)
      (org-table-align)
      (setq-local truncate-lines t)
      (setq jenkins-local-jobname jobname)
      (setq-local jenkins-local-parameters parameters)
      (setq-local jenkins-local-parameter-choices
                  (mapcar (lambda (param)
                            (cons (cdr (assoc 'name param))
                                  (append (cdr (assoc 'choices param)) nil)))
                          (cl-remove-if-not (lambda (p)
                                              (string= (cdr (assoc 'type p))
                                                       "ChoiceParameterDefinition"))
                                            parameters)))
      (jenkins--setup-parameters-buffer-keymap jobname parameters)
      (pop-to-buffer buffer-name))))

(defun jenkins--get-parameter-type-by-name (param-name parameters)
  "Get parameter type for PARAM-NAME from PARAMETERS list."
  (let ((param (cl-find-if (lambda (p)
                             (string= (cdr (assoc 'name p)) param-name))
                           parameters)))
    (when param
      (cdr (assoc 'type param)))))

(defun jenkins--get-parameter-choices (param-name parameters)
  "Get choices for PARAM-NAME from PARAMETERS list."
  (let ((param (cl-find-if (lambda (p)
                             (string= (cdr (assoc 'name p)) param-name))
                           parameters)))
    (when param
      (append (cdr (assoc 'choices param)) nil))))

(defun jenkins--parse-parameters-from-org-table ()
  "Parse parameter values from the current org table using org-table functions."
  (when (org-at-table-p)
    (let* ((table-data (org-table-to-lisp))
           (parameters-alist '()))
      ;; Skip header row and separator line
      (dolist (row (cddr table-data))
        (unless (eq row 'hline)
          (let* ((param-name (string-trim (nth 0 row)))
                 (param-default (string-trim (nth 3 row)))
                 (param-value (string-trim (nth 4 row)))
                 (raw-value (if (string-empty-p param-value) param-default param-value))
                 (param-type (jenkins--get-parameter-type-by-name param-name jenkins-local-parameters))
                 (final-value (if (string= param-type "BooleanParameterDefinition")
                                  (cond
                                   ((string= raw-value "t") "true")
                                   ((or (string= raw-value "nil") (string-empty-p raw-value)) "false")
                                   (t raw-value))
                                raw-value)))
            (push (cons param-name final-value) parameters-alist))))
      (reverse parameters-alist))))

(defun jenkins--format-parameters-for-post (parameters-alist)
  "Format PARAMETERS-ALIST for HTTP POST request."
  (mapconcat (lambda (param)
               (format "%s=%s"
                       (url-hexify-string (car param))
                       (url-hexify-string (cdr param))))
             parameters-alist "&"))

(defun jenkins--submit-parameters-and-build (jobname parameters)
  "Submit parameters and build JOBNAME with PARAMETERS."
  (let* ((parameters-alist (jenkins--parse-parameters-from-org-table))
         (url-request-extra-headers (jenkins--get-auth-headers))
         (url-request-method "POST")
         (url-request-data (jenkins--format-parameters-for-post parameters-alist))
         (build-url (format "%sjob/%s/buildWithParameters" (get-jenkins-url) jobname)))
    (when (y-or-n-p (format "Ready to start %s with %d parameters?" jobname (length parameters-alist)))
      (with-current-buffer (url-retrieve-synchronously build-url)
        (message (format "Building %s job with parameters started!" jobname))
        ;; (inspector-inspect (buffer-substring-no-properties (point-min) (point-max))))
        (kill-buffer (current-buffer))))))

(defun jenkins--set-param-value ()
  "Set parameter value based on parameter type."
  (when (and (org-at-table-p)
             (> (org-table-current-line) 1))
    (let* ((param-name (string-trim
                        (substring-no-properties
                         (org-table-get-field 1))))
           (param-type (string-trim
                        (substring-no-properties
                         (org-table-get-field 2))))
           (param-value (string-trim
                         (substring-no-properties
                          (org-table-get-field 5)))))
      (cond
       ((equal param-type "boolean")
        (org-table-get-field 5 (if (string= param-value "t") "" "t")))
       ((equal param-type "string")
        (org-table-get-field 5 (read-string "String value: " param-value)))
       ((equal param-type "choice")
        (let ((choices (cdr (assoc param-name jenkins-local-parameter-choices))))
          (if choices
              (org-table-get-field 5 (completing-read
                                      (format "Choose value for %s: " param-name)
                                      choices nil t param-value))
            (org-table-get-field 5 (read-string "Choice value: " param-value)))))
       ((equal param-type "text")
        (org-table-get-field 5 (read-string "Text value: " param-value))))
      (org-table-align))))

(defun jenkins--setup-parameters-buffer-keymap (jobname parameters)
  "Set up local keymap for parameters buffer with JOBNAME and PARAMETERS."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
                (lambda () (interactive) (jenkins--submit-parameters-and-build jenkins-local-jobname jenkins-local-parameters)))
    (define-key map (kbd "C-c C-e")
                (lambda () (interactive) (jenkins--set-param-value)))
    (define-key map (kbd "C-c C-k")
                (lambda () (interactive) (kill-buffer (current-buffer))))
    (use-local-map map)))

;;; actions

(defun jenkins-enter-job (&optional jobindex)
  "Open each job detalization page, using JOBINDEX."
  (interactive)
  (let ((jobindex (or jobindex (tabulated-list-get-id))))
    (jenkins-job-view jobindex)))

(defun jenkins--parse-time-from (time-since timeitems)
  (let* ((timeitem (car timeitems))
         (extracted-time (mod time-since (cdr timeitem)))
         (rest-time (/ (- time-since extracted-time) (cdr timeitem)))
         )
    (if (cdr timeitems)
        (apply 'list
               (list extracted-time (car timeitem))
               (jenkins--parse-time-from rest-time (cdr timeitems)))
      (list (list time-since (car timeitem)))
      )))

(defun jenkins--time-since-to-text (timestamp)
  "Return beautiful string presenting TIMESTAMP since event."
  (let* ((timeitems
          '(("s" . 60) ("m" . 60)
            ("h" . 24) ("d" . 1)))
         (seconds-since (- (float-time) timestamp))
         (time-pairs (jenkins--parse-time-from seconds-since timeitems))
         )
    (mapconcat
     (lambda (values) (apply 'format "%d%s" values))
     (-take 3 (reverse (--filter (not (= (car it) 0)) time-pairs)))
     ":")))

(defun jenkins--refresh-jobs-list ()
  "Force loading reloading jobs from jenkins and return them formatter for table."
  (jenkins-get-jobs-list)
  (jenkins--convert-jobs-to-tabulated-format))

(defun jenkins--get-auth-headers ()
  "Helper function to setup auth header for jenkins url call."
  `(("Content-Type" . "application/x-www-form-urlencoded")
    ("Authorization" .
     ,(concat
       "Basic "
       (base64-encode-string
        (concat jenkins-username ":" jenkins-api-token) t)))))

(defun jenkins--retrieve-page-as-json (url)
  "Shortcut for jenkins api URL to return valid json."
  (let ((url-request-extra-headers (jenkins--get-auth-headers))
        (json-false nil))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (json-read-from-string (buffer-string)))
    ))

(defun jenkins--extract-time-of-build (x buildname)
  "Helper defun to render timestamps."
  (let ((val (cdr (assoc 'timestamp (assoc buildname x)))))
    (if val (jenkins--time-since-to-text (/ val 1000)) "")))

(defun jenkins-get-jobs-list ()
  "Get list of jobs from jenkins server."
  (setq
   *jenkins-jobs-list*
   (let* ((jobs-url (jenkins-jobs-view-url))
          (raw-data (jenkins--retrieve-page-as-json jobs-url))
          (jobs (cdr (assoc 'jobs raw-data))))
     (--map
      (apply 'list (cdr (assoc 'name it))
             (jenkins--make-job
              (cdr (assoc 'name it))
              (cdr (assoc 'result (assoc 'lastCompletedBuild it)))
              (cdr (assoc 'progress (assoc 'executor (assoc 'lastBuild it))))
              (jenkins--extract-time-of-build it 'lastSuccessfulBuild)
              (jenkins--extract-time-of-build it 'lastFailedBuild)))
      jobs))))

(defun jenkins-get-job-parameters (jobname)
  "Get parameter definitions for JOBNAME from Jenkins API."
  (let* ((params-url (format "%sjob/%s/api/json?tree=property[parameterDefinitions[name,type,description,defaultParameterValue[value],choices]]"
                             (get-jenkins-url) jobname))
         (raw-data (jenkins--retrieve-page-as-json params-url))
         (properties (cdr (assoc 'property raw-data)))
         (param-definitions nil))
    (when properties
      (cl-loop for prop across properties do
               (let ((param-defs (cdr (assoc 'parameterDefinitions prop))))
                 (when param-defs
                   (setq param-definitions (append param-definitions (append param-defs nil))))))
      param-definitions)))

(defun jenkins-get-job-details (jobname)
  "Make to particular JOBNAME call."
  (cl-labels ((retrieve (attr item)
                (cdr (assoc attr item)))
              (convert-item (item)
                (list
                 (retrieve 'number item)
                 :author (let ((culprits (cdr (assoc 'culprits values))))
                           (if (> (length culprits) 0)
                               (cdar (aref culprits 0)) "---"))
                 :url (retrieve 'url item)
                 :timestring (jenkins--time-since-to-text (/ (retrieve 'timestamp item) 1000))
                 :building (retrieve 'building item)
                 :result (retrieve 'result item)))
              (vector-take (N vec)
                (--map
                 (aref vec it)
                 (number-sequence 0 (1- (min  N (length vec)))))))
    (let* (
           (job-url (jenkins-job-url jobname))
           (raw-data (jenkins--retrieve-page-as-json job-url))
           (builds (-map #'convert-item (vector-take 25 (alist-get 'builds raw-data))))
           (latestSuccessful
            (caar (--filter (equal (plist-get (cdr it) :result) "SUCCESS") builds)))
           (latestFailed
            (caar (--filter (equal (plist-get (cdr it) :result) "FAILURE") builds)))
           (latestFinished
            (caar (--filter (equal (plist-get (cdr it) :building) :json-false) builds)))
           )
      (list :name jobname
            :builds builds
            :latestSuccessful latestSuccessful
            :latestFailed latestFailed
            :latestFinished latestFinished
            ))))

;; helpers
(defun jenkins-visit-jenkins-web-page ()
  "Open main jenkins web page using predefined variables."
  (interactive)
  (browse-url (get-jenkins-url)))

(defun jenkins-visit-job (jobname)
  "Open job's webpage using JOBNAME."
  (interactive)
  (browse-url (format "%s/job/%s/" (get-jenkins-url) jobname)))

(defun jenkins-get-console-output (jobname build)
  "Show the console output for the current job"
  (let* ((url-request-extra-headers (jenkins--get-auth-headers))
         (console-buffer (get-buffer-create (format "*jenkins-console-%s-%s*" jobname build)))
         (url (format "%sjob/%s/%s/consoleText" (get-jenkins-url) jobname build))
         (response (with-current-buffer (url-retrieve-synchronously url)
                     (goto-char url-http-end-of-headers)
                     (buffer-substring-no-properties (point) (point-max))))

         )
    (with-current-buffer (get-buffer-create console-buffer)
      (set-buffer-file-coding-system 'unix)
      (read-only-mode -1)    ; make sure buffer is writable
      (erase-buffer)
      (insert (replace-regexp-in-string "\r" "" response))
      (goto-char (point-min))
      (jenkins-console-output-mode)
      (setq-local jenkins-local-jobname jobname)
      (setq-local jenkins-local-build-number build))
    (pop-to-buffer console-buffer)))

(defun jenkins--visit-job-from-main-screen ()
  "Open browser for current job."
  (interactive)
  (jenkins-visit-job (tabulated-list-get-id)))

(defun jenkins--visit-job-from-job-screen ()
  "Open browser for current job."
  (interactive)
  (jenkins-visit-job jenkins-local-jobname))

(defun jenkins--show-console-output-from-job-screen ()
  "Show the console output for the currently selected build"
  (interactive)
  (let* ((props (text-properties-at (point) (current-buffer)))
         (jenkins-tag (member 'jenkins-build-number props))
         (build-number (and jenkins-tag
                            (cadr jenkins-tag)))
         (job-name jenkins-local-jobname))
    (if build-number
        (jenkins-get-console-output job-name build-number)
      (error "Not on a Jenkins build line"))))

(defun jenkins-mode-map-setup-for-evil ()
  "Set up jenkins-mode-map for evil-mode."
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal jenkins-mode-map
      "b" 'jenkins--call-build-job-from-main-screen)
    (evil-define-key 'normal jenkins-mode-map
      (kbd "r") 'jenkins--call-rebuild-job-from-main-screen)
    (evil-define-key 'normal jenkins-mode-map
      (kbd "v") 'jenkins--visit-job-from-main-screen)
    (evil-define-key 'normal jenkins-mode-map
      (kbd "RET") 'jenkins-enter-job)))

(defun jenkins-job-view-mode-map-setup-for-evil ()
  "Set up jenkins-job-view-mode-map for evil-mode."
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal jenkins-job-view-mode-map
      (kbd "1") 'jenkins-job-details-toggle)
    (evil-define-key 'insert jenkins-job-view-mode-map
      (kbd "g") 'jenkins--refresh-job-from-job-screen)
    (evil-define-key 'normal jenkins-job-view-mode-map
      (kbd "b") 'jenkins--call-build-job-from-job-screen)
    (evil-define-key 'normal jenkins-job-view-mode-map
      (kbd "r") 'jenkins--call-rebuild-job-from-job-screen)
    (evil-define-key 'normal jenkins-job-view-mode-map
      (kbd "v") 'jenkins--visit-job-from-job-screen)
    (evil-define-key 'normal jenkins-job-view-mode-map
      (kbd "$") 'jenkins--show-console-output-from-job-screen)))

(defun jenkins-console-output-mode-map-setup-for-evil ()
  "Set up jenkins-console-output-mode-map for evil-mode."
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal jenkins-console-output-mode-map
      (kbd "r") 'jenkins--refresh-console-output)
    (evil-define-key 'normal jenkins-console-output-mode-map
      (kbd "q") 'kill-this-buffer)))

;; emacs major mode funcs and variables
(define-derived-mode jenkins-mode tabulated-list-mode "Jenkins"
  "Special mode for jenkins status buffer."
  (setq truncate-lines t)
  (kill-all-local-variables)
  (jenkins-mode-map-setup-for-evil)
  (setq mode-name "Jenkins")
  (setq major-mode 'jenkins-mode)
  (use-local-map jenkins-mode-map)
  (hl-line-mode 1)
  (setq tabulated-list-format (jenkins-list-format))
  (setq tabulated-list-entries 'jenkins--refresh-jobs-list)
  (tabulated-list-init-header)
  (tabulated-list-print))

(define-derived-mode jenkins-job-view-mode special-mode "jenkins-job"
  "Mode for viewing jenkins job details"
  ;; buffer defaults
  (jenkins-job-view-mode-map-setup-for-evil))

(define-derived-mode jenkins-console-output-mode special-mode "jenkins-console-output"
  "Mode for viewing jenkins console output"
  ;; setup evil mode bindings
  (jenkins-console-output-mode-map-setup-for-evil))

(defun jenkins-job-render (jobname)
  "Render details buffer for JOBNAME."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((job (cdr (assoc jobname *jenkins-jobs-list*))))
    (insert
     (jenkins-job-details-screen jobname)
     ))
  (setq buffer-read-only t))

(defun jenkins-job-view (jobname)
  "Open JOBNAME details screen."
  (interactive)
  (setq jenkins-local-jobs-shown t)
  (let* ((details-buffer-name (format "*jenkins: %s details*" jobname)))
    (with-current-buffer (get-buffer-create details-buffer-name)
      (jenkins-job-render jobname)
      (jenkins-job-view-mode)
      (setq-local jenkins-local-jobname jobname))
    (switch-to-buffer details-buffer-name)))

(defun jenkins-job-details-toggle ()
  "Toggle builds list."
  (interactive)
  (setq-local jenkins-local-jobs-shown (not jenkins-local-jobs-shown))
  (jenkins-job-render jenkins-local-jobname)
  (goto-line 4))

(defun jenkins-job-call-build (jobname)
  "Call jenkins build JOBNAME function."
  (let ((parameters (jenkins-get-job-parameters jobname)))
    (if parameters
        (jenkins--create-parameters-buffer jobname parameters)
      (let ((url-request-extra-headers (jenkins--get-auth-headers))
            (url-request-method "POST")
            (build-url (format "%sjob/%s/build" (get-jenkins-url) jobname)))
        (when (y-or-n-p (format "Ready to start %s?" jobname))
          (with-current-buffer (url-retrieve-synchronously build-url)
            (message (format "Building %s job started!" jobname))))))))

(defun jenkins-job-call-rebuild (jobname)
  "Call jenkins rebuild JOBNAME function."
  (let ((parameters (jenkins-get-job-parameters jobname)))
    (if parameters
        (jenkins--create-parameters-buffer jobname parameters)
      (let ((url-request-extra-headers (jenkins--get-auth-headers))
            (url-request-method "GET")
            (build-url (format "%sjob/%s/lastCompletedBuild/rebuild/" (get-jenkins-url) jobname)))
        (when (y-or-n-p (format "Ready to rebuild %s?" jobname))
          (with-current-buffer (url-retrieve-synchronously build-url)
            (message (format "Building %s job started!" jobname))))))))

(defun jenkins--call-build-job-from-main-screen ()
  "Build job from main screen."
  (interactive)
  (jenkins-job-call-build (tabulated-list-get-id)))

(defun jenkins--call-build-job-from-job-screen ()
  "Call building job from job details in jenkins."
  (interactive)
  (jenkins-job-call-build jenkins-local-jobname))

(defun jenkins--call-rebuild-job-from-main-screen ()
  "rebuild job from main screen."
  (interactive)
  (jenkins-job-call-rebuild (tabulated-list-get-id)))

(defun jenkins--call-rebuild-job-from-job-screen ()
  "Call rebuilding job from job details in jenkins."
  (interactive)
  (jenkins-job-call-rebuild jenkins-local-jobname))

(defun jenkins--refresh-job-from-job-screen ()
  "Refresh the current job"
  (interactive)
  (jenkins-job-render jenkins-local-jobname))

(defun jenkins--refresh-console-output ()
  "Refresh the console output for the current build."
  (interactive)
  (when (and jenkins-local-jobname jenkins-local-build-number)
    (let ((url-request-extra-headers (jenkins--get-auth-headers))
          (url (format "%sjob/%s/%s/consoleText" (get-jenkins-url) jenkins-local-jobname jenkins-local-build-number))
          (buf (current-buffer)))
      (save-excursion
        (read-only-mode -1)
        ;; (erase-buffer)
        (with-current-buffer (url-retrieve-synchronously url)
          (goto-char url-http-end-of-headers)
          (forward-line)
          (copy-to-buffer buf (point) (point-max)))
        (read-only-mode 1))
      (message "Console output refreshed for %s #%s" jenkins-local-jobname jenkins-local-build-number))))

(defun jenkins-job-details-screen (jobname)
  "Jenkins job detailization screen, JOBNAME."
  (let* ((job-details (jenkins-get-job-details jobname))
         (jobname (plist-get job-details :name))
         (builds (plist-get job-details :builds))
         (latest (assoc (plist-get job-details :latestFinished) builds))
         (latest-result (plist-get (cdr latest) :result))
         (latestSuccessful
          (cdr (assoc (plist-get job-details :latestSuccessful) builds)))
         )
    (concat
     (format "Job name:\t%s\n" jobname)
     "Status:\t\t"
     (propertize
      (format "%s\n\n" latest-result)
      'face (jenkins--get-proper-face-for-result latest-result))
     (propertize
      (concat
       (format
        "Latest %s builds: "
        (length builds))
       (propertize ";; (press 1 to toggle)\n" 'font-lock-face 'italic)
       (if jenkins-local-jobs-shown
           (apply 'concat
                  (--map
                   (propertize
                    (format "- Job #%s, %s %s\n"
                            (car it)
                            (plist-get (cdr it) :author)
                            (plist-get (cdr it) :timestring)
                            )
                    'jenkins-build-number
                    (car it)
                    'face
                    (jenkins--get-proper-face-for-result
                     (plist-get (cdr it) :result)
                     ))
                   builds)))))
     "\nBuild now! "
     (propertize ";; (press b to Build)\n" 'font-lock-face 'italic)
     "Rebuild last run! "
     (propertize ";; (press r to Rebuild)\n" 'font-lock-face 'italic)
     "View job's page "
     (propertize ";; (press v to open browser)\n" 'font-lock-face 'italic)
     "View job's console output"
     (propertize ";; (press $ to open a new buffer with the text log; press g in console to refresh)\n" 'font-lock-face 'italic)
     )))

;;;###autoload
(defun jenkins ()
  "Initialize jenkins buffer."
  (interactive)
  (jenkins--setup-variables)
  (pop-to-buffer jenkins-buffer-name)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq buffer-read-only t)
  (jenkins-mode))


(provide 'jenkins)
;;; jenkins.el ends here
