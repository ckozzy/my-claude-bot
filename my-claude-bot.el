;;; my-claude-bot.el --- GitHub App integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: vc, tools
;; URL: https://github.com/ckozzy/my-claude-bot

;;; Commentary:

;; This package provides GitHub App integration for Emacs, allowing you to
;; interact with GitHub's API using GitHub App authentication.
;;
;; Setup:
;; 1. Create a GitHub App at https://github.com/settings/apps
;; 2. Download the private key (.pem file)
;; 3. Configure the app settings in your Emacs config:
;;
;;    (setq my-claude-bot-app-id "YOUR_APP_ID")
;;    (setq my-claude-bot-private-key-file "/path/to/private-key.pem")
;;    (setq my-claude-bot-installation-id "YOUR_INSTALLATION_ID")
;;
;; Usage:
;; M-x my-claude-bot-test-connection to verify your setup

;;; Code:

(require 'json)
(require 'url)

;;; Customization

(defgroup my-claude-bot nil
  "GitHub App integration for Emacs."
  :group 'tools
  :prefix "my-claude-bot-")

(defcustom my-claude-bot-app-id nil
  "GitHub App ID.
You can find this in your GitHub App settings."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "App ID"))
  :group 'my-claude-bot)

(defcustom my-claude-bot-private-key-file nil
  "Path to the GitHub App private key (.pem file).
Keep this file secure and never commit it to version control."
  :type '(choice (const :tag "Not set" nil)
                 (file :tag "Private key file"))
  :group 'my-claude-bot)

(defcustom my-claude-bot-installation-id nil
  "GitHub App installation ID.
This identifies which installation of your app to use."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Installation ID"))
  :group 'my-claude-bot)

(defcustom my-claude-bot-api-url "https://api.github.com"
  "GitHub API base URL."
  :type 'string
  :group 'my-claude-bot)

;;; Internal variables

(defvar my-claude-bot--access-token nil
  "Cached installation access token.")

(defvar my-claude-bot--token-expiry nil
  "Expiry time of the cached access token.")

(defvar my-claude-bot--script-path
  (expand-file-name "github-app-token.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the GitHub App token generation script.")

;;; Helper functions

(defun my-claude-bot--get-access-token ()
  "Get or refresh the installation access token using Python script."
  ;; Check if we have a cached token that hasn't expired (with 5 minute buffer)
  (when (and my-claude-bot--access-token
             my-claude-bot--token-expiry
             (time-less-p (current-time)
                         (time-subtract my-claude-bot--token-expiry (seconds-to-time 300))))
    (return my-claude-bot--access-token))

  ;; Validate configuration
  (unless my-claude-bot-app-id
    (user-error "GitHub App ID not configured. Set `my-claude-bot-app-id'"))
  (unless my-claude-bot-private-key-file
    (user-error "GitHub App private key file not configured. Set `my-claude-bot-private-key-file'"))
  (unless (file-exists-p my-claude-bot-private-key-file)
    (user-error "Private key file not found: %s" my-claude-bot-private-key-file))
  (unless my-claude-bot-installation-id
    (user-error "GitHub App installation ID not configured. Set `my-claude-bot-installation-id'"))

  ;; Generate new token using Python script
  (let* ((command (format "python3 '%s' --app-id '%s' --installation-id '%s' --private-key '%s'"
                         my-claude-bot--script-path
                         my-claude-bot-app-id
                         my-claude-bot-installation-id
                         (expand-file-name my-claude-bot-private-key-file)))
         (token (string-trim (shell-command-to-string command))))

    (when (or (string-empty-p token)
              (string-prefix-p "Error:" token)
              (string-prefix-p "Traceback" token))
      (user-error "Failed to generate access token: %s" token))

    ;; Cache the token (GitHub App installation tokens expire in 1 hour)
    (setq my-claude-bot--access-token token)
    (setq my-claude-bot--token-expiry (time-add (current-time) (seconds-to-time 3600)))

    token))

;;; API functions

(defun my-claude-bot-request (method endpoint &optional data)
  "Make an authenticated request to GitHub API.
METHOD is the HTTP method (GET, POST, etc.)
ENDPOINT is the API endpoint (e.g., \"/repos/owner/repo/issues\")
DATA is optional request body (will be JSON-encoded)."
  (let* ((url (concat my-claude-bot-api-url endpoint))
         (url-request-method method)
         (url-request-extra-headers
          `(("Accept" . "application/vnd.github+json")
            ("Authorization" . ,(format "Bearer %s" (my-claude-bot--get-access-token)))
            ("X-GitHub-Api-Version" . "2022-11-28")
            ("Content-Type" . "application/json")))
         (url-request-data (when data (json-encode data))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point-min) (point))
      (json-read))))

;;; Interactive commands

;;;###autoload
(defun my-claude-bot-get-repo-info (owner repo)
  "Get information about a GitHub repository.
OWNER is the repository owner.
REPO is the repository name."
  (interactive "sRepository owner: \nsRepository name: ")
  (let ((data (my-claude-bot-request "GET" (format "/repos/%s/%s" owner repo))))
    (message "Repository: %s" (alist-get 'full_name data))
    (message "Description: %s" (alist-get 'description data))
    (message "Stars: %d" (alist-get 'stargazers_count data))
    data))

;;;###autoload
(defun my-claude-bot-test-connection ()
  "Test the GitHub App connection."
  (interactive)
  (condition-case err
      (progn
        (message "Testing GitHub App connection...")
        (message "App ID: %s" my-claude-bot-app-id)
        (message "Private key file: %s" my-claude-bot-private-key-file)
        (message "Installation ID: %s" my-claude-bot-installation-id)

        (unless (file-exists-p my-claude-bot-private-key-file)
          (user-error "Private key file not found: %s" my-claude-bot-private-key-file))
        (message "✓ Private key file found")

        ;; Test actual API connection
        (message "Making test API request...")
        (let ((response (my-claude-bot-request "GET" "/app")))
          (message "✓ Successfully authenticated as GitHub App: %s"
                   (alist-get 'name response))
          (message "✓ Connection test passed!")
          response))
    (error
     (message "✗ Connection test failed: %s" (error-message-string err))
     nil)))

;;; Issue Management

;;;###autoload
(defun my-claude-bot-list-issues (owner repo &optional state)
  "List issues for a repository.
OWNER is the repository owner.
REPO is the repository name.
STATE is the issue state: open, closed, or all (default: open)."
  (interactive "sRepository owner: \nsRepository name: \nsState (open/closed/all) [open]: ")
  (let* ((state (if (string-empty-p state) "open" state))
         (endpoint (format "/repos/%s/%s/issues?state=%s" owner repo state))
         (issues (my-claude-bot-request "GET" endpoint))
         (buf (get-buffer-create "*GitHub Issues*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Issues for %s/%s (%s)\n\n" owner repo state))
      (dolist (issue issues)
        (insert (format "#%d: %s\n"
                       (alist-get 'number issue)
                       (alist-get 'title issue)))
        (insert (format "    State: %s | User: %s\n"
                       (alist-get 'state issue)
                       (alist-get 'login (alist-get 'user issue))))
        (insert (format "    URL: %s\n\n"
                       (alist-get 'html_url issue))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    issues))

;;;###autoload
(defun my-claude-bot-create-issue (owner repo title body)
  "Create a new issue in a repository.
OWNER is the repository owner.
REPO is the repository name.
TITLE is the issue title.
BODY is the issue body/description."
  (interactive "sRepository owner: \nsRepository name: \nsIssue title: \nsIssue body: ")
  (let* ((endpoint (format "/repos/%s/%s/issues" owner repo))
         (data `((title . ,title)
                (body . ,body)))
         (issue (my-claude-bot-request "POST" endpoint data)))
    (message "Created issue #%d: %s"
             (alist-get 'number issue)
             (alist-get 'html_url issue))
    issue))

;;;###autoload
(defun my-claude-bot-comment-on-issue (owner repo issue-number comment)
  "Add a comment to an issue.
OWNER is the repository owner.
REPO is the repository name.
ISSUE-NUMBER is the issue number.
COMMENT is the comment text."
  (interactive "sRepository owner: \nsRepository name: \nnIssue number: \nsComment: ")
  (let* ((endpoint (format "/repos/%s/%s/issues/%d/comments" owner repo issue-number))
         (data `((body . ,comment)))
         (result (my-claude-bot-request "POST" endpoint data)))
    (message "Comment added to issue #%d" issue-number)
    result))

;;;###autoload
(defun my-claude-bot-close-issue (owner repo issue-number)
  "Close an issue.
OWNER is the repository owner.
REPO is the repository name.
ISSUE-NUMBER is the issue number."
  (interactive "sRepository owner: \nsRepository name: \nnIssue number: ")
  (let* ((endpoint (format "/repos/%s/%s/issues/%d" owner repo issue-number))
         (data '((state . "closed")))
         (result (my-claude-bot-request "PATCH" endpoint data)))
    (message "Closed issue #%d" issue-number)
    result))

;;; Pull Request Operations

;;;###autoload
(defun my-claude-bot-list-prs (owner repo &optional state)
  "List pull requests for a repository.
OWNER is the repository owner.
REPO is the repository name.
STATE is the PR state: open, closed, or all (default: open)."
  (interactive "sRepository owner: \nsRepository name: \nsState (open/closed/all) [open]: ")
  (let* ((state (if (string-empty-p state) "open" state))
         (endpoint (format "/repos/%s/%s/pulls?state=%s" owner repo state))
         (prs (my-claude-bot-request "GET" endpoint))
         (buf (get-buffer-create "*GitHub Pull Requests*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Pull Requests for %s/%s (%s)\n\n" owner repo state))
      (dolist (pr prs)
        (insert (format "#%d: %s\n"
                       (alist-get 'number pr)
                       (alist-get 'title pr)))
        (insert (format "    State: %s | User: %s\n"
                       (alist-get 'state pr)
                       (alist-get 'login (alist-get 'user pr))))
        (insert (format "    %s -> %s\n"
                       (alist-get 'ref (alist-get 'head pr))
                       (alist-get 'ref (alist-get 'base pr))))
        (insert (format "    URL: %s\n\n"
                       (alist-get 'html_url pr))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    prs))

;;;###autoload
(defun my-claude-bot-create-pr (owner repo title head base body)
  "Create a new pull request.
OWNER is the repository owner.
REPO is the repository name.
TITLE is the PR title.
HEAD is the branch with your changes.
BASE is the branch you want to merge into.
BODY is the PR description."
  (interactive "sRepository owner: \nsRepository name: \nsPR title: \nsHead branch (your changes): \nsBase branch (merge into): \nsPR body: ")
  (let* ((endpoint (format "/repos/%s/%s/pulls" owner repo))
         (data `((title . ,title)
                (head . ,head)
                (base . ,base)
                (body . ,body)))
         (pr (my-claude-bot-request "POST" endpoint data)))
    (message "Created PR #%d: %s"
             (alist-get 'number pr)
             (alist-get 'html_url pr))
    pr))

;;;###autoload
(defun my-claude-bot-merge-pr (owner repo pr-number)
  "Merge a pull request.
OWNER is the repository owner.
REPO is the repository name.
PR-NUMBER is the pull request number."
  (interactive "sRepository owner: \nsRepository name: \nnPR number: ")
  (when (yes-or-no-p (format "Merge PR #%d?" pr-number))
    (let* ((endpoint (format "/repos/%s/%s/pulls/%d/merge" owner repo pr-number))
           (result (my-claude-bot-request "PUT" endpoint nil)))
      (message "Merged PR #%d" pr-number)
      result)))

;;;###autoload
(defun my-claude-bot-review-pr (owner repo pr-number event comment)
  "Review a pull request.
OWNER is the repository owner.
REPO is the repository name.
PR-NUMBER is the pull request number.
EVENT is the review event: APPROVE, REQUEST_CHANGES, or COMMENT.
COMMENT is the review comment."
  (interactive "sRepository owner: \nsRepository name: \nnPR number: \nsEvent (APPROVE/REQUEST_CHANGES/COMMENT): \nsReview comment: ")
  (let* ((endpoint (format "/repos/%s/%s/pulls/%d/reviews" owner repo pr-number))
         (data `((event . ,event)
                (body . ,comment)))
         (result (my-claude-bot-request "POST" endpoint data)))
    (message "Review submitted for PR #%d" pr-number)
    result))

;;; Repository Browsing

;;;###autoload
(defun my-claude-bot-list-repos ()
  "List repositories accessible to this GitHub App installation."
  (interactive)
  (let* ((endpoint "/installation/repositories")
         (response (my-claude-bot-request "GET" endpoint))
         (repos (alist-get 'repositories response))
         (buf (get-buffer-create "*GitHub Repositories*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Accessible Repositories\n\n")
      (dolist (repo repos)
        (insert (format "%s\n" (alist-get 'full_name repo)))
        (insert (format "    %s\n" (or (alist-get 'description repo) "No description")))
        (insert (format "    Stars: %d | Forks: %d | Language: %s\n"
                       (alist-get 'stargazers_count repo)
                       (alist-get 'forks_count repo)
                       (or (alist-get 'language repo) "N/A")))
        (insert (format "    URL: %s\n\n" (alist-get 'html_url repo))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    repos))

;;;###autoload
(defun my-claude-bot-search-code (query)
  "Search code across repositories.
QUERY is the search query (GitHub code search syntax)."
  (interactive "sSearch query: ")
  (let* ((endpoint (format "/search/code?q=%s" (url-hexify-string query)))
         (response (my-claude-bot-request "GET" endpoint))
         (items (alist-get 'items response))
         (buf (get-buffer-create "*GitHub Code Search*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Code Search Results for: %s\n\n" query))
      (insert (format "Total: %d results\n\n" (alist-get 'total_count response)))
      (dolist (item items)
        (insert (format "%s\n" (alist-get 'name item)))
        (insert (format "    Repository: %s\n"
                       (alist-get 'full_name (alist-get 'repository item))))
        (insert (format "    Path: %s\n" (alist-get 'path item)))
        (insert (format "    URL: %s\n\n" (alist-get 'html_url item))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    items))

;;;###autoload
(defun my-claude-bot-get-file-contents (owner repo path &optional ref)
  "Get the contents of a file from a repository.
OWNER is the repository owner.
REPO is the repository name.
PATH is the file path.
REF is the branch/tag/commit (default: default branch)."
  (interactive "sRepository owner: \nsRepository name: \nsFile path: \nsBranch/tag/commit [default]: ")
  (let* ((ref-param (if (string-empty-p ref) "" (format "?ref=%s" ref)))
         (endpoint (format "/repos/%s/%s/contents/%s%s" owner repo path ref-param))
         (response (my-claude-bot-request "GET" endpoint))
         (content (base64-decode-string (alist-get 'content response)))
         (buf (get-buffer-create (format "*%s/%s:%s*" owner repo path))))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (view-mode))
    (switch-to-buffer buf)
    content))

;;; Commit Operations

;;;###autoload
(defun my-claude-bot-list-commits (owner repo &optional sha path)
  "List commits in a repository.
OWNER is the repository owner.
REPO is the repository name.
SHA is the branch/tag/commit to start from (optional).
PATH is a file path to filter commits (optional)."
  (interactive "sRepository owner: \nsRepository name: \nsBranch/SHA [default]: \nsFile path filter [none]: ")
  (let* ((params (concat
                  (if (and sha (not (string-empty-p sha)))
                      (format "?sha=%s" sha) "")
                  (if (and path (not (string-empty-p path)))
                      (format "%spath=%s"
                             (if (and sha (not (string-empty-p sha))) "&" "?")
                             path) "")))
         (endpoint (format "/repos/%s/%s/commits%s" owner repo params))
         (commits (my-claude-bot-request "GET" endpoint))
         (buf (get-buffer-create "*GitHub Commits*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Commits for %s/%s\n\n" owner repo))
      (dolist (commit commits)
        (let ((commit-data (alist-get 'commit commit)))
          (insert (format "%s\n"
                         (substring (alist-get 'sha commit) 0 7)))
          (insert (format "    %s\n"
                         (alist-get 'message commit-data)))
          (insert (format "    Author: %s\n"
                         (alist-get 'name (alist-get 'author commit-data))))
          (insert (format "    Date: %s\n"
                         (alist-get 'date (alist-get 'author commit-data))))
          (insert (format "    URL: %s\n\n"
                         (alist-get 'html_url commit)))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    commits))

;;;###autoload
(defun my-claude-bot-get-commit (owner repo sha)
  "Get details of a specific commit.
OWNER is the repository owner.
REPO is the repository name.
SHA is the commit SHA."
  (interactive "sRepository owner: \nsRepository name: \nsCommit SHA: ")
  (let* ((endpoint (format "/repos/%s/%s/commits/%s" owner repo sha))
         (commit (my-claude-bot-request "GET" endpoint))
         (buf (get-buffer-create (format "*Commit %s*" (substring sha 0 7)))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Commit: %s\n\n" (alist-get 'sha commit)))
      (let ((commit-data (alist-get 'commit commit)))
        (insert (format "Message:\n%s\n\n" (alist-get 'message commit-data)))
        (insert (format "Author: %s <%s>\n"
                       (alist-get 'name (alist-get 'author commit-data))
                       (alist-get 'email (alist-get 'author commit-data))))
        (insert (format "Date: %s\n\n"
                       (alist-get 'date (alist-get 'author commit-data)))))
      (insert (format "Stats:\n"))
      (insert (format "  Additions: %d\n" (alist-get 'additions (alist-get 'stats commit))))
      (insert (format "  Deletions: %d\n" (alist-get 'deletions (alist-get 'stats commit))))
      (insert (format "  Total: %d\n\n" (alist-get 'total (alist-get 'stats commit))))
      (insert "Files changed:\n")
      (dolist (file (alist-get 'files commit))
        (insert (format "  %s (+%d -%d)\n"
                       (alist-get 'filename file)
                       (alist-get 'additions file)
                       (alist-get 'deletions file))))
      (insert (format "\nURL: %s\n" (alist-get 'html_url commit)))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    commit))

;;; Workflow Automation

;;;###autoload
(defun my-claude-bot-list-workflows (owner repo)
  "List GitHub Actions workflows for a repository.
OWNER is the repository owner.
REPO is the repository name."
  (interactive "sRepository owner: \nsRepository name: ")
  (let* ((endpoint (format "/repos/%s/%s/actions/workflows" owner repo))
         (response (my-claude-bot-request "GET" endpoint))
         (workflows (alist-get 'workflows response))
         (buf (get-buffer-create "*GitHub Workflows*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Workflows for %s/%s\n\n" owner repo))
      (dolist (workflow workflows)
        (insert (format "%s (ID: %d)\n"
                       (alist-get 'name workflow)
                       (alist-get 'id workflow)))
        (insert (format "    Path: %s\n" (alist-get 'path workflow)))
        (insert (format "    State: %s\n" (alist-get 'state workflow)))
        (insert (format "    URL: %s\n\n" (alist-get 'html_url workflow))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    workflows))

;;;###autoload
(defun my-claude-bot-trigger-workflow (owner repo workflow-id ref)
  "Trigger a GitHub Actions workflow.
OWNER is the repository owner.
REPO is the repository name.
WORKFLOW-ID is the workflow ID or filename.
REF is the branch/tag to run the workflow on."
  (interactive "sRepository owner: \nsRepository name: \nsWorkflow ID or filename: \nsBranch/tag: ")
  (let* ((endpoint (format "/repos/%s/%s/actions/workflows/%s/dispatches"
                          owner repo workflow-id))
         (data `((ref . ,ref)))
         (result (my-claude-bot-request "POST" endpoint data)))
    (message "Workflow triggered on %s" ref)
    result))

;;;###autoload
(defun my-claude-bot-list-workflow-runs (owner repo &optional workflow-id)
  "List workflow runs for a repository.
OWNER is the repository owner.
REPO is the repository name.
WORKFLOW-ID is optional workflow ID to filter by."
  (interactive "sRepository owner: \nsRepository name: \nsWorkflow ID (optional): ")
  (let* ((endpoint (if (and workflow-id (not (string-empty-p workflow-id)))
                       (format "/repos/%s/%s/actions/workflows/%s/runs" owner repo workflow-id)
                     (format "/repos/%s/%s/actions/runs" owner repo)))
         (response (my-claude-bot-request "GET" endpoint))
         (runs (alist-get 'workflow_runs response))
         (buf (get-buffer-create "*GitHub Workflow Runs*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Workflow Runs for %s/%s\n\n" owner repo))
      (dolist (run runs)
        (insert (format "%s - Run #%d\n"
                       (alist-get 'name run)
                       (alist-get 'run_number run)))
        (insert (format "    Status: %s | Conclusion: %s\n"
                       (alist-get 'status run)
                       (or (alist-get 'conclusion run) "N/A")))
        (insert (format "    Branch: %s | Event: %s\n"
                       (alist-get 'head_branch run)
                       (alist-get 'event run)))
        (insert (format "    URL: %s\n\n" (alist-get 'html_url run))))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    runs))

;;;###autoload
(defun my-claude-bot-check-commit-status (owner repo ref)
  "Check the status of checks for a commit.
OWNER is the repository owner.
REPO is the repository name.
REF is the commit SHA, branch, or tag."
  (interactive "sRepository owner: \nsRepository name: \nsCommit/branch/tag: ")
  (let* ((endpoint (format "/repos/%s/%s/commits/%s/status" owner repo ref))
         (status (my-claude-bot-request "GET" endpoint))
         (buf (get-buffer-create "*GitHub Commit Status*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Status for %s/%s @ %s\n\n" owner repo ref))
      (insert (format "Overall State: %s\n\n" (alist-get 'state status)))
      (insert (format "Statuses:\n"))
      (dolist (s (alist-get 'statuses status))
        (insert (format "  %s: %s\n"
                       (alist-get 'context s)
                       (alist-get 'state s)))
        (insert (format "    Description: %s\n"
                       (or (alist-get 'description s) "N/A")))
        (when (alist-get 'target_url s)
          (insert (format "    URL: %s\n" (alist-get 'target_url s))))
        (insert "\n"))
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buf)
    status))

(provide 'my-claude-bot)
;;; my-claude-bot.el ends here
