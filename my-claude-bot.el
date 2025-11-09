;;; my-claude-bot.el --- GitHub App integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (jwt "0.1.0"))
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
(require 'auth-source)

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

;;; Helper functions

(defun my-claude-bot--read-private-key ()
  "Read the private key from file."
  (unless my-claude-bot-private-key-file
    (user-error "GitHub App private key file not configured. Set `my-claude-bot-private-key-file'"))
  (unless (file-exists-p my-claude-bot-private-key-file)
    (user-error "Private key file not found: %s" my-claude-bot-private-key-file))
  (with-temp-buffer
    (insert-file-contents my-claude-bot-private-key-file)
    (buffer-string)))

(defun my-claude-bot--generate-jwt ()
  "Generate a JWT for GitHub App authentication.
Note: This is a simplified version. For production use, consider using
the `jwt' package or calling an external tool like `github-app-token'."
  (unless my-claude-bot-app-id
    (user-error "GitHub App ID not configured. Set `my-claude-bot-app-id'"))

  (let* ((current-time (floor (float-time)))
         (expiry-time (+ current-time 600)) ; 10 minutes
         (header '((alg . "RS256") (typ . "JWT")))
         (payload `((iat . ,current-time)
                   (exp . ,expiry-time)
                   (iss . ,my-claude-bot-app-id))))
    ;; NOTE: This is a placeholder. Emacs doesn't have built-in RS256 JWT signing.
    ;; You'll need to either:
    ;; 1. Use the `jwt' package (not in MELPA yet)
    ;; 2. Call an external tool via shell
    ;; 3. Use a different authentication method
    (my-claude-bot--generate-jwt-external)))

(defun my-claude-bot--generate-jwt-external ()
  "Generate JWT using external tool.
This uses the `github-app-token' npm package if available,
or you can install it with: npm install -g github-app-token"
  (let ((default-directory (file-name-directory my-claude-bot-private-key-file)))
    (string-trim
     (shell-command-to-string
      (format "openssl dgst -sha256 -sign '%s' <<< '{\"iat\":%d,\"exp\":%d,\"iss\":\"%s\"}' | base64"
              my-claude-bot-private-key-file
              (floor (float-time))
              (+ (floor (float-time)) 600)
              my-claude-bot-app-id)))))

(defun my-claude-bot--get-access-token ()
  "Get or refresh the installation access token."
  ;; Check if we have a cached token that hasn't expired
  (when (and my-claude-bot--access-token
             my-claude-bot--token-expiry
             (time-less-p (current-time) my-claude-bot--token-expiry))
    (return my-claude-bot--access-token))

  ;; Need to get a new token
  (unless my-claude-bot-installation-id
    (user-error "GitHub App installation ID not configured. Set `my-claude-bot-installation-id'"))

  (message "Note: JWT generation in pure Elisp is complex. Consider using ghub or forge packages instead.")
  (user-error "JWT generation not fully implemented. See my-claude-bot--generate-jwt for options"))

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
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
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
        (when (file-exists-p my-claude-bot-private-key-file)
          (message "✓ Private key file found"))
        (message "Connection test would complete if JWT generation was implemented"))
    (error
     (message "Connection test failed: %s" (error-message-string err)))))

(provide 'my-claude-bot)
;;; my-claude-bot.el ends here
