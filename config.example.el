;;; config.example.el --- Example configuration for my-claude-bot -*- lexical-binding: t; -*-

;; This is an example configuration file for my-claude-bot.
;; Copy this to your Emacs init file and update with your actual values.

;; IMPORTANT: Never commit your actual private key or credentials to version control!

;; GitHub App Configuration
;; You can find these values in your GitHub App settings at:
;; https://github.com/settings/apps/YOUR_APP_NAME

(use-package my-claude-bot
  :load-path "~/path/to/my-claude-bot"
  :custom
  ;; Your GitHub App ID (found in app settings)
  (my-claude-bot-app-id "123456")

  ;; Path to your private key file (downloaded from GitHub App settings)
  ;; Keep this file outside of version control!
  (my-claude-bot-private-key-file "~/.config/github-apps/my-claude-bot.pem")

  ;; Your installation ID
  ;; To find this:
  ;; 1. Install your app on a repository
  ;; 2. Visit: https://api.github.com/users/YOUR_USERNAME/installation
  ;; 3. Look for the "id" field
  (my-claude-bot-installation-id "12345678"))

;; Alternative configuration without use-package:
;; (require 'my-claude-bot)
;; (setq my-claude-bot-app-id "123456")
;; (setq my-claude-bot-private-key-file "~/.config/github-apps/my-claude-bot.pem")
;; (setq my-claude-bot-installation-id "12345678")

;; Test your configuration:
;; M-x my-claude-bot-test-connection

;;; config.example.el ends here
