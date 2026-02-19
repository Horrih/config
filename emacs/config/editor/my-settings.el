;; Global options and options for built-in packages
;;; Various options
(global-auto-revert-mode) ; Refresh files automatically when modified from outside emacs
(savehist-mode t) ; Save history for commands
(setopt isearch-resume-in-command-history t) ; Use history for isearch as well
(setopt enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(setopt enable-local-variables :all) ; Enable by default variables in .dir-locals.el

;; Display the stacktrace if error encountered in one of the lisp method
(setopt debug-on-error t) ; Enable it at first to troubleshoot our config
(add-hook 'after-init-hook #'toggle-debug-on-error) ; Disable it after startup

(put 'narrow-to-region 'disabled nil) ; Allow narrow to region without prompt
(setopt send-mail-function 'mailclient-send-it) ; E-mail : open default mail client, e.g for report-emacs-bug
(setopt recenter-positions '(top middle bottom)) ; Start recenter on top instead of middle
(setopt make-backup-files nil) ; Do not use backup files (filename~)
(setopt create-lockfiles nil) ; Do not use lock files (.#filename)
(setopt cycle-spacing-actions '(delete-space-after delete-all-space restore))

;;; Grep options
(use-package grep
  :straight (:type built-in)
  :config
  (setopt grep-find-ignored-directories
          (append grep-find-ignored-directories '("node_modules" ".venv" "build" "dist" ".pytest_cache" ".mypy_cache" ".ruff_cache" "__pycache__"))))


;;; Dired options : built-in navigation of folders
(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("u" . dired-up-directory)
              ("i" . nil))
  :custom
  (dired-listing-switches "-alh")
  (dired-kill-when-opening-new-dired-buffer t)) ; Auto close previous folder buffer
