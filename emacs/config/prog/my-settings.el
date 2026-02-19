;;; Customizations
(setopt vc-follow-symlinks t) ; Do not ask to follow symlinks to version controlled files (mostly my emacs config)

;; Automatic pairing for parentheses/braces
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Fill-column display
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(setq-default fill-column 99)

(setopt electric-pair-inhibit-predicate ; Inhibit electric pairing for single/double quotes
        (lambda (c) (if (or (char-equal c ?\')(char-equal c ?\")) t (electric-pair-default-inhibit c))))

;;Show in red the spaces forgotten at the end of lines
(setq-default show-trailing-whitespace t)
(add-hook 'special-mode-hook ; Except for special modes
          (lambda() (setq-local show-trailing-whitespace nil)))

(setopt indent-tabs-mode nil) ; Use spaces for indent
(setopt tab-width 4) ; How to display tab characters in emacs
(setopt next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line

;;; project.el : Automatic detection of project, and various related project management commands
(use-package project
  :straight (:type built-in)
  :custom
  ;; Make `project-switch-project' open dired instead of prompting for the command to run
  (project-switch-commands 'project-dired))

;;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
  :mode "\\.csv\\'")

;;; markdown-mode
(use-package markdown-mode
  :hook (markdown-mode . (lambda()
                           (setq-local fill-column 79)
                           (display-fill-column-indicator-mode t))))

;;; Require treesit at startup
;;;; Cheatsheet
;; (setq treesit--indent-verbose t)
;; M-x treesit-explore-mode
;; M-x treesit-inspect-mode
;; (treesit-query-capture (treesit-buffer-root-node) '((compound_statement "{") @comp))
;; (treesit-query-validate 'cpp '((if_statement consequence: (_)@exp)))

;;;; Enable treesitter
(use-package treesit
  :custom (treesit-font-lock-level 3) ; Level of syntax coloring
  :straight (:type built-in)
  :demand t)
