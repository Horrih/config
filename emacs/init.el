;;; Enable lexical binding -*- lexical-binding: t -*-
;;Lexical binding enables using variables defined with let in lambda functions called later
;;; Package management
;;;; Optimize garbage collection : improves the startup time
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()(setq gc-cons-threshold 800000)))

;;;; Enable straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package straight
  :custom
  (straight-use-package-by-default t)) ; use-package integration

;;;; use-package : Customize the package macro for this file
(use-package use-package
  :straight (:type built-in)
  :custom
  (use-package-always-defer t)) ; Lazy load by default, use :demand otherwise

;;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish
  :demand
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

(use-package simple
  :straight (:type built-in)
  :config
  (diminish 'auto-fill-function))

;;;; esup : Launch the esup command to measure startup time of each emacs plugin
(use-package esup
  :disabled
  :custom (esup-depth 0)) ; Sometimes fixes the bug https://github.com/jschaf/esup/issues/54

;;; Various customizations options
;;;; my/keys minor mode for global keybindings overriding to be turned off/on
(define-minor-mode my/keys-mode
  "Minor mode to enable custom keybindings"
  :lighter ""
  :global t
  :keymap '())
(my/keys-mode)

;;;; Main color theme : vscode-dark-plus-theme
(use-package vscode-dark-plus-theme
  :demand
  :config (load-theme 'vscode-dark-plus t))

;;;; Mode line theme : doom mode line - Must download CaskaydiaCove Nerd Font
(use-package doom-modeline
  :straight (:fork "Horrih/doom-modeline")  ; Fork to fix right-margin
  :demand
  :custom-face
  (mode-line ((t :background "black")))
  (mode-line-inactive ((t :background "#333333"))) ; Dark grey
  :custom
  ;; Font with icons to download here
  ;; https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/CascadiaCode.zip
  (nerd-icons-font-family "CaskaydiaCove NFM")
  (doom-modeline-buffer-file-name-style 'relative-from-project) ; Use 'truncate-nil for full path
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes t)
  (display-time-format "%H:%M")  ; Display time as 13:37
  (display-time-default-load-average nil)  ; Do not show load average when displaying time
  :config
  (display-battery-mode)
  (display-time-mode)

  ;; Add a segment telling wether we are in insert or edit mode
  (doom-modeline-def-segment ijkl
    (unless (bound-and-true-p ijkl-local-mode)
        (propertize "     > edit " 'face 'doom-modeline-project-dir)))

  ;; Use a custom modeline
  (doom-modeline-def-modeline 'main
    '(eldoc bar buffer-info remote-host buffer-position selection-info ijkl)
    '(compilation misc-info battery debug lsp minor-modes buffer-encoding major-mode process vcs check time))
  (doom-modeline-mode))

;;;; Dired as default buffer
(when (< (length command-line-args) 2)
  (add-hook 'after-init-hook 'dired-jump))

;;;; Misc
(progn
  (tool-bar-mode 0) ; Disable the toolbar in GUI mode
  (customize-set-variable 'cursor-type 'bar)
  (customize-set-variable 'scroll-bar-mode nil)
  (customize-set-variable 'horizontal-scroll-bar-mode nil)
  (customize-set-variable 'vc-follow-symlinks t) ; Do not ask to follow symlinks to version controlled files (mostly my emacs config)
  (customize-set-variable 'inhibit-startup-screen t) ; Hide the startup screen
  (savehist-mode t) ; Save history for commands
  (customize-set-variable 'isearch-resume-in-command-history t) ; Use history for isearch as well
  (global-auto-revert-mode) ; Refresh files automatically when modified from outside emacs
  (customize-set-variable 'enable-local-eval t) ; Enable eval blocks in .dir-locals.el
  (customize-set-variable 'enable-local-variables :all) ; Enable by default variables in .dir-locals.el
  (customize-set-variable 'ring-bell-function 'ignore) ; Disable the bell for emacs
  (customize-set-variable 'debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
  (customize-set-variable 'completions-detailed t) ; Detailed description for the built in describe symbol etc
  (column-number-mode t) ; Display column numbers in the status line
  (global-display-line-numbers-mode -1) ; If true, display line numbers on the left
  (line-number-mode t) ; Display line number
  (size-indication-mode t) ; Display size indication
  (delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
  (show-paren-mode 1) ; Highlight the matching parenthesis
  (put 'narrow-to-region 'disabled nil) ; Allow narrow to region without prompt
  (customize-set-variable 'send-mail-function 'mailclient-send-it) ; E-mail : open default mail client, e.g for report-emacs-bug

  ;; Automatic pairing for parentheses/braces
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)

  ;; Fill-column display
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
  (setq-default fill-column 99)

  (customize-set-variable 'electric-pair-inhibit-predicate ; Inhibit electric pairing for single/double quotes
      (lambda (c) (if (or (char-equal c ?\')(char-equal c ?\")) t (electric-pair-default-inhibit c))))

  ;;Show in red the spaces forgotten at the end of lines
  (setq-default show-trailing-whitespace t)
  (add-hook 'special-mode-hook ; Except for special modes
            (lambda() (setq-local show-trailing-whitespace nil)))

  (customize-set-variable 'indent-tabs-mode nil) ; Use spaces for indent
  (customize-set-variable 'tab-width 4) ; How to display tab characters in emacs
  (customize-set-variable 'next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
  (menu-bar-mode -1) ; Hide Menu bar
  (customize-set-variable 'use-short-answers t) ; Abreviate Yes/No to y or n
  (customize-set-variable 'recenter-positions '(top middle bottom)) ; Start recenter on top instead of middle
  (customize-set-variable 'make-backup-files nil) ; Do not use backup files (filename~)
  (customize-set-variable 'create-lockfiles nil)) ; Do not use lock files (.#filename)


;;; Helper commands
;;;; my/mode-is-one-of-p helper function
(defun my/mode-is-one-of-p(modes)
  "Returns t if the current modes (minor or major) matches one in the input modes list"
  (let (res)
    (dolist (input-mode modes res)
      (dolist (mode (cons major-mode minor-mode-list))
        (when (string-equal input-mode mode)
          (setq res t))))))

;;;; my/switch-to-last-buffer
(defun my/switch-to-last-buffer()
  "Use `switch-to-buffer' to visit the last buffer"
  (interactive)
  (switch-to-buffer nil))

;;;; my/delete-start-or-previous-line
(defun my/delete-start-or-previous-line()
  "Use `kill-line' to delete either the start of the line, or the previous line if empty"
  (interactive)
  (kill-line (if (= (line-beginning-position) (point)) -1 0)))

;;;; Automatic margin
;;;;; Customize variables to tweak margin size
(defcustom my/margin-center-column 45
  "Column that should appear at the center when auto margin mode is enabled"
  :local t
  :type '(integer))

(defcustom my/margin-right-ratio 0.75
  "Ratio of the right margin to the left one"
  :local t
  :type '(float))

;;;;; Implementation functions
(defun my/margin--compute(window)
  "Computes the left margin for window `WINDOW' if width big enough, 0 otherwise"
  (let* ((width (window-width window))
         (prev-margin-left (or (car (window-margins window)) 0))
         (prev-margin-right (or (cdr (window-margins window)) 0))
         (total-width (+ width prev-margin-left prev-margin-right)))
    (with-current-buffer (window-buffer window)
      (if (or display-line-numbers-mode (minibufferp))  ; No margin if minibuffer or line numbers
          '(0 . 0)
        (let* ((left-margin (- (/ total-width 2) my/margin-center-column))
               (right-margin (truncate (* left-margin my/margin-right-ratio))))
          `(,(max 0 left-margin) . ,(max 0 right-margin)))))))

(defun my/margin--set()
  "Margin if single window, no margin if split"
  (walk-windows
   (lambda(window)
       (with-current-buffer (window-buffer window)
         (if my/margin-auto-local-mode
             (let* ((margins (my/margin--compute window))
                    (left-margin (car margins))
                    (right-margin (cdr margins)))
               (setq-local split-window-preferred-function #'my/margin--reset-split-window-sensibly)
               (set-window-parameter window 'split-window 'my/margin--reset-split-window)
               (set-window-margins window left-margin right-margin))
           (setq-local split-window-preferred-function #'split-window-sensibly)
           (set-window-parameter window 'split-window nil)
           (set-window-margins window nil))))
   nil t))

(defun my/margin--reset()
  "Reset the margins to 0 for all windows"
  (walk-windows (lambda(w)
                  (set-window-parameter w 'split-window nil)
                  (set-window-margins w nil))
                nil t))

(defun my/margin--reset-split-window (&optional window size side pixelwise)
  "Call `split-window' after resetting margins (which interfer with splitting)"
  (my/margin--reset)
  (split-window window size side pixelwise))

(defun my/margin--reset-split-window-sensibly (&optional window)
  "Call `split-window-sensibly' after resetting margins (which interfer with splitting)"
  (my/margin--reset)
  (split-window-sensibly window))

;;;;; Define the auto-margin minor mode, local and global
(define-minor-mode my/margin-auto-local-mode
  "Minor mode to enable/disable left margin"
  :lighter " Margin"
  (if my/margin-auto-local-mode
      (add-hook 'window-configuration-change-hook 'my/margin--set nil t)
    (my/margin--set)
    (remove-hook 'window-configuration-change-hook 'my/margin--set t)))

(define-globalized-minor-mode my/margin-auto-mode my/margin-auto-local-mode
  (lambda()
    (unless (my/mode-is-one-of-p '("magit-log-mode"))
      (my/margin-auto-local-mode t))))
(my/margin-auto-mode t) ; Turn it on
(diminish 'my/margin-auto-local-mode)


;;;; Window management
;;;;; my/other-window-reverse
(defun my/other-window-reverse()
  "Like `other-window' but in the reverse order"
  (interactive)
  (other-window -1))

;;;;; my/pick-window-right
(defun my/pick-window-right()
  "Like `split-window-right' except it lets you pick the buffer on the other side"
  (interactive)
  (let ((split-height-threshold (+ (frame-height) 1)))  ; Increase height threshold to avoid vertical split
    (consult-buffer-other-window)))

;;;;; my/pick-window-below
(defun my/pick-window-below()
  "Like `split-window-below' except it lets you pick the buffer on the other side"
  (interactive)
  (let ((split-width-threshold (+ (frame-width) 1)))  ; Increase width threshold to avoid horizontal split
    (consult-buffer-other-window)))

;;;; my/replace-chat-at-point
(defun my/replace-char-at-point(char)
  "Replaces the caracter at point by `CHAR'"
  (interactive "cReplace character at point with : ")
  (delete-char 1)
  (insert-char char)
  (backward-char 1))

;;;; my/delete-char-or-kill-region
(defun my/delete-char-or-kill-region()
  "If mark is active, kill region, otherwise delete-char"
  (interactive)
  (call-interactively
    (if mark-active
        'kill-region
      'delete-char)))

;;;; my/sexp-other-side
(defun my/sexp-other-side()
  "uses `backward-sexp' and `forward-sexp' to switch position between
start and end of sexp. If inside a sexp, go the the start of the sexp.
Does not work for strings, since they do not have separate start/end characters"
  (interactive)
  (cond
   ((seq-contains "([{" (char-after)) (forward-sexp))
   ((seq-contains ")}]" (char-before)) (backward-sexp))
   (t (backward-up-list))))

;;;; my/comment-dwim
(defun my/comment-dwim()
  "Like `comment-dwim', but comment line if cursor at beginning of line"
  (interactive)
  (call-interactively
    (if (or (region-active-p) (/= (line-beginning-position) (point)))
        #'comment-dwim
      #'comment-line)))

;;; Compilation options
;;;; Compilation misc
(use-package compile
  :straight (:type built-in)
  :hook (compilation-mode . (lambda()(setq show-trailing-whitespace nil)))
  :custom
  (compilation-always-kill t)) ; Do not ask for confirmation when I stop current compilation

;;;; my/switch-to-compilation-other-window()
(defun my/switch-to-compilation-other-window()
  "Switches to the compilation buffer in another window"
  (interactive)
  (unless (string-equal "*compilation*" (buffer-name))
    (switch-to-buffer-other-window "*compilation*")))

(defun my/switch-to-compilation-other-window-end()
  "Switches to the compilation buffer in another window and go to buffer end"
  (interactive)
  (my/switch-to-compilation-other-window)
  (end-of-buffer))

;;;; my/recompile-switch
(defun my/recompile-switch()
  "Uses the recompile function and switches to the buffer end"
  (interactive)
  (recompile)
  (my/switch-to-compilation-other-window-end))

;;;; my/compile-all
(defcustom my/compile-all-command nil
  "If non nil, `my/compile-all' will use it as command instead of `compile-command'
This can be useful in conjunction to your project's variables defined in .dir-locals.el"
  :type 'string
  :risky nil)

(defun my/compile-all()
  "Compiles the whole project and switch to buffer end"
  (interactive)
  (compile (or my/compile-all-command "make -j8"))
  (my/switch-to-compilation-other-window-end))

;;;; my/compile-file
(defun my/compile-file(file-name)
  "Compiles the file FILE-NAME using a command to be define `compile-file-command'
  This function should take a filename as parameter and returning the command as output"
  (interactive (list (buffer-file-name)))
  (unless (fboundp 'compile-file-command)
    (error "compile-file expects the compile-file-command function to be defined"))
  (compile (compile-file-command file-name))
  (my/switch-to-compilation-other-window-end))

;;;; ansi-color : Translate TTY escape sequences into colors
(defun my/ansi-color-compilation-filter-except-ag()
  "Like `ansi-color-compilation-filter', except on buffers generated by the ag package.
   If we use vanilla ansi-color-compilation-filter, the colors get messed up"
  (unless (string-match "ag search text" (buffer-name))
    (ansi-color-compilation-filter)))

(use-package ansi-color
  :straight (:type built-in)
  :hook (compilation-filter . my/ansi-color-compilation-filter-except-ag)) ; Handle terminal colors in the compilation buffer

;;;; Error regexps : Set compilation regex for errors
(let ((enabled-regexps ())
      (custom-error-list '(
        ;; Insert your custom regexps here
        (file-path-col "^\\([a-zA-Z_/.]*\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3 0)
        (file-path "^\\([a-zA-Z_/.]*\\):\\([0-9]+\\):.*$" 1 2 nil 0)
        (doctest "^\\(.*\\):\\([0-9]+\\): FATAL ERROR" 1 2 nil)
        (link-error "^\\(.*\\):.*: undefined reference to `\\(.*\\)'$" 1 nil nil nil nil (2 compilation-info-face))
        (jest-error "^.*\(\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\).*$" 1 2 3)
        (gcc-error "^[ ]*\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*error:\\|  required from here\\).*$" 1 2 3)
        (gcc-warning "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): warning:.*$" 1 2 3 1)
        (gcc-info "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): note:.*$" 1 2 3 0)
        (qt-test "^   Loc: \\[\\(.*\\)\(\\([0-9]+\\)\)\\]$" 1 2)
        (python-unittest "^  File \"\\(.*\\)\", line \\([0-9]+\\)[^0-9]?.*$" 1 2)
        )))
  (dolist (err custom-error-list)
    (add-to-list 'enabled-regexps (car err)))
  (custom-set-variables `(compilation-error-regexp-alist ',enabled-regexps))
  (add-hook 'compilation-mode-hook (lambda()
    (dolist (err custom-error-list)
      (add-to-list 'compilation-error-regexp-alist-alist err)))))

;;; General usage packages
;;;; project.el : Automatic detection of project, and various related project management commands
(use-package project
  :straight (:type built-in)
  :custom
  ;; Make `project-switch-project' open dired instead of prompting for the command to run
  (project-switch-commands 'project-dired))

;;;; magit : Git front end (amazing!)
(use-package magit
  :custom-face (magit-filename ((t :foreground "white"))) ; Otherwise untracked files have the same color as title in git status
  :custom
  (magit-no-confirm t) ; Do not ask for confirmation for actions
  (magit-visit-ref-behavior '(checkout-any focus-on-ref))) ; Enter on branch names makes you checkout the branch

;;;; ediff : Built in side by side diffs of files
(use-package ediff
  :straight (:type built-in)
  :hook (ediff-keymap-setup . (lambda()
                        (keymap-set ediff-mode-map "h" 'ediff-status-info)
                        (keymap-set ediff-mode-map "'" 'other-window)
                        (keymap-set ediff-mode-map "4" 'my/other-window-reverse)
                        (keymap-set ediff-mode-map "i" 'ediff-previous-difference)
                        (keymap-set ediff-mode-map "k" 'ediff-next-difference)))
  :custom
  (ediff-split-window-function 'split-window-horizontally)) ; Make ediff split side by side

;;;; which-key : Displays command shortcuts when typing commands
(use-package which-key
  :demand
  :config (which-key-mode)
  :diminish)

;;;; key-chord  : Enables combination of keys like zz
(use-package key-chord
  :demand
  :custom (key-chord-safety-interval-forward 0.1)
  :config (key-chord-mode))

;;;; hydra : Keybindings combinations
(use-package hydra)

;;;; Vertico : Completion for commands in a vertical way
(use-package vertico
  :init (vertico-mode)
  :hook (completion-list-mode . (lambda()(setq-local show-trailing-whitespace nil)))  ; Disable whitespace check in completion buffers (e.g M-:)
  :custom-face
  (vertico-current ((t (:background "#264f78")))) ; Current selected item shown as blue
  :custom
  (vertico-cycle t)
  (vertico-count 15))

;;;; Marginalia : Display additional completion data (doc strings, file permissions...)
(use-package marginalia
  :init (marginalia-mode)
  :custom-face
  (completions-annotations ((t (:inherit 'shadow))))) ; Disable italic since it is translated to underline in terminal

;;;; Orderless : Matching of several patterns without order in completion
(use-package orderless
  :custom-face
  (orderless-match-face-0 ((t (:weight bold :foreground "gold1")))) ; Display the first matching part as yellow gold
  :custom
  ((completion-styles '(orderless basic))
   (completion-category-defaults nil)
   (completion-category-overrides '((file (styles partial-completion))))))

;;;; Consult : a collection of commands that improve emacs defaults
(use-package consult
  :bind (:map my/keys-mode-map
         ("M-y" . consult-yank-pop)
         :map help-map
         ("I" . consult-info)
         :map ctl-x-map
         ("b" . consult-buffer))
  :config
  (recentf-mode))

;;;; Expand Region : expand or contract selection
(use-package expand-region)

;;;; Helpful : nice looking and more complete help buffers
(use-package helpful
  :bind (:map help-map
              ("p" . helpful-at-point)
              ("s" . helpful-symbol)
              ("v" . helpful-variable)
              ("f" . helpful-callable)
              ("k" . helpful-key)))

;;;; Dired : built-in navigation of folders
(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("u" . dired-up-directory)
              ("i" . nil))
  :custom(dired-kill-when-opening-new-dired-buffer t)) ; Auto close previous folder buffer

;;; Org mode : Note taking and presentation
;;;; Org mode : package customizations
(use-package org
  :straight (:type built-in)
  :custom-face
  (org-warning ((t (:underline nil)))) ; Do not underline org-warnings, red is enough
  (org-done ((t (:foreground "lightgreen" :box(:color "lightgreen")) )))
  (org-document-title ((t (:weight bold :height 1.6))))
  (org-level-1        ((t (:height 1.2))))
  (org-level-2        ((t (:height 1.1))))
  (org-block          ((t (:inherit 'fixed-pitch))))
  :custom ((org-agenda-files '("~/.config/emacs/org_roam")) ; For autopopulating todos from notes
           (org-todo-keywords '((sequence "A FAIRE(t)" "EN ATTENTE(w@/!)" "|" "ANNULÉ(c@/!)" "FAIT(d!)")))
           (org-todo-keyword-faces '(("EN ATTENTE" . "gold") ("ANNULÉ" . "grey")))
           (org-agenda-span 'month) ; To have a monthly view by default
           (org-startup-folded 'content)
           (org-agenda-start-on-weekday 1) ; Agenda starts on monday in agenda
           (calendar-week-start-day 1) ; Date picker starts on monday
           (org-capture-bookmark nil)) ; To disable adding a bookmark on each org capture
  :hook
  (org-agenda-mode . (lambda()
                       (keymap-set org-agenda-mode-map "C-c p" 'org-agenda-earlier)
                       (keymap-set org-agenda-mode-map "C-c n" 'org-agenda-later)))
  (org-mode . (lambda()
                (require 'org-tempo) ; For templates like <sTAB to insert a code block
                (key-chord-define org-mode-map "CC" 'org-ctrl-c-ctrl-c) ; Close notes by typing CC
                (org-indent-mode) ; Auto indent lines according to depth
                (auto-fill-mode) ; Wrap lines when longer than fill column

                ;; Ignore org files from recentf due to agenda loading everything
                (require 'recentf)
                (add-to-list 'recentf-exclude ".*org$"))))

(use-package org-indent
  :straight (:type built-in)
  :diminish)

;;;; Org bullets : Pretty mode for org
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;;; org-roam : Notes organizing
(use-package org-roam
  :custom
  (org-roam-directory "~/.config/emacs/org_roam")
  (org-return-follows-link t)
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode))

(defun my/org-roam-pull-commit-push()
  "Git commit and push all the modified files in `org-roam-directory'"
  (interactive)
  (let ((default-directory org-roam-directory))
    (shell-command "git add -u")
    (shell-command "git commit -m 'Automated commit from org-roam-commit-and-push'" )
    (shell-command "git pull --rebase" )
    (shell-command "git push" )))

;;;; org-download : Download images directly into org
(use-package org-download
  :commands (org-download-clipboard)
  :custom (org-image-actual-width 900)
  :config
  ;; Override the default dir to have one dir for each org file instead of per heading
  (defun org-download--dir-1 ()
    (or org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))))

;;;; visual-fill-column : Center text in the window and wrap around fill-column
;; TODO : not sure if still useful with my automargin mode
(use-package visual-fill-column
  :custom ((visual-fill-column-width 130)
           (visual-fill-column-center-text t)))

;;;; org-present : Using org files for powerpoints
(defun my/org-present-start()
  "To be called when enabling `org-present-mode' : sets up the various presentation options"
  (setq-local face-remapping-alist '((default (:height 1.5 :family "Arial") default)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq-local header-line-format " ")
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (global-display-line-numbers-mode 0))

(defun my/org-present-end()
  "To be called when leaving `org-present-mode' : disables the various presentation options"
(setq-local header-line-format nil)
  (setq-local face-remapping-alist nil)
  (visual-fill-column-mode 0)
  (global-display-line-numbers-mode 1))

(use-package org-present
  :hook ((org-present-mode      . my/org-present-start)
         (org-present-mode-quit . my/org-present-end))
  :bind (:map org-present-mode-keymap
         ("C-c C-h" . org-present-hide-cursor)))

;;; Development packages and options
;;;; markdown-mode
(use-package markdown-mode
  :hook (markdown-mode . (lambda()
                           (setq-local fill-column 79)
                           (display-fill-column-indicator-mode t))))

;;;; yasnippet : Snippets insertion
(use-package yasnippet
  :demand
  :config
  (yas-global-mode)
  (diminish 'yas-minor-mode))

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; web-mode : Support various web files
(use-package web-mode
  :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.vue\\'")
  :custom
  (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
  (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

;;;; prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
  :custom
  (prettier-js-show-errors nil))

;;;; Outline mode with package outline-minor-faces
;;;;; Package configuration
(use-package outline
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . (lambda()
                       (setq-local outline-regexp "^;;;;* ")
                       (outline-minor-mode)))
  :config
  (diminish 'outline-minor-mode)
  :custom
  (outline-minor-mode-cycle t) ; Tab and S-Tab cycle between different visibility settings
  (outline-minor-mode-cycle-filter 'bolp)) ; Cycle only when on line beginning

;;;;; Pretty colors for headings
;; We don't use (outline-minor-mode-highlight 'override) because it applies to some non headings as well
(use-package outline-minor-faces
   :hook (outline-minor-mode . outline-minor-faces-mode))

;;;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
  :mode "\\.csv\\'")

;;;; hide-show-mode : Hide/show sections of code : current function, class, or if/else section
(use-package hideshow
  :straight (:type built-in)
  :config
  (diminish 'hs-minor-mode)
  :hook
  (prog-mode . hs-minor-mode))


;;;; Clang format
(use-package clang-format)
(defun my/clang-format-save-hook()
  "Create a buffer local save hook to apply `clang-format-buffer'"
  ;; Only format if .clang-format is found
  (when (locate-dominating-file "." ".clang-format")
    (clang-format-buffer))
  ;; Continue to save
  nil)

(define-minor-mode my/clang-format-on-save-mode
  "Minor mode to enable/disable automated clang format on save"
  :lighter " ClangFormat"
  (if my/clang-format-on-save-mode
      (add-hook 'before-save-hook 'my/clang-format-save-hook nil t)
    (remove-hook 'before-save-hook 'my/clang-format-save-hook t)))

(define-globalized-minor-mode my/clang-format-auto-mode my/clang-format-on-save-mode
  (lambda()(my/clang-format-on-save-mode t))
  :predicate '(c-mode c-ts-mode c++-mode c++-ts-mode c-or-c++-mode c-or-c++-ts-mode))
(my/clang-format-auto-mode t) ; Turn it on

;;;; code coverage
(defun my/toggle-coverage-overlay()
  (interactive)
  (cl-case major-mode
    (c++-mode    (call-interactively #'python-coverage-overlay-mode))
    (c-mode      (call-interactively #'python-coverage-overlay-mode))
    (c++-ts-mode (call-interactively #'python-coverage-overlay-mode))
    (python-mode (call-interactively #'python-coverage-overlay-mode))
    (t (user-error "No coverage overlay specified for mode %s" major-mode))))

(use-package python-coverage
  :config
  (require 'magit))  ; Uses magit's faces

;;;; legacy c++ mode
(use-package cc-mode
  :straight (:type built-in)
  :config
  (setq-default c-basic-offset  4) ; Base indent size when indented automatically
  (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
  (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
  (advice-add 'c-update-modeline :override #'ignore)) ;; Don't use a modeline suffix (i.e C++//l)

;;;; legacy python mode
(use-package python-mode
  :straight (:type built-in)
  :hook (python-mode . (lambda() (setq-local fill-column 79))))

;;; Tree-Sitter Section
;;;; Cheatsheet
;; (setq treesit--indent-verbose t)
;; M-x treesit-explore-mode
;; M-x treesit-inspect-mode
;; (treesit-query-capture (treesit-buffer-root-node) '((compound_statement "{") @comp))
;; (treesit-query-validate 'cpp '((if_statement consequence: (_)@exp)))

;;;; Require treesit at startup
(use-package treesit
  :custom (treesit-font-lock-level 3) ; Level of syntax coloring
  :straight (:type built-in)
  :demand t)

;;;; cmake-ts-mode : Major mode for CMakeLists.txt
(use-package cmake-ts-mode
  :if (treesit-language-available-p 'cmake)
  :mode ("CMakeLists\\.txt" "\\.cmake\\'")
  :custom (cmake-ts-mode-indent-offset 4)
  :config
  (setq cmake-ts-mode--indent-rules
        `((cmake
           ,@(alist-get 'cmake cmake-ts-mode--indent-rules)
           ((parent-is "source_file") parent-bol 0)   ; If top level, no indent
           ((match nil nil nil 1 nil) prev-line 0) ; Otherwise align with previous-sibiling
           ((match nil nil nil nil nil) parent-bol cmake-ts-mode-indent-offset))))) ; No sibling : parent + offset

;;;; docker-ts-mode : Major mode for Dockerfiles
(use-package dockerfile-ts-mode
  :if (treesit-language-available-p 'dockerfile)
  :mode ("Dockerfile" "\\.dockerfile\\'" "\\.docker\\'"))

;;;; yaml-ts-mode : Major mode for yaml files
(use-package yaml-ts-mode
  :if (treesit-language-available-p 'yaml)
  :hook (yaml-ts-mode . (lambda()
                          ;; Tab indentation is traditionnally 2 for yaml
                          (setq-local tab-width 2)
                          ;; Remap string face so that it is displayed as plain text
                          (face-remap-add-relative 'font-lock-string-face :inherit 'default)))
  :mode ("\\.ya?ml\\'"))

;;;; c/c++-ts-mode : Major mode for C/C++ files
(defun my/cpp-indent-style()
  "Override the built-in K&R indentation style with some additional rules"
  `(
    ((node-is ")") parent-bol 0) ; Otherwise aligned with opening parenthese
    ((node-is "compound_statement") parent-bol 0) ; Blocks with {}
    ((parent-is "declaration_list") parent-bol 0) ; Do not indent namespace blocks
    ((parent-is "template_declaration") parent-bol 0) ; function declaration on the line following the template declaration
    ((parent-is "comment") parent-bol 0) ; Align comments at the start of the comment section
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset) ; Standard indent if argument starts on next-line
    ((parent-is "argument_list") prev-line 0) ; Align argument with previous one systematically
    ((node-is "field_initializer_list") parent-bol ,(/ c-ts-mode-indent-offset 2)) ; class field initializer : use half i
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset) ; Standard indent if parameter starts on next-line
    ((parent-is "parameter_list") prev-line 0) ; Align parameter with previous one systematically
    ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))

(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'my/cpp-indent-style)
  :hook (c-ts-base-mode . (lambda() (setq-local my/margin-line-width 100)))
  :init
  (advice-add 'c-ts-mode-set-modeline :override 'ignore) ; Do not add a // suffix to the modeline
  ;; Remap the standard C/C++ modes
  (add-to-list 'major-mode-remap-alist '(c-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c++-ts-mode)))

;;; LSP completion, linting
;;;; corfu : Completion frontend, used by lsp
;;;;; corfu-terminal : Corfu's UI only works in GUI emacs
(use-package corfu-terminal
  :hook (corfu-mode . (lambda()
                        (unless (display-graphic-p)
                          (corfu-terminal-mode t)))))

;;;;; corfu itself
(use-package corfu
  :custom
  (corfu-auto t)  ; Auto completion as you type (can be triggered manyally with completion-at-point)
  (corfu-cycle t) ; Cycle when at bottom of candidates
  :bind (:map corfu-map ("M-c" . corfu-insert-separator))
  :init
  (require 'corfu-info)  ; M-g/M-h to see location/help for candidate
  (global-corfu-mode))   ; Enable corfu everywhere

;;;;; corfu in the minibuffer
(defun my/corfu-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'my/corfu-minibuffer)

;;;; flymake : Syntax highlighting, used by eglot/lsp
(use-package flymake
  :straight (:type built-in)
  :custom(flymake-mode-line-lighter ""))

;;;; eglot : Built-in package for completion with LSP. Light-weight alternative to lsp-mode
(use-package eglot
  :straight (:type built-in)
  :hook (eglot-managed-mode . (lambda()
                                (eglot-inlay-hints-mode -1) ; Disable inlay hint for params by default
                                (custom-set-variables
                                 '(help-at-pt-timer-delay 0.5)
                                 '(help-at-pt-display-when-idle '(flymake-diagnostic)))))
  :bind (:map help-map ("h" . eldoc))
  :custom
  (eglot-report-progress nil)
  :custom-face
  (eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow :underline t)))))

;;;; lsp-mode : Full-featured LSP client
(defun my/lsp-if-active-workspace()
  "Call `lsp-deferred' only if the file is in an active lsp-mode workspace"
  (when (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
    (require 'lsp-mode)
    (lsp-deferred)))

(use-package lsp-mode
  :hook
  (c++-ts-mode . my/lsp-if-active-workspace)
  (python-mode . my/lsp-if-active-workspace))

;;; ASMR - A Simple Mark Ring - Reimplementation of an IDE-like mark ring
;;;; Define the global variables used
(defgroup asmr nil "A Simple Mark Ring : Bidirectionnal global mark ring.

It combines the ideas of `mark-ring'  and `global-mark-ring', with the following additions :
- Can be navigated two ways using `asmr-next' and `asmr-previous'
- Unlike global-mark-ring, it stores multiple marks per file")

(defvar asmr--mark-previous ()
  "List containing previous mark positions, combining the ideas of `mark-ring'  and `global-mark-ring'.
This mark-ring will record all mark positions globally, multiple times per buffer")

(defvar asmr--mark-next ()
  "List containing next mark positions, used to revert the effects of `asmr--mark-previous'")

(defcustom asmr-max-history-size 40
  "Maximum size of the asmr mark ring`asmr--mark-previous'. Start discarding off end if it gets this big."
  :type 'integer
  :group 'asmr)

;;;; Override pushmark
(defun asmr--push-mark(&optional location nomsg activate)
  (interactive)
    (let ((old (nth asmr-max-history-size asmr--mark-previous))
          (history-delete-duplicates nil))
      ;; Reset the next marks if we go to another buffer
      (unless (and asmr--mark-previous
                   (eq (marker-buffer (car asmr--mark-previous))
                       (current-buffer)))
        (setq asmr--mark-next ()))
      (add-to-history 'asmr--mark-previous (copy-marker (mark-marker)) asmr-max-history-size)
      (when old
        (set-marker old nil))))

(advice-add 'push-mark :after 'asmr--push-mark)

;;;; mark manipulation utilities
(defun asmr--marker-is-point-p (marker)
  "Tests if MARKER is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun asmr--jump-to-marker(marker)
  "Jumps to the given MARKER buffer/position"
  (let* ((buffer (marker-buffer marker))
	 (position (marker-position marker)))
    (set-marker (mark-marker) marker)
    (set-buffer buffer)
    (goto-char position)
    (when (hs-overlay-at position)
      (hs-show-block)
      (goto-char position))
    (switch-to-buffer buffer)))

;;;; Push mark on the buffer change functions which do not currently do it
(defun asmr-push-mark-advice(&rest _args)
  "To be used as advice :before to perform a push-mark before a command"
  (push-mark))

(dolist (item '(consult-buffer other-window))
  (advice-add item :before 'asmr-push-mark-advice))

;;;; asmr-backward() : main back function
(defun asmr-backward()
  "Records the current position at mark and jump to previous mark"
  (interactive)
  (let* ((target (car asmr--mark-previous))
         (current target))
    (cond ((not current) (setq target nil))
          ((asmr--marker-is-point-p current) (setq target (car (cdr asmr--mark-previous))))
          (t (push-mark)))
    (if (not target)
        (user-error "No previous mark position")
      (push (copy-marker (mark-marker)) asmr--mark-next)
      (pop asmr--mark-previous)
      (asmr--jump-to-marker (car asmr--mark-previous)))))

;;;; asmr-forward() : main next function
(defun asmr-forward()
  "Goes back to the last mark before `asmr-backward' was called"
  (interactive)
  (let* ((target (car asmr--mark-next))
         (prev (car asmr--mark-previous)))
    (if (not target)
        (user-error "No next mark position")
      (unless (and prev (asmr--marker-is-point-p prev))
        (push-mark))
      (push (copy-marker target) asmr--mark-previous)
      (pop asmr--mark-next)
      (asmr--jump-to-marker target))))

;;; Modal edition mode using ijkl for movement
;;;; ijkl minor mode definition
(define-minor-mode ijkl-local-mode
  "Minor mode to enable movement using ijkl"
  :lighter " ijkl"
  :keymap '(([remap self-insert-command]  ignore)) ; The actual keymaps are defined later below
  (add-to-list 'emulation-mode-map-alists '(ijkl-local-mode . ijkl-local-mode-map))
  (ijkl-insert-mode (if ijkl-local-mode -1 1))
  )
(diminish 'ijkl-local-mode)

(define-minor-mode ijkl-insert-mode
  "Minor mode for key bindings to be used when `ijkl-local-mode' is disabled"
  :keymap '())

(keymap-set ijkl-local-mode-map "d" 'ijkl-local-mode) ; Leave ijkl mode with d
(key-chord-define ijkl-insert-mode-map "jj" 'ijkl-local-mode-and-save) ; Enter ijkl mode with "jj"
(key-chord-define ijkl-insert-mode-map "qq" 'ijkl-local-mode) ; Enter ijkl mode with "qq"
(keymap-set my/keys-mode-map "C-q" 'ijkl-local-mode) ; Fallback if "kk" key-chord fails (e.g high latency ssh)
(keymap-set ijkl-insert-mode-map "M-c" ijkl-local-mode-map) ; Make ijkl bindings available in insert mode

(defun ijkl-local-mode-and-save()
  "Enables ijkl-local-mode and saves the current file if applicable"
  (interactive)
  (ijkl-local-mode)
  (when (and (buffer-modified-p) buffer-file-name)
    (save-buffer)))

;;;; ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode on traditional buffers"
    (if (or (minibufferp)
            (string-match "[Gg]it" (format "%s" major-mode))
            (string-match "[Gg]it" (format "%s" major-mode))
            (string-equal (buffer-name) "*Org Note*")
            (string-equal (buffer-name) "*Ediff Control Panel*")
            (and
             (string-equal (buffer-name) "COMMIT_EDITMSG")
             (not (string-match-p "rebase" (buffer-string)))))
        (ijkl-insert-mode)
      (ijkl-local-mode))))
(ijkl-mode)

;;; Main keybindings
;;;; Helper function gen-input
(defun my/gen-input(KEYS)
  "Generates a key `KEYS' sequence as if the user typed it"
  (setq unread-command-events (nconc (listify-key-sequence (kbd KEYS)) unread-command-events)))

;;;; Helper macro key-alias
(defmacro key-alias(keymap from to &optional except-modes)
  "Binds the key-binding FROM to the function called by typing TO.

The forwarding will only occur if the current major mode is not in EXCEPT-MODES list"
  `(keymap-set ,keymap ,from
     (defun ,(intern (format "my/%s-alias/%s/%s" keymap from to))(&optional args)
       ,(format "Forwards the interactive call from %s to %s (bound by default to `%s')" from to (keymap-lookup nil to))
       (interactive "P")
       ;; By default, fetch the binding bound to `to'
       (let ((to-call (keymap-lookup nil ,to))
             (old-binding (keymap-lookup nil ,from)))
         ;; If exception : then the appropriate command must be fetched in other keymaps
         ;; This is done here by temporarily setting the `from' binding to nil in the input keymap
         (when (my/mode-is-one-of-p ,except-modes)
           (keymap-set ,keymap ,from nil) ; Disable the keybinding temporarily
           (setq to-call (keymap-lookup nil ,from)) ; Get the command bound in other keymaps
           (keymap-set ,keymap ,from old-binding)) ; Restore the original keybinding

         ;; Call the appropriate function
         (call-interactively to-call)))))

;;;; remap : new binding in the same keymap
(defun my/remap(keymap from to)
  "Creates a new binding TO in KEYMAP for the command bound to FROM"
  (let ((existing (keymap-lookup keymap from)))
    (when existing (keymap-set keymap to existing))))

;;;; key-alias-fallback : define binding and rebind existing if it exists
(defmacro key-alias-fallback(keymap from to fallback)
  "Like `key-alias' : binds FROM to function called with TO, remapping existing bindings to FALLBACK"
  `(progn
     (my/remap ,keymap ,from ,fallback)
     (key-alias ,keymap ,from ,to)))

;;;; utility bindings
(keymap-set    my/keys-mode-map "C-+" 'text-scale-increase) ; Increase text size with Ctrl +
(keymap-set    my/keys-mode-map "C--" 'text-scale-decrease) ; Decrease text size with Ctrl -
(key-alias  ijkl-local-mode-map "+" "C-+" '("dired-mode")) ; See above
(key-alias  ijkl-local-mode-map "-" "C-\-") ; See above
(keymap-set ijkl-local-mode-map "TAB" nil)    ; Do not override tab binding
(keymap-set ijkl-local-mode-map "<tab>" nil)  ; Do not override tab binding
(keymap-set ijkl-local-mode-map "h" help-map) ; Use the help functions
(keymap-set ijkl-local-mode-map "x" 'my/delete-char-or-kill-region)
(keymap-set           ctl-x-map "k" 'kill-current-buffer) ; Replace C-x k (kill buffer) with kill-current-buffer
(keymap-set           ctl-x-map "f" 'find-file) ; Replace C-x f (set-fill-column) with find-file (C-x C-f usually)
(keymap-set         ctl-x-r-map "d" 'bookmark-delete) ; Repace C-x r d (delete-rectangle) with delete bookmark
(key-alias  ijkl-local-mode-map "m"   "C-m")
(key-alias     my/keys-mode-map "C-S-m" "S-<return>")
(key-alias     my/keys-mode-map "M-m" "C-<return>")
(key-alias  ijkl-local-mode-map "1"   "C-x 0")
(key-alias  ijkl-local-mode-map "&"   "C-x 1")
(key-alias  ijkl-local-mode-map "2" "C-x 2")
(key-alias  ijkl-local-mode-map "3" "C-x 3")
(keymap-set ijkl-local-mode-map "é"  'my/pick-window-below)
(keymap-set ijkl-local-mode-map "\"" 'my/pick-window-right)
(keymap-set ijkl-local-mode-map "'" 'other-window)
(keymap-set ijkl-local-mode-map "4" 'my/other-window-reverse)
(key-alias ijkl-local-mode-map "w" "C-x C-s")
(keymap-set ijkl-local-mode-map "z" 'recenter-top-bottom)
(keymap-set ijkl-local-mode-map "r" ctl-x-r-map)
(key-alias  ijkl-local-mode-map "c" "M-w")
(key-alias  ijkl-local-mode-map "y" "C-y")
(key-alias  ijkl-local-mode-map "_" "C-_") ; Undo
(key-alias  ijkl-local-mode-map "8" "C-M-_") ; redo
(keymap-set ijkl-local-mode-map "p" 'asmr-backward) ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "n" 'asmr-forward)  ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "P" 'previous-buffer)
(keymap-set ijkl-local-mode-map "N" 'next-buffer)
(key-alias  ijkl-local-mode-map "<SPC>" "C-@")
(keymap-set ijkl-local-mode-map "I" 'er/expand-region)   ; Expand the selection progressively
(keymap-set ijkl-local-mode-map "K" 'er/contract-region) ; Reduce the selection progressively
(keymap-set ijkl-local-mode-map "." 'completion-at-point) ; Trigger completion

;;;; movement and deletion bindings (accessible in both modes)
;;;;; backwards
(keymap-set ijkl-local-mode-map "j"   'backward-char)
(keymap-set    my/keys-mode-map "C-j" 'backward-char)
(key-alias     my/keys-mode-map "M-j" "M-b")
(key-alias     my/keys-mode-map "C-M-j" "C-a")
(key-alias  ijkl-local-mode-map "a" "C-a")

;;;;; forwards
(keymap-set ijkl-local-mode-map   "l" 'forward-char)
(keymap-set    my/keys-mode-map "C-l" 'forward-char)
(key-alias     my/keys-mode-map "M-l" "M-f")
(key-alias     my/keys-mode-map "C-M-l" "C-e")
(key-alias  ijkl-local-mode-map "e" "C-e")

;;;;; upwards
(keymap-set ijkl-local-mode-map "i" 'previous-line)
;; C-i is bound to TAB in terminals. You need to remap C-i to C-p at your GUI app level
;; For example powertoys on windows, konsole or xterm remapping on linux
(when (display-graphic-p)
  (keymap-set input-decode-map "C-i" "C-<i>") ; Disable C-i -> TAB
  (key-alias my/keys-mode-map "C-<i>" "C-p")) ; Rebind C-i as previous line

(keymap-set   my/keys-mode-map "M-i" (lambda() (interactive)(previous-line 7)))
(key-alias    my/keys-mode-map "C-M-i" "M-<")
(key-alias ijkl-local-mode-map "<" "M-<")
(key-alias ijkl-local-mode-map "A" "C-M-a")

;;;;; downwards
(keymap-set    my/keys-mode-map "C-k" 'next-line)
(keymap-set ijkl-local-mode-map   "k" 'next-line)
(keymap-set   my/keys-mode-map "M-k" (lambda() (interactive)(next-line 7)))
(key-alias    my/keys-mode-map "C-M-k" "M->")
(key-alias ijkl-local-mode-map ">" "M->")
(key-alias ijkl-local-mode-map "E" "C-M-e")

;;;;; deletion
(key-alias  ijkl-local-mode-map "u" "C-M-u" '("dired-mode" "Info-mode"))
(keymap-set    my/keys-mode-map "C-u" 'delete-backward-char)
(keymap-set    my/keys-mode-map "C-S-u" 'universal-argument)
(keymap-set    my/keys-mode-map "C-M-u" 'my/delete-start-or-previous-line)
(keymap-set    my/keys-mode-map "M-u" 'backward-kill-word)
(keymap-set    my/keys-mode-map "C-o" 'delete-forward-char)
(keymap-set    my/keys-mode-map "C-M-o" 'kill-line)
(key-alias  ijkl-local-mode-map "o" "C-M-o")
(keymap-set    my/keys-mode-map "M-o" 'kill-word)

;;;; Misc
(keymap-set ijkl-local-mode-map "/"     'my/comment-dwim) ; Comment region or line
(keymap-set ijkl-local-mode-map "M-s"   'multi-occur-in-matching-buffers) ; Search in all buffers
(keymap-set ijkl-local-mode-map "<f2>"  'rename-visited-file) ; Rename the current file/buffer
(keymap-set ijkl-local-mode-map "<f5>"  'revert-buffer-quick) ; Refreshes the current file/buffer without confirmation
(keymap-set ijkl-local-mode-map "<f12>" 'my/include-c-header) ; Shortcuts for a #include directive

;;;; Resize the window when split using split screen (C-2 or C-3)
(keymap-set ijkl-local-mode-map "C-M-<right>" 'enlarge-window-horizontally)
(keymap-set ijkl-local-mode-map "C-M-<left>" 'shrink-window-horizontally)
(keymap-set ijkl-local-mode-map "C-M-<down>" 'enlarge-window)
(keymap-set ijkl-local-mode-map "C-M-<up>" 'shrink-window)

;;;; Hydra tabs
;;;;; Hydra
(defhydra my/hydra-tabs(:hint nil :exit t)
  "
^Create tabs^          ^Move^                   ^Misc^
------------------------------------------------------------------------
_t_: New tab           _n_: Next tab            _s_: Switch to tab
_r_: Rename tab        _p_: Previous tab        _l_: Switch to most recent
_x_: Close tab         _m_: Move tab right      _h_: Toggle tab-bar-mode
_u_: Restore tab       _M_: Move tab left

"
  ("t" tab-new   )     ("n" tab-next        )   ("s" tab-switch)
  ("r" tab-rename)     ("p" tab-previous    )   ("l" tab-recent)
  ("x" tab-close )     ("m" tab-move        )   ("h" tab-bar-mode)
  ("u" tab-undo  )     ("M" my/tab-move-left)

  ("q" nil "Quit"))
(keymap-set ijkl-local-mode-map "t" 'my/hydra-tabs/body)

;;;;; my/tab-move-left
(defun my/tab-move-left()
  "Like `tab-move' but to the left"
  (interactive)
  (tab-move -1))

;;;; Transient for buffer switching commands
;;;;; transient definition
(transient-define-prefix my/transient-buffer()
  "Transient for buffer switching"
  [["Commands"
    ("l" "List buffers" consult-buffer)
    ("p" "Switch between projects"   project-switch-project)
    ("k" "Kill current buffer"       kill-current-buffer)
    ("w" "Kill current window"       delete-window)
    ]
   ["Shortcuts"
   ("b" "Go to last buffer" my/switch-to-last-buffer)
   ("e" "Emacs config file" my/switch-to-emacs-config)
   ("s" "*scratch* buffer"  scratch-buffer)
   ("m" "*messages* buffer" my/switch-to-messages)
   ]])
(keymap-set ijkl-local-mode-map "b" 'my/transient-buffer)

;;;;; my/switch-to-messages
(defun my/switch-to-messages()
  "Switch to *Messages* buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;;; my/switch-to-emacs-config
(defcustom my/emacs-config "~/.config/emacs/init.el"
  "Path to the emacs main config file. Used by `my/switch-to-emacs-config'
for some direct navigation bindings"
  :type 'string)

(defun my/switch-to-emacs-config()
  "Switch to *Messages* buffer"
  (interactive)
  (find-file my/emacs-config))

;;;; Hydra outline
(defhydra my/hydra-outline(:columns 3)
  "outline"
  ("u" outline-up-heading "up")
  ("TAB" outline-toggle-children "toggle hide/show children")
  ("a" outline-show-all "show all")
  ("n" outline-next-visible-heading "next")
  ("l" outline-hide-sublevels "hide level")
  ("s" outline-show-subtree "show all subtree")
  ("p" outline-previous-visible-heading "prev")
  ("h" outline-hide-subtree "hide subtree"))
(keymap-set ijkl-local-mode-map "à" 'my/hydra-outline/body)

;;;; Hydra gdb/gud
(transient-define-prefix my/transient-gdb()
  "Transient for gdb/gud manipulation"
  [["Navigation"
    ("n" "Next line"  gud-next :transient t)
    ("i" "Up frame"   gud-up   :transient t)
    ("k" "Down frame" gud-down :transient t)
    ("s" "Step in"    gud-step :transient t)
    ("m" "Switch to *gud* buffer" gdb-display-gdb-buffer)
    ]
   ["Breakpoints"
   ("b" "Enable breakpoint"  gud-break :transient t)
   ("B" "Disable breakpoint" gud-remove)
   ("v" "Print variable"     gud-print)
   ]
   ["Start/stop"
    ("g" "Start GDB" gud-gdb    :transient t)
    ("p" "Start PDB" pdb        :transient t)
    ("r" "Run"       gud-run    :transient t)
    ("f" "Finish"    gud-finish :transient t)
    ("c" "Continue"  gud-cont   :transient t)
    ]
   ["Misc"
    ("q" "Quit"                   ignore)
    ("&" "Close other windows"    delete-other-windows :transient t)
    ("4" "Switch to other window" other-window         :transient t)
    ]])

;;;; Hydra hide/show
(transient-define-prefix my/transient-hide-show()
  "Transient for `hs-minor-mode' and other display options"
  [["Toggle"
    ("H" "Toggle H/S"            hs-toggle-hiding)
    ("n" "Line number mode"      display-line-numbers-mode)
    ("m" "Margin mode"           my/margin-auto-local-mode)
    ("f" "Fill column indicator" display-fill-column-indicator-mode)
    ("c" "Show coverage"         my/toggle-coverage-overlay)
    ]
   ["Hide"
   ("l" "Hide Level" hs-hide-level)
   ("q" "Hide all"   hs-hide-all)
   ("h" "Hide block" hs-hide-block)
   ]
   ["Show"
    ("s" "Show block" hs-show-block)
    ("a" "Show all"   hs-show-all)
    ]])
(keymap-set ijkl-local-mode-map "H" 'my/transient-hide-show)

;;;; Transient search text
;;;;; Transient definition
(transient-define-prefix my/transient-search() "Transient for all string query/replace"
  [["Incremental search"
    ("s" "Forward"         isearch-forward)
    ("S" "Backward"        isearch-backward)
    ("w" "Symbol at point" isearch-forward-symbol-at-point)
    ]
   ["Occurences"
    ("o" "In file"              consult-line)
    ("b" "In all buffers"       consult-line-multi)
    ("p" "In current project"   consult-grep)
    ("P" "In current project (point)" my/consult-grep)
    ("a" "In current directory" my/consult-directory)
    ]
   ["Replace"
    ("r" "String"  query-replace)
    ("R" "Regexp"  query-replace-regexp)
    ("P" "Project" project-query-replace-regexp)
    ]])
(keymap-set ijkl-local-mode-map "s" 'my/transient-search)

;;;;; my/consult-directory
(defun my/consult-directory()
  "`consult-grep' in `default-directory'"
  (interactive)
  (consult-grep default-directory))

;;;;; my/consult-grep
(defun my/consult-grep()
  "`consult-grep' with current symbol as start string"
  (interactive)
  (funcall #'consult-grep nil (format "%s" (symbol-at-point))))

;;;; Transient register
(defun my/dir-to-register(register)
  "Saves the current directory in a register. Opening it will use dired"
  (interactive (list (register-read-with-preview "Directory to register : ")))
  (set-register register (cons 'file default-directory)))

;;;;; Transient definition
(transient-define-prefix my/transient-register() "Transient for all register operations"
  [["Register - Save"
   ("p" "Save point" point-to-register)
   ("w" "Save window configuration" window-configuration-to-register)
   ("d" "Save directory" my/dir-to-register)
   ("R" "Load register" consult-register)
   ("x" "Clear register" my/clear-register)
   ]])
(keymap-set ijkl-local-mode-map "R" 'my/transient-register)

;;;;; Clear register
(defun my/clear-register(char)
  "Clear register currently referred by `CHAR'"
  (interactive "cClear register at : ")
  (set-register char nil))

;;;; Transient for find commands
;;;;; Transient definition
(transient-define-prefix my/transient-find() "Transient for various find commands"
  [["Find files"
   ("d" "Dired - Current director" dired-jump)
   ("f" "By path"                  find-file)
   ("p" "Project"                  project-find-file)
   ("P" "Project in other window"  my/project-find-file-other-window)
   ("l" "Locate file"              consult-locate)
   ("m" "Man page"                 consult-man)
   ]
   ["Coding"
    ("e" "List errors (file)"     consult-flymake                  :if (lambda() (featurep 'flymake)))
    ("t" "List errors (project)"  flymake-show-project-diagnostics :if (lambda() (featurep 'flymake)))
    ("r" "Find references (xref)" xref-find-references)
    ("o" "Switch header/cpp"      ff-find-other-file)
    ]])
(keymap-set ijkl-local-mode-map "f" 'my/transient-find)

;;;;; my/project-find-file-other-window()
(defun my/project-find-file-other-window()
  "Like `project-find-file' but opens the file in another window"
  (interactive)
  (cl-letf (((symbol-function 'find-file) 'find-file-other-window))
    (project-find-file)))

;;;; Hydra compile
(defhydra my/hydra-compile(:exit t :hint nil)
  "
^Start compilation^             ^Buffer commands^
--------------------------------------------------------------
_e_: Edit command               _o_: Open *compilation* buffer
_ç_: Recompile                  _n_: Next compilation error
_a_: Compile project            _p_: Prev compilation error
_f_: Compile current file       _l_: Cycle error threshold
_k_: Stop compilation
_g_: Start GDB
"
  ("e" compile)             ("o" my/switch-to-compilation-other-window)
  ("ç" my/recompile-switch) ("l" compilation-set-skip-threshold :color red)
  ("a" my/compile-all)      ("n" next-error     :color red)
  ("f" my/compile-file)     ("p" previous-error :color red)
  ("k" kill-compilation)
  ("g" my/transient-gdb))
(keymap-set ijkl-local-mode-map "ç" 'my/hydra-compile/body)

;;;; Hydra go
(transient-define-prefix my/transient-go() "Transient for various navigation commands"
  [["Go to"
    ("l" "Line n°"         consult-goto-line)
    ("p" "Go to project"   project-switch-project)
    ("b" "Bookmark"        bookmark-jump)
    ("," "Org roam file"   org-roam-node-find)
    ("c" "Column n°"       move-to-column)
    ("h" "Outline heading" consult-outline)]
   ["LSP Navigation"
    ("j" "Definition"              xref-find-definitions)
    ("J" "Definition other window" xref-find-definitions-other-window)
    ("m" "Symbol (imenu) in current buffer" consult-imenu)
    ("M" "Symbol (imenu) in all buffers" consult-imenu-multi)
    ("e" "Next error"     flymake-goto-next-error :transient t :if (lambda() (featurep 'flymake)))
    ("E" "Previous error" flymake-goto-prev-error :transient t :if (lambda() (featurep 'flymake)))]
   ["Expressions"
    ("k" "Forward expressionn" forward-sexp     :transient t)
    ("i" "Backward expression" backward-sexp    :transient t)
    ("u" "Upward expression "  backward-up-list :transient t)
    ("g" "Go to opposite end"  my/sexp-other-side)
    ("q" "Quit" ignore)]])
(keymap-set ijkl-local-mode-map "g" 'my/transient-go)

;;;; Rectangle
;;;;; my/replace-char-or-rectangle-region
(defun my/replace-char-or-rectangle-region()
  "If mark is active, rectangle actions, otherwise replace-char"
  (interactive)
  (call-interactively
    (if mark-active
        'my/hydra-rectangle/body
      'my/replace-char-at-point)))

(keymap-set ijkl-local-mode-map "r" 'my/replace-char-or-rectangle-region)

;;;;; Hydra
(defhydra my/hydra-rectangle(:exit t :columns 2)
  "Rectangle operations"
  ("t" string-rectangle "Edition")
  ("k" kill-rectangle "Cut")
  ("c" copy-rectangle-as-kill "Copy")
  ("C" clear-rectangle "Clear")
  ("y" yank-rectangle "Paste")
  ("o" open-rectangle "Insert whitespace")
  ("n" rectangle-number-lines "Number the lines"))

;;;; Org ijkl
(with-eval-after-load "org"
  ;; Use ijkl in the date selection buffer
  (key-alias org-read-date-minibuffer-local-map "m" "RET")
  (key-alias org-read-date-minibuffer-local-map "i" "S-<up>")
  (key-alias org-read-date-minibuffer-local-map "j" "S-<left>")
  (key-alias org-read-date-minibuffer-local-map "k" "S-<down>")
  (key-alias org-read-date-minibuffer-local-map "l" "S-<right>"))

;;;; isearch ijkl
(with-eval-after-load "isearch"
  ;; Make C-u delete the last character of isearch
  ;; Since there is no isearch-del-word, make M-u delete the last 10 characters
  (keymap-set isearch-mode-map "C-u" 'isearch-del-char)
  (keymap-set isearch-mode-map "M-u" (lambda() (interactive) (isearch-del-char 10))))

;;;; Hydra org-roam
(defun my/org-now-time-stamp()
    "Inserts the timestamp of the current time without prompting the user"
  (interactive)
  (org-insert-time-stamp (current-time) t))

(defhydra my/hydra-org(:exit t :hint nil)
  "
^Agenda/TODOs^             ^Edition^                ^Org Roam^
---------------------------------------------------------------------------------
_a_: Show agenda           _l_: Insert link         _g_: Go to an org-roam file
_s_: Set scheduled date    _n_: Now timestamp       _i_: Link to an org-roam file
_d_: Set deadline date     _N_: Pick timestamp      _h_: Info for file
_t_: Cycle TODOs states    _y_: Yank(paste) image   _S_: Sync
_L_: List TODOs            _,_: Toggle images

"
  ("a" org-agenda-list)        ("l" org-insert-link)           ("i" org-roam-node-insert)
  ("s" org-schedule)           ("t" org-todo)                  ("g" org-roam-node-find)
  ("d" org-deadline)           ("y" org-download-clipboard)    ("h" org-roam-buffer-toggle )
  ("n" my/org-now-time-stamp)  ("L" org-todo-list)             ("S" my/org-roam-pull-commit-push)
  ("N" org-time-stamp)         ("," org-toggle-inline-images)

  ("q" nil "Quit"))
(keymap-set ijkl-local-mode-map "," 'my/hydra-org/body)

;;;; Hydra commands
(defhydra my/hydra-commands(:exit t :columns 2)
  "Execute commands"
  ("!" execute-extended-command "Emacs command")
  ("x" execute-extended-command)
  (":" eval-expression "Interpret lisp code")
  ("l" eglot "Start eglot (LSP)")
  ("e" eval-last-sexp "Interpret last lisp expression")
  ("s" shell-command "Shell Command")
  ("b" eval-buffer "Interpret the whole lisp buffer"))
(keymap-set ijkl-local-mode-map "!" 'my/hydra-commands/body)

;;;; Magit transient
(transient-define-prefix my/transient-magit()
  "Transient for `magit' commands"
  [["Magit commands"
    ("s" "Status (Home)"   magit-status)
    ("f" "File commands"   magit-file-dispatch)
    ("v" "Global Commands" magit-dispatch)
    ("c" "Clone"           magit-clone)
    ]])
(keymap-set ijkl-local-mode-map "v" 'my/transient-magit)

;;;; Narrow Hydra
(defhydra my/hydra-narrow(:exit t :columns 1)
  "Narrow the displayed buffer to a subsection"
  ("N" narrow-to-region "Narrow to selected region")
  ("d" narrow-to-defun "Narrow to function")
  ("f" narrow-to-defun)
  ("w" widen "Widen (cancel narrowing)"))
(keymap-set ijkl-local-mode-map "N" 'my/hydra-narrow/body)

;;;; Magit ijkl
(with-eval-after-load "magit"
  (key-chord-define magit-log-select-mode-map "CC" 'magit-log-select-pick)
  (key-chord-define magit-log-select-mode-map "QQ" 'magit-log-select-quit)
  (dolist (keymap (list magit-diff-section-base-map magit-mode-map))
    (key-alias keymap "&"  "C-x 1")
    (key-alias keymap "é"  "C-x 2")
    (key-alias keymap "\"" "C-x 3")
    (keymap-set keymap "'" 'other-window)
    (keymap-set keymap "4" 'other-window-reverse)
    (key-alias-fallback keymap "m" "RET" "C-c m")
    (key-alias-fallback keymap "j" "C-j" "C-c j")
    (key-alias-fallback keymap "i" "C-p" "C-c i")
    (key-alias-fallback keymap "l" "C-l" "C-c l")
    (key-alias-fallback keymap "k" "C-n" "C-c k")))
(with-eval-after-load "git-rebase"
  (key-alias-fallback git-rebase-mode-map "m" "RET" "C-c m")
  (key-alias-fallback git-rebase-mode-map "j" "C-l" "C-c j")
  (key-alias-fallback git-rebase-mode-map "i" "C-p" "C-c i")
  (key-alias-fallback git-rebase-mode-map "k" "C-n" "C-c k")
  (key-alias-fallback git-rebase-mode-map "l" "C-f" "C-c l")
  (keymap-set git-rebase-mode-map "d" 'git-rebase-kill-line))
(with-eval-after-load "with-editor"  ; Called for commits
  (diminish "with-editor-mode")
  (key-chord-define with-editor-mode-map "CC" 'with-editor-finish)
  (key-chord-define with-editor-mode-map "QQ" 'with-editor-cancel))
