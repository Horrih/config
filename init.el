;;; Enable lexical binding -*- lexical-binding: t -*-
;;Lexical binding enables using variables defined with let in lambda functions called later
;;; Package management
;;;; Optimize garbage collection : improves the startup time
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()(setq gc-cons-threshold 800000)))

;;;; Enable MELPA : Add the main user repository of packages
;; cf Getting Started https://melpa.org/
;; ELPA, the default repository, has much less available
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat "http" "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;; use-package : Use package will be used as a package loader in this file
;; Install the package if not available yet
(use-package use-package
  :custom
  (use-package-always-ensure t) ; Download missing packages by default
  (use-package-always-defer t) ; Lazy load by default, use :demand otherwise
)

;;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish
  :demand
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

(use-package simple
  :ensure nil
  :config
  (diminish 'auto-fill-function))

;;;; esup : Launch the esup command to measure startup time of each emacs plugin
(use-package esup
  :custom (esup-depth 0)) ; Sometimes fixes the bug https://github.com/jschaf/esup/issues/54

;;; Various customizations options
;;;; my-keys minor mode for global keybindings overriding
(define-minor-mode my-keys-mode
  "Minor mode to enable custom keybindings"
  :lighter ""
  :global t
  :keymap '())
(my-keys-mode)

;;;; Main color theme : vscode-dark-plus-theme
(use-package vscode-dark-plus-theme
  :demand
  :config (load-theme 'vscode-dark-plus t))

;;;; Mode line theme : doom mode line
(use-package doom-modeline
  :demand
  :custom-face
  (mode-line ((t :background "black")))
  (mode-line-inactive ((t :background "#333333"))) ; Dark grey
  :custom
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes t)
  :init
  (doom-modeline-mode))

;;;; All the icons
;; TODO on 1st install : use all-the-icons-install-fonts
;; Caskaydia => https://www.nerdfonts.com/font-downloads
;; Symbola => https://fontlibrary.org/fr/font/symbola
(use-package all-the-icons
  :if (display-graphic-p))

;;;; Dired as default buffer
(when (< (length command-line-args) 2)
  (add-hook 'after-init-hook 'dired-jump))

;;;; Dashboard as default buffer
(use-package dashboard
  :disabled
  :demand
  :hook (dashboard-mode . (lambda()(setq-local show-trailing-whitespace nil)))
  :diminish dashboard-mode
  :custom (dashboard-items '((projects . 5)
                             (bookmarks . 10)
                             (recents  . 10)))
  :config
  (dashboard-setup-startup-hook))

;;;; Misc
(progn
  (tool-bar-mode 0) ; Disable the toolbar in GUI mode
  (customize-set-variable 'scroll-bar-mode nil)
  (customize-set-variable 'horizontal-scroll-bar-mode nil)
  (setq inhibit-startup-screen t) ; Hide the startup screen
  (savehist-mode) ; Save history for commands
  (setq isearch-resume-in-command-history t) ; Use history for isearch as well
  (global-auto-revert-mode) ; Refresh files automatically when modified from outside emacs
  (setq enable-local-eval t) ; Enable eval blocks in .dir-locals.el
  (setq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
  (setq ring-bell-function 'ignore) ; Disable the bell for emacs
  (setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
  (setq completions-detailed t) ; Detailed description for the built in describe symbol etc
  (column-number-mode t) ; Display column numbers in the status line
  (global-display-line-numbers-mode t) ; Display line numbers on the left
  (line-number-mode t) ; Display line number
  (size-indication-mode t) ; Display size indication
  (delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
  (show-paren-mode 1) ; Highlight the matching parenthesis
  (setq-default show-trailing-whitespace t) ; Show in red the spaces forgotten at the end of lines
  (setq-default indent-tabs-mode nil) ; Use spaces for indent
  (setq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
  (menu-bar-mode -1) ; Hide Menu bar
  (setq use-short-answers t) ; Abreviate Yes/No to y or n
  (setq default-tab-width 4) ; Number of spaces inserted by tab
  (setq-default c-basic-offset  4) ; Base indent size when indented automatically
  (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
  (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
  (customize-set-variable 'recenter-positions '(top middle bottom)) ; Start recenter on top instead of middle
  (setq make-backup-files nil) ; Do not use backup files (filename~)
  (setq create-lockfiles nil)) ; Do not use lock files (.#filename)

;;;; switch-to-last-buffer
(defun switch-to-last-buffer()
  "Use `switch-to-buffer' to visit the last buffer"
  (interactive)
  (switch-to-buffer nil))

;;;; delete-start-or-previous-line
(defun delete-start-or-previous-line()
  "Use `kill-line' to delete either the start of the line, or the previous line if empty"
  (interactive)
  (kill-line (if (= (line-beginning-position) (point)) -1 0)))

;;;; match-buffer-extension
(defun match-buffer-extension(&rest extensions)
  "Returns t if the current buffer has an extension in EXTENSIONS"
  (if (member (file-name-extension (buffer-name)) extensions)
      t))

;;;; other-window-reverse
(defun other-window-reverse()
  "Like `other-window' but in the reverse order"
  (interactive)
  (other-window -1))

;;;; replace-chat-at-point
(defun replace-char-at-point(char)
  "Replaces the caracter at point by `CHAR'"
  (interactive "cReplace character at point with : ")
  (delete-char 1)
  (insert-char char)
  (backward-char 1))

;;;; delete-char-or-kill-region
(defun delete-char-or-kill-region()
  "If mark is active, kill region, otherwise delete-char"
  (interactive)
  (call-interactively
    (if mark-active
        'kill-region
      'delete-char)))

;;;; replace-char-or-rectangle-region
(defun replace-char-or-rectangle-region()
  "If mark is active, rectangle actions, otherwise replace-char"
  (interactive)
  (call-interactively
    (if mark-active
        'rectangle/body
      'replace-char-at-point)))

;;; Compilation options
;;;; Compilation misc
(use-package compile
  :ensure nil ; Emacs built in
  :hook (compilation-mode . (lambda()(setq show-trailing-whitespace nil)))
  :custom
  (compilation-always-kill t) ; Do not ask for confirmation when I stop current compilation
  (compilation-message-face 'all-the-icons-green))

;;;; switch-to-compilation-other-window()
(defun switch-to-compilation-other-window()
  "Switches to the compilation buffer in another window"
  (interactive)
  (unless (string-equal "*compilation*" (buffer-name))
    (switch-to-buffer-other-window "*compilation*")))

(defun switch-to-compilation-other-window-end()
  "Switches to the compilation buffer in another window and go to buffer end"
  (interactive)
  (switch-to-compilation-other-window)
  (end-of-buffer))

;;;; recompile-switch
(defun recompile-switch()
  "Uses the recompile function and switches to the buffer end"
  (interactive)
  (recompile)
  (switch-to-compilation-other-window-end))

;;;; compile-all
(defcustom compile-all-command nil
  "If non nil, `compile-all' will use it as command instead of `compile-command'
This can be useful in conjunction to projectile's .dir-locals variables"
  :type 'string
  :risky nil)

(defun compile-all()
  "Compiles the whole project and switch to buffer end"
  (interactive)
  (compile (or compile-all-command "make -j8"))
  (switch-to-compilation-other-window-end))

;;;; compile-file
(defun compile-file(file-name)
  "Compiles the file FILE-NAME using a command to be define `compile-file-command'
  This function should take a filename as parameter and returning the command as output"
  (interactive (list (buffer-file-name)))
  (unless (fboundp 'compile-file-command)
    (error "compile-file expects the compile-file-command function to be defined"))
  (compile (compile-file-command file-name))
  (switch-to-compilation-other-window-end))

;;;; ansi-color : Translate TTY escape sequences into colors
(defun ansi-color-compilation-filter-except-ag()
  "Like `ansi-color-compilation-filter', except on buffers generated by the ag package.
   If we use vanilla ansi-color-compilation-filter, the colors get messed up"
  (unless (string-match "ag search text" (buffer-name))
    (ansi-color-compilation-filter)))

(use-package ansi-color
  :ensure nil ; Emacs built-in
  :hook (compilation-filter . ansi-color-compilation-filter-except-ag)) ; Handle terminal colors in the compilation buffer

;;;; regexps : Set compilation regex for errors
(let ((enabled-regexps ())
      (custom-error-list '(
        ;; Insert your custom regexps here
        (link-error "^\\(.*\\):\\([0-9]+\\): undefined reference to.*$" 1 2)
        (jest-error "^.*\(\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\).*$" 1 2 3)
        (gcc-error "^[ ]*\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*error:\\|  required from here\\).*$" 1 2 3)
        (gcc-warning "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): warning:.*$" 1 2 3 1)
        (gcc-info "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): note:.*$" 1 2 3 0)
        (qt-test "^   Loc: \\[\\(.*\\)\(\\([0-9]+\\)\)\\]$" 1 2)
        (python-unittest "^  File \"\\(.*\\)\", line \\([0-9]+\\),.*$" 1 2))))
  (dolist (err custom-error-list)
    (add-to-list 'enabled-regexps (car err)))
  (custom-set-variables `(compilation-error-regexp-alist ',enabled-regexps))
  (add-hook 'compilation-mode-hook (lambda()
    (dolist (err custom-error-list)
      (add-to-list 'compilation-error-regexp-alist-alist err)))))

;;; General usage packages
;;;; magit : Git front end (amazing!)
(use-package magit
  :custom-face (magit-filename ((t :foreground "white"))) ; Otherwise untracked files have the same color as title in git status
  :custom
  (magit-no-confirm t) ; Do not ask for confirmation for actions
  (magit-visit-ref-behavior '(checkout-any focus-on-ref))) ; Enter on branch names makes you checkout the branch

;;;; ediff : Built in side by side diffs of files
(use-package ediff
  :ensure nil ; Built-in
  :hook (ediff-keymap-setup . (lambda()
                        (keymap-set ediff-mode-map "h" 'ediff-status-info)
                        (keymap-set ediff-mode-map "'" 'other-window)
                        (keymap-set ediff-mode-map "4" 'other-window-reverse)
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
  :custom (key-chord-two-keys-delay 0.1)
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
  :bind (:map my-keys-mode-map
         ("M-y" . consult-yank-pop)
         :map help-map
         ("a" . consult-apropos)
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
  :ensure nil  ; emacs built-in
  :bind (:map dired-mode-map ("u" . dired-up-directory))
  :custom(dired-kill-when-opening-new-dired-buffer t)) ; Auto close previous folder buffer

;;;; Org mode : Base mode for note taking
(use-package org
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
  :hook (org-mode . (lambda()
                      (require 'org-tempo) ; For templates like <sTAB to insert a code block
                      (require 'recentf)
                      (add-to-list 'recentf-exclude ".*org$") ; Ignore org files from recentf due to agenda loading everything
                      (org-indent-mode) ; Auto indent lines according to depth
                      (auto-fill-mode)))) ; Wrap lines when longer than fill column

(use-package org-indent
  :ensure nil
  :diminish)

;;;; Org org-agenda-other-window-no-switch()
(defun org-agenda-other-window-no-switch()
  "Opens the org agenda (monthly view) in a side window without leaving the current window"
  (interactive)
  (save-selected-window (org-agenda-list)))

;;;; Org bullets : Pretty mode for org
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;;; org-roam : Notes organizing
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-return-follows-link t)
  (org-roam-directory "~/.config/emacs/org_roam")
  (org-roam-completion-everywhere t))

(defun org-roam-pull-commit-push()
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
  ;; Override the default dir to have one dir for each org file
  (defun org-download--dir-1 ()
    (or org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))))

;;;; visual-fill-column : Center text in the window and wrap around fill-column
(use-package visual-fill-column
  :custom ((visual-fill-column-width 130)
           (visual-fill-column-center-text t)))

;;;; org-present : Using org files for powerpoints
(setq my/original-default_face 'default)
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
;;;; ag and projectile-ag : Front end for the CLI utility ag
(use-package ag
  :custom (ag-highlight-search t))

;;;; Projectile : git project functions, like the built in project but better
(use-package projectile)

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; treemacs : Displays the current project on the left as in an IDE
(use-package treemacs
  :custom (treemacs-no-delete-other-windows nil))

;;;; web-mode : Support various web files
(use-package web-mode
  :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.vue\\'")
  :hook
  (web-mode . (lambda () (when (match-buffer-extension "ts" "js" "vue")
                           (lsp-deferred)
                           (setq-local lsp-auto-format t))))
  :custom
  (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
  (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

;;;; prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
  :custom
  (prettier-js-show-errors nil))

;;;; Outline mode with package outline-minor-faces and outshine
;;;;; Enable sane bindings and actions for outline mode
(use-package outline
  :ensure nil ; emacs built-in
  :hook
  (emacs-lisp-mode . outline-minor-mode)
  :config
  (diminish 'outline-minor-mode)
  :custom
  (outline-minor-mode-cycle t) ; Tab and S-Tab cycle between different visibility settings
  (outline-minor-mode-cycle-filter 'bolp))

;;;;; Pretty colors for headings
;; We don't use (outline-minor-mode-highlight 'override) because it applies to some non headings as well
(use-package outline-minor-faces
   :hook (outline-minor-mode . outline-minor-faces-mode))

;;;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
  :mode "\\.csv\\'")

;;;; yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode
  :mode "\\.yml\\'")

;;;; hide-show-mode : Hide/show sections of code : current function, class, or if/else section
(use-package hideshow
  :ensure nil ; Built-in emacs
  :config
  (diminish 'hs-minor-mode)
  :hook
  (prog-mode . hs-minor-mode))

;;;; include-guards(text) : Add include guards to the current file
(defun include-guards(text)
  "Adds include guards in the current file, useful for C/C++ devs

It will add the following code :

     #ifndef TEXT
     #define TEXT
     // Current file content
     #endif //TEXT
"
  (interactive
   (list
    (let* ((default (replace-regexp-in-string "\\." "_" (upcase (buffer-name))))
           (prompt (format "Include guard text (default %s): " default)))
      (read-string prompt nil  nil default))))
  (save-excursion
    (goto-char 0)
    (insert (format "#ifndef %s\n#define %s\n" text text))
    (goto-char (max-char))
    (insert (format "#endif // %s" text))
    ))

;;;; include-c-header-val() : Inserts a #include directive for C/C++
(defun include-c-header(val)
  "Adds a #include \"VAL.h\" at point and saves the file"
  (interactive "MHeader file name: ")
  (insert (format "#include \"%s.h\"\n" val))
  (save-buffer))

;;;; c++ mode
(use-package cc-mode
  :ensure nil  ; Part of emacs
  :hook (c++-mode . lsp-deferred)
  :config
  (advice-add 'c-update-modeline :override #'ignore)) ;; Don't use a modeline suffix (i.e C++//l)


;;; LSP + DAP : completion, linting, debugging
;;;; lsp-treemacs : treemacs style views for various lsp results
(use-package lsp-treemacs)

;;;; company : Completion frontend, used by lsp
(use-package company
  :diminish
  :hook (emacs-lisp-mode . company-mode))

;;;; yasnippet : Dependency used by lsp to insert snippets. Used by some lsp commands like completion
(use-package yasnippet
  :hook (lsp-mode . (lambda()
                      (yas-minor-mode)
                      (diminish 'yas-minor-mode))))

;;;; cmake-mode : Major mode for CMakeLists.txt
(use-package cmake-mode
  :custom (cmake-tab-width 4)
  :mode ("CMakeLists\\.txt"))

;;;; dap-mode : Debug adapter protocol for emacs
;; For c++ Install mono on linux then run dap-cpptools-setup for c++
;; For dap-firefox : download and unzip the package in ~/.emacs.d/.extension/vscode/
;; and modify the path to the executable js file which has been renamed to adapter.bundle.js
;; For any language, require then the appropriate packages, and use a launch.json at your lsp root
(use-package dap-mode
  :hook
  (c++-mode . (lambda()(require 'dap-cpptools)))
  (python-mode . (lambda()(require 'dap-python)))
  (web-mode . (lambda()(require 'dap-firefox))))

;; UI settings for dap-mode (comes with the dap-mode package)
(use-package dap-ui
  :ensure nil
  :config
  (unless (display-graphic-p)
    (set-face-background 'dap-ui-marker-face "color-166") ; An orange background for the line to execute
    (set-face-attribute 'dap-ui-marker-face nil :inherit nil) ; Do not inherit other styles
    (set-face-background 'dap-ui-pending-breakpoint-face "blue") ; Blue background for breakpoints line
    (set-face-attribute 'dap-ui-verified-breakpoint-face nil :inherit 'dap-ui-pending-breakpoint-face)))


;;;; flycheck : Syntax highlighting, used by lsp
(use-package flycheck
  ;; Add a flake8 for python. Needs to be done after lsp-diagnostics has been loaded
  :hook (lsp-diagnostics-mode . (lambda()(flycheck-add-next-checker 'lsp 'python-flake8))))

;;;; lsp-mode : Completion and syntax highlighting backend API, available for most languages
;; The following packages need to be installed according to the language
;; Python : pip install pyright flake8
;; c++ : pacman -S clang bear (or jq)
;; vue.js, javascript, typescript : sudo npm install -g vls typescript-language-server
(use-package lsp-mode
  :hook
  (lsp-mode    . lsp-enable-which-key-integration)
  :bind (("C-h l" . lsp-describe-thing-at-point))
  :custom
  ;; Formatting options for vue.js (.vue files)
  (lsp-enable-links nil) ; Make links non clickable
  :config
  (setq lsp-headerline-arrow ">")) ; Material design icon not working on windows

;;;; inhibit-lsp-mode : When turned on, lsp won't trigger unless manually calling 'lsp
(define-minor-mode inhibit-lsp-mode
  "Minor mode to temporarily inhibit the `lsp-deferred' command"
  :lighter " NoLSP"
  :global t
  (if inhibit-lsp-mode
      (advice-add 'lsp-deferred :override #'ignore) ;; lsp-deferred will be inhibited
    (advice-remove 'lsp-deferred #'ignore)))

;;;; lsp-format-and-save : format on save if lsp-auto-format is not nil
(defcustom lsp-auto-format nil
  "If not nil, lsp-format-and-save will format the buffer before saving"
   :type 'boolean)

(defun lsp-format-and-save()
  "Saves the current buffer and formats it if lsp-format-on-save is not nil"
  (interactive)
  (when (and (not buffer-read-only) lsp-auto-format)
    (lsp-format-buffer))
  (save-buffer))

;;;; lsp-pyright : An LSP backend for python
(use-package lsp-pyright
  :hook (python-mode . (lambda()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

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

;;;; forward-mark() : main next function
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
  "Minor mode to be able to move using ijkl"
  :lighter " ijkl"
  :keymap '(([remap self-insert-command]  ignore)) ; The actual keymaps are defined later below
  (add-to-list 'emulation-mode-map-alists '(ijkl-local-mode . ijkl-local-mode-map))
  )
(keymap-set ijkl-local-mode-map "d" 'ijkl-local-mode)

(defun ijkl-local-mode-and-save()
  "Enables ijkl-local-mode and saves the current file if applicable"
  (interactive)
  (ijkl-local-mode)
  (when (and (buffer-modified-p) buffer-file-name)
    (save-buffer)))
(key-chord-define my-keys-mode-map "sd" 'ijkl-local-mode-and-save)
(key-chord-define my-keys-mode-map "qs" 'ijkl-local-mode)
(keymap-set my-keys-mode-map "C-q" 'ijkl-local-mode) ; Fallback if key-chord fails
(diminish 'ijkl-local-mode)

;;;; ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode on traditional buffers"
    (unless (or (minibufferp)
                (string-match "[Gg]it" (format "%s" major-mode))
                (string-match "[Gg]it" (format "%s" major-mode))
                (string-equal (buffer-name) "*Ediff Control Panel*")
                (string-equal (buffer-name) "COMMIT_EDITMSG"))
      (ijkl-local-mode))))
(ijkl-mode)

;;;; Change color of the mode line according to the mode (command, edit, unsaved)
(let ((default-color (face-background 'mode-line)))
  (add-hook 'post-command-hook
            (lambda ()
              (set-face-background 'mode-line
                                   (cond ((not ijkl-local-mode) "red")
                                         (t default-color))))))

;;; Main keybindings
;;;; Helper function gen-input
(defun gen-input(KEYS)
  "Generates a key `KEYS' sequence as if the user typed it"
  (setq unread-command-events (nconc (listify-key-sequence (kbd KEYS)) unread-command-events)))

;;;; Helper function - mode-is-one-of-p
(defun mode-is-one-of-p(modes)
  "Returns t if the current modes (minor or major) matches one in the input modes list"
  (let (res)
    (dolist (input-mode modes res)
      (dolist (mode (cons major-mode minor-mode-list))
        (when (string-equal input-mode mode)
          (setq res t))))))

;;;; Helper macro key-alias (mostly obsolete in emacs 29 with the introduction of keymap-set)
(defmacro key-alias(keymap from to &optional exceptions)
  "Binds the key-binding FROM to the function called by typing TO.

The forwarding will only occur if the current major mode is not in EXCEPTIONS list"
  `(define-key ,keymap ,(kbd from)
     (defun ,(intern (format "%s-alias/%s/%s" keymap from to))(&optional args)
       ,(format "Forwards the interactive call from %s to %s (bound by default to `%s')" from to (key-binding (kbd to)))
       (interactive "P")
       ;; By default, fetch the binding bound to `to'
       (let ((to-call (key-binding ,(kbd to)))
             (old-binding (key-binding ,(kbd from))))
         ;; If exception : then the appropriate command must be fetched in other keymaps
         ;; This is done here by temporarily setting the `from' binding to nil in the input keymap
         (when (mode-is-one-of-p ,exceptions)
           (define-key ,keymap ,(kbd from) nil) ; Disable the keybinding temporarily
           (setq to-call (key-binding ,(kbd from))) ; Get the command bound in other keymaps
           (define-key ,keymap ,(kbd from) old-binding)) ; Restore the original keybinding

         ;; Call the appropriate function
         (call-interactively to-call)))))

;;;; keymap-set-remap : define binding and rebind existing if it exists
(defmacro keymap-set-remap(keymap keys command fallback)
  "Like `keymap-set' : binds KEYS to COMMAND but remaps the existing binding to FALLBACK"
  `(progn
     (keymap-substitute ,keymap ,keys ,fallback)
     (keymap-set ,keymap ,keys ,command)))

;;;; utility bindings
(keymap-set ijkl-local-mode-map "C-+" 'text-scale-increase) ; Increase text size with Ctrl +
(keymap-set ijkl-local-mode-map "C--" 'text-scale-decrease) ; Decrease text size with Ctrl -
(keymap-set ijkl-local-mode-map "TAB" nil)    ; Do not override tab binding
(keymap-set ijkl-local-mode-map "<tab>" nil)  ; Do not override tab binding
(keymap-set ijkl-local-mode-map "h" help-map) ; Use the help functions
(keymap-set ijkl-local-mode-map "x" 'delete-char-or-kill-region) ; Bind x to the delete-char-or-kill-region command
(keymap-set ctl-x-map "k" 'kill-current-buffer) ; Replace C-x k (kill buffer) with kill-current-buffer
(keymap-set ctl-x-map "f" 'find-file) ; Replace C-x f (set-fill-column) with find-file (C-x C-f usually)
(keymap-set ctl-x-r-map "d" 'bookmark-delete) ; Repace C-x r d (delete-rectangle) with delete bookmark
(keymap-set ijkl-local-mode-map "m" "C-m")
(keymap-set my-keys-mode-map "M-m" "M-<RET>")
(keymap-set ijkl-local-mode-map "&"  "C-x 1")
(keymap-set ijkl-local-mode-map "é"  "C-x 2")
(keymap-set ijkl-local-mode-map "\"" "C-x 3")
(keymap-set ijkl-local-mode-map "'" 'other-window)
(keymap-set ijkl-local-mode-map "4" 'other-window-reverse)
(keymap-set ijkl-local-mode-map "w" 'lsp-format-and-save)
(keymap-set ijkl-local-mode-map "z" 'recenter-top-bottom)
(keymap-set ijkl-local-mode-map "r" ctl-x-r-map)
(keymap-set ijkl-local-mode-map "c" "M-w")
(keymap-set ijkl-local-mode-map "y" "C-y")
(keymap-set ijkl-local-mode-map "_" "C-_")
(keymap-set ijkl-local-mode-map "p" 'asmr-backward) ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "n" 'asmr-forward)  ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "<SPC>" "C-@")
(keymap-set ijkl-local-mode-map "I" 'er/expand-region)   ; Expand the selection progressively
(keymap-set ijkl-local-mode-map "K" 'er/contract-region) ; Reduce the selection progressively

;;;; movement and deletion bindings (accessible in both modes)
;;;;; backwards
(keymap-set ijkl-local-mode-map "j"   "C-j")
(keymap-set    my-keys-mode-map "C-j" "C-b")
(keymap-set    my-keys-mode-map "M-j" "M-b")
(keymap-set    my-keys-mode-map "C-M-j" "C-a")
(keymap-set ijkl-local-mode-map "a" "C-a")

;;;;; forwards
(keymap-set    my-keys-mode-map "C-l" "C-f")
(keymap-set ijkl-local-mode-map "l"   "C-l")
(keymap-set    my-keys-mode-map "M-l" "M-f")
(keymap-set    my-keys-mode-map "C-M-l" "C-e")
(keymap-set ijkl-local-mode-map "e" "C-e")

;;;;; upwards
(keymap-set ijkl-local-mode-map "i" "C-p")
;; C-i is bound to TAB in terminals. You need to remap C-i to C-p at your GUI app level
;; For example powertoys on windows, konsole or xterm remapping on linux
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" [C-i])   ; Disable C-i -> TAB
  (key-alias ijkl-local-mode-map "<C-i>" "C-p")) ;; Rebind C-i as previous line

(keymap-set my-keys-mode-map "M-i" (lambda() (interactive)(previous-line 7)))
(keymap-set my-keys-mode-map "C-M-i" "M-<")
(keymap-set ijkl-local-mode-map "<" "M-<")
(keymap-set ijkl-local-mode-map "A" "C-M-a")

;;;;; downwards
(keymap-set ijkl-local-mode-map "k" "C-n")
(keymap-set    my-keys-mode-map "M-k" (lambda() (interactive)(next-line 7)))
(keymap-set    my-keys-mode-map "C-M-k" "M->")
(keymap-set ijkl-local-mode-map ">" "M->")
(keymap-set ijkl-local-mode-map "E" "C-M-e")

;;;;; deletion
(key-alias ijkl-local-mode-map "u" "C-M-u" '("dired-mode" "Info-mode"))
(keymap-set    my-keys-mode-map "C-u" 'delete-backward-char)
(keymap-set    my-keys-mode-map "C-M-u" 'delete-start-or-previous-line)
(keymap-set    my-keys-mode-map "M-u" 'backward-kill-word)
(keymap-set    my-keys-mode-map "C-o" 'delete-forward-char)
(keymap-set    my-keys-mode-map "C-M-o" 'kill-line)
(keymap-set ijkl-local-mode-map "o" "C-M-o")
(keymap-set    my-keys-mode-map "M-o" 'kill-word)

;;;; Misc
(keymap-set ijkl-local-mode-map "/"     'comment-or-uncomment-region) ; Comment all the lines of the selected area
(keymap-set ijkl-local-mode-map "M-s"   'multi-occur-in-matching-buffers) ; Search in all buffers
(keymap-set ijkl-local-mode-map "<f2>"  'rename-visited-file) ; Rename the current file/buffer
(keymap-set ijkl-local-mode-map "<f5>"  'revert-buffer-quick) ; Refreshes the current file/buffer without confirmation
(keymap-set ijkl-local-mode-map "<f6>"  'revert-all-file-buffers) ; Refreshes all the current files/buffers
(keymap-set ijkl-local-mode-map "<f12>" 'include-c-header) ; Shortcuts for a #include directive

;;;; Resize the window when split using split screen (C-2 or C-3)
(keymap-set ijkl-local-mode-map "M-S-<right>" 'enlarge-window-horizontally)
(keymap-set ijkl-local-mode-map "M-S-<left>" 'shrink-window-horizontally)
(keymap-set ijkl-local-mode-map "M-S-<down>" 'enlarge-window)
(keymap-set ijkl-local-mode-map "M-S-<up>" 'shrink-window)

;;;; Hydra buffer
(defcustom emacs-config-main "~/.config/emacs/init.el"
  "Path to the emacs main config file. Used for some direct navigation bindings"
  :type 'string)

(defhydra buffer(:columns 3 :exit t)
  "Buffer actions"
  ("b" switch-to-last-buffer "Last buffer")
  ("l" consult-buffer "Show buffer list")
  ("s" scratch-buffer  "Switch to *scratch* buffer")
  ("B" consult-buffer-other-window "Open in other window")
  ("k" kill-current-buffer "Kill buffer")
  ("m" (lambda() (interactive)(switch-to-buffer "*Messages*"))  "Switch to *Messages* buffer")
  ("e" (lambda() (interactive)(find-file emacs-config-main)) "Switch to emacs config file"))
(keymap-set ijkl-local-mode-map "b" 'buffer/body)

;;;; Hydra outline
(defhydra outline(:columns 3)
  "outline"
  ("u" outline-up-heading "up")
  ("TAB" outline-toggle-children "toggle hide/show children")
  ("a" outline-show-all "show all")
  ("n" outline-next-visible-heading "next")
  ("l" outline-hide-sublevels "hide level")
  ("s" outline-show-subtree "show all subtree")
  ("p" outline-previous-visible-heading "prev")
  ("h" outline-hide-subtree "hide subtree"))
(keymap-set ijkl-local-mode-map "à" 'outline/body)

;;;; Hydra gdb/gud
(defhydra hydra-gdb(:columns 4 :color pink :foreign-keys run)
  "GDB"
  ("n" gud-next "Step")
  ("u" gud-up "Up")
  ("b" gud-break "Breakpoint on")
  ("g" gdb "Launch GDB")
  ("s" gud-step "Step in")
  ("d" gud-down "Down")
  ("B" gud-remove "Breakpoint off")
  ("r" gud-run "Run")
  ("f" gud-finish "Finish")
  ("p" gud-print "Print")
  ("c" gud-go "Continue")
  ("q" nil "Quit" :color blue))

;;;; Hydra hide/show
(defhydra hydra-hide-show (:exit t :columns 2)
  "Hydra for hide-show commands"
  ("t" hs-toggle-hiding "Toggle H/S")
  ("H" hs-toggle-hiding)
  ("l" hs-hide-level "Hide Level")
  ("q" hs-hide-all "Hide all")
  ("s" hs-show-block "Show block")
  ("a" hs-show-all "Show all")
  ("h" hs-hide-block "Hide block"))
(keymap-set ijkl-local-mode-map "H" 'hydra-hide-show/body)

;;;; Hydra search text
(defhydra search(:exit t :columns 3)
  "Text search related commands"
  ("o" consult-line "Occurences in file")
  ("s" isearch-forward "Next occurence in file")
  ("S" isearch-backward "Previous occurence in file")
  ("w" isearch-forward-symbol-at-point "Next occurence in file of word")
  ("r" query-replace "Next occurence in file")
  ("a" (lambda() (interactive) (consult-grep default-directory)) "Grep in current directory")
  ("p" projectile-ag "Grep in current project")
  ("P" projectile-replace "Replace in current project")
  ("b" multi-occur-in-matching-buffers "Occur in all buffers"))
(keymap-set ijkl-local-mode-map "s" 'search/body)

;;;; Hydra find
(defhydra find(:exit t :columns 2)
  "Search related commands"
  ("d" dired-jump "Open current directory in dired")
  ("f" find-file "Find file by URL")
  ("e" flycheck-list-errors "Errors current file (flycheck + LSP)")
  ("t" lsp-treemacs-errors-list "Errors current project (LSP treemacs)")
  ("r" xref-find-references "Find references (xref)")
  ("o" ff-find-other-file "switch header/cpp")
  ("p" project-find-file "project-find-file")
  ("P" projectile-find-file-other-window "projectile-find-file-other-window"))
(keymap-set ijkl-local-mode-map "f" 'find/body)

;;;; Hydra compile
(defhydra compile(:exit t :columns 3)
  "Compilation commands"
  ("ç" recompile-switch "Reuse last compilation command")
  ("e" compile "Edit the compilation command")
  ("a" compile-all "Compile STC")
  ("f" compile-file "Compile the current file")
  ("k" kill-compilation "Kill compilation")
  ("o" switch-to-compilation-other-window "Switch to compilation in side window")
  ("l" compilation-set-skip-threshold "Cycle skip level(0, 1, 2) for errors navigation")
  ("d" dap-hydra "Use dap mode hydra as an interactive debugger")
  ("g" hydra-gdb/body "Use gdb-hydra as an interactive debugger")
  ("n" next-error "Go to next error")
  ("p" previous-error "Go to previous error")
  ("d" dap-hydra "Dap mode commands"))
(keymap-set ijkl-local-mode-map "ç" 'compile/body)

;;;; Hydra go
(defhydra go(:exit t :columns 3)
  "Jump to destination in text"
  ("l" goto-line "Go to line n°")
  ("b" bookmark-jump "Bookmark jump")
  ("u" backward-up-list "Go up one level" :color red)
  ("j" xref-find-definitions "Jump to definition (xref)")
  ("," org-roam-node-find "Go to an org roam file")
  ("n" forward-sexp  "Go to the closing parenthesis/bracket" :color red)
  ("J" xref-find-definitions-other-window  "Jump to definition other window(xref)")
  ("E" flycheck-previous-error "Previous error (Flycheck)" :color red)
  ("p" backward-sexp "Go to the opening parenthesis/bracket" :color red)
  ("r" jump-to-register "Jump to register (see point-to-register)")
  ("e" flycheck-next-error "Next error (Flycheck)" :color red)
  ("q" nil "Quit"))
(keymap-set ijkl-local-mode-map "g" 'go/body)

;;;; Rectangle
(defhydra rectangle(:exit t :columns 2)
  "Rectangle operations"
  ("t" string-rectangle "Edition")
  ("k" kill-rectangle "Cut")
  ("c" copy-rectangle-as-kill "Copy")
  ("C" clear-rectangle "Clear")
  ("y" yank-rectangle "Paste")
  ("o" open-rectangle "Insert whitespace")
  ("n" rectangle-number-lines "Number the lines"))
(keymap-set ijkl-local-mode-map "r" 'replace-char-or-rectangle-region)

;;;; Org ijkl
(with-eval-after-load "org"
  ;; Use ijkl in the date selection buffer
  (keymap-set org-read-date-minibuffer-local-map "m" "RET")
  (keymap-set org-read-date-minibuffer-local-map "i" "S-<up>")
  (keymap-set org-read-date-minibuffer-local-map "j" "S-<left>")
  (keymap-set org-read-date-minibuffer-local-map "k" "S-<down>")
  (keymap-set org-read-date-minibuffer-local-map "l" "S-<right>"))

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

(defhydra org(:exit t :columns 3)
  "Jump to destination in text"
  ("a" org-agenda-list "Org Agenda")
  ("i" org-roam-node-insert "Insert new org roam file")
  ("s" org-schedule "Set a scheduled date for the TODO item")
  ("d" org-deadline "Set a deadline for the TODO item")
  ("l" org-insert-link "Insert a link to another file/website")
  ("g" org-roam-node-find "Go to an org roam file")
  ("y" org-download-clipboard "Insert an image from the clipboard")
  ("," org-toggle-inline-images "Toggle image display")
  ("n" my/org-now-time-stamp "Insert today's date")
  ("N" org-time-stamp "Choose date from calendar")
  ("t" org-todo "Changes the TODO state of the current line")
  ("P" org-roam-pull-commit-push "Org roam sync")
  ("h" org-roam-buffer-toggle  "Org roam info for current file")
  ("q" nil "Quit"))
(keymap-set ijkl-local-mode-map "," 'org/body)

;;;; Hydra commands
(defhydra commands(:exit t :columns 1)
  "Execute commands"
  ("!" execute-extended-command "Emacs command")
  ("x" execute-extended-command)
  (":" eval-expression "Interpret lisp code")
  ("e" eval-last-sexp "Interpret last lisp expression"))
(keymap-set ijkl-local-mode-map "!" 'commands/body)

;;;; Magit hydra
(defhydra magit(:exit t :columns 1)
  "Magit commands"
  ("s" magit-status "Status (Home)")
  ("f" magit-file-dispatch "File commands")
  ("v" magit-dispatch "Global Commands"))
(keymap-set ijkl-local-mode-map "v" 'magit/body)

;;;; Magit ijkl
(with-eval-after-load "magit"
  (key-chord-define magit-log-select-mode-map "CC" 'magit-log-select-pick)
  (key-chord-define magit-log-select-mode-map "QQ" 'magit-log-select-quit)
  (dolist (keymap (list magit-diff-section-base-map magit-mode-map))
    (keymap-set keymap "&"  "C-x 1")
    (keymap-set keymap "é"  "C-x 2")
    (keymap-set keymap "\"" "C-x 3")
    (keymap-set keymap "'" 'other-window)
    (keymap-set keymap "4" 'other-window-reverse)
    (keymap-set-remap keymap "m" "RET" "C-c m")
    (keymap-set-remap keymap "j" "C-j" "C-c j")
    (keymap-set-remap keymap "i" "C-p" "C-c i")
    (keymap-set-remap keymap "l" "C-l" "C-c l")
    (keymap-set-remap keymap "k" "C-n" "C-c k")))
(with-eval-after-load "git-rebase"
  (keymap-set-remap git-rebase-mode-map "m" "RET" "C-c m")
  (keymap-set-remap git-rebase-mode-map "j" "C-l" "C-c j")
  (keymap-set-remap git-rebase-mode-map "i" "C-p" "C-c i")
  (keymap-set-remap git-rebase-mode-map "k" "C-n" "C-c k")
  (keymap-set-remap git-rebase-mode-map "l" "C-f" "C-c l")
  (keymap-set git-rebase-mode-map "d" 'git-rebase-kill-line))
(with-eval-after-load "with-editor"  ; Called for commits
  (diminish "with-editor-mode")
  (key-chord-define with-editor-mode-map "CC" 'with-editor-finish)
  (key-chord-define with-editor-mode-map "QQ" 'with-editor-cancel))
