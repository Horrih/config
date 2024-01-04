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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;; use-package : Customize the package macro for this file
(use-package use-package
  :custom
  (use-package-always-ensure t) ; Download missing packages by default
  (use-package-always-defer t)) ; Lazy load by default, use :demand otherwise

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
  :demand
  :custom-face
  (mode-line ((t :background "black")))
  (mode-line-inactive ((t :background "#333333"))) ; Dark grey
  :custom
  ;; Font with icons to download here
  ;; https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/CascadiaCode.zip
  (nerd-icons-font-family "CaskaydiaCove NFM")
  (doom-modeline-unicode-fallback t)
  (doom-modeline-minor-modes t)
  :init
  (doom-modeline-mode))

;;;; Dired as default buffer
(when (< (length command-line-args) 2)
  (add-hook 'after-init-hook 'dired-jump))

;;;; Misc
(progn
  (tool-bar-mode 0) ; Disable the toolbar in GUI mode
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

  ;; Automatic pairing for parentheses/braces
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)
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
  (customize-set-variable 'c-basic-offset  4) ; Base indent size when indented automatically
  (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
  (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
  (customize-set-variable 'recenter-positions '(top middle bottom)) ; Start recenter on top instead of middle
  (customize-set-variable 'make-backup-files nil) ; Do not use backup files (filename~)
  (customize-set-variable 'create-lockfiles nil)) ; Do not use lock files (.#filename)

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

;;;; my/match-buffer-extension
(defun my/match-buffer-extension(&rest extensions)
  "Returns t if the current buffer has an extension in EXTENSIONS"
  (if (member (file-name-extension (buffer-name)) extensions)
      t))

;;;; my/other-window-reverse
(defun my/other-window-reverse()
  "Like `other-window' but in the reverse order"
  (interactive)
  (other-window -1))

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

;;; Compilation options
;;;; Compilation misc
(use-package compile
  :ensure nil ; Emacs built in
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
This can be useful in conjunction to projectile's .dir-locals variables"
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
  :ensure nil ; Emacs built-in
  :hook (compilation-filter . my/ansi-color-compilation-filter-except-ag)) ; Handle terminal colors in the compilation buffer

;;;; Error regexps : Set compilation regex for errors
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
  :bind (:map my/keys-mode-map
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


;;; Org mode : Note taking and presentation
;;;; Org mode : package customizations
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
  :ensure nil
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
  (web-mode . (lambda () (when (my/match-buffer-extension "ts" "js" "vue")
                           (lsp-deferred)
                           (setq-local lsp-auto-format t))))
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
  :ensure nil ; emacs built-in
  :hook
  (emacs-lisp-mode . outline-minor-mode)
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

;;;; my/include-guards(text) : Add include guards to the current file
(defun my/include-guards(text)
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

;;;; my/include-c-header() : Inserts a #include directive for C/C++
(defun my/include-c-header(val)
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
;;;; yasnippet : Snippets for various languages
(use-package yasnippet
  :demand
  :config
  (yas-global-mode)
  (diminish 'yas-minor-mode))

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

;;;; my/inhibit-lsp-mode : When turned on, lsp won't trigger unless manually calling 'lsp
(define-minor-mode my/inhibit-lsp-mode
  "Minor mode to temporarily inhibit the `lsp-deferred' command"
  :lighter " NoLSP"
  :global t
  (if my/inhibit-lsp-mode
      (advice-add 'lsp-deferred :override #'ignore) ;; lsp-deferred will be inhibited
    (advice-remove 'lsp-deferred #'ignore)))

;;;; my/lsp-format-and-save : format on save if my/lsp-auto-format is not nil
(defcustom my/lsp-auto-format nil
  "If not nil, lsp-format-and-save will format the buffer before saving"
   :type 'boolean)

(defun my/lsp-format-and-save()
  "Saves the current buffer and formats it if lsp-format-on-save is not nil"
  (interactive)
  (when (and (not buffer-read-only) my/lsp-auto-format)
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
(key-chord-define my/keys-mode-map "sd" 'ijkl-local-mode-and-save)
(key-chord-define my/keys-mode-map "qs" 'ijkl-local-mode)
(keymap-set my/keys-mode-map "C-q" 'ijkl-local-mode) ; Fallback if key-chord fails
(keymap-set my/keys-mode-map "M-q" 'quoted-insert) ; Fallback if key-chord fails
(keymap-set my/keys-mode-map "M-c" ijkl-local-mode-map) ; Make all bindings accessible with M-c
(diminish 'ijkl-local-mode)

;;;; ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode on traditional buffers"
    (unless (or (minibufferp)
                (string-match "[Gg]it" (format "%s" major-mode))
                (string-match "[Gg]it" (format "%s" major-mode))
                (string-equal (buffer-name) "*Org Note*")
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
(defun my/gen-input(KEYS)
  "Generates a key `KEYS' sequence as if the user typed it"
  (setq unread-command-events (nconc (listify-key-sequence (kbd KEYS)) unread-command-events)))

;;;; my/mode-is-one-of-p helper function
(defun my/mode-is-one-of-p(modes)
  "Returns t if the current modes (minor or major) matches one in the input modes list"
  (let (res)
    (dolist (input-mode modes res)
      (dolist (mode (cons major-mode minor-mode-list))
        (when (string-equal input-mode mode)
          (setq res t))))))

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
(key-alias  ijkl-local-mode-map "&"   "C-x 1")
(keymap-set ijkl-local-mode-map "é" "C-x 2")
(key-alias  ijkl-local-mode-map "\"" "C-x 3")
(keymap-set ijkl-local-mode-map "'" 'other-window)
(keymap-set ijkl-local-mode-map "4" 'my/other-window-reverse)
(keymap-set ijkl-local-mode-map "w" 'my/lsp-format-and-save)
(keymap-set ijkl-local-mode-map "z" 'recenter-top-bottom)
(keymap-set ijkl-local-mode-map "r" ctl-x-r-map)
(key-alias  ijkl-local-mode-map "c" "M-w")
(key-alias  ijkl-local-mode-map "y" "C-y")
(key-alias  ijkl-local-mode-map "_" "C-_")
(keymap-set ijkl-local-mode-map "p" 'asmr-backward) ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "n" 'asmr-forward)  ; Reimplementation of a mark ring
(key-alias  ijkl-local-mode-map "<SPC>" "C-@")
(keymap-set ijkl-local-mode-map "I" 'er/expand-region)   ; Expand the selection progressively
(keymap-set ijkl-local-mode-map "K" 'er/contract-region) ; Reduce the selection progressively

;;;; movement and deletion bindings (accessible in both modes)
;;;;; backwards
(key-alias ijkl-local-mode-map "j"   "C-j")
(key-alias    my/keys-mode-map "C-j" "C-b")
(key-alias    my/keys-mode-map "M-j" "M-b")
(key-alias    my/keys-mode-map "C-M-j" "C-a")
(key-alias ijkl-local-mode-map "a" "C-a")

;;;;; forwards
(key-alias ijkl-local-mode-map "l"   "C-l")
(key-alias    my/keys-mode-map "C-l" "C-f")
(key-alias    my/keys-mode-map "M-l" "M-f")
(key-alias    my/keys-mode-map "C-M-l" "C-e")
(key-alias ijkl-local-mode-map "e" "C-e")

;;;;; upwards
(key-alias ijkl-local-mode-map "i" "C-p")
(key-alias    my/keys-mode-map "C-k" "C-n")
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
(key-alias ijkl-local-mode-map "k" "C-n")
(keymap-set   my/keys-mode-map "M-k" (lambda() (interactive)(next-line 7)))
(key-alias    my/keys-mode-map "C-M-k" "M->")
(key-alias ijkl-local-mode-map ">" "M->")
(key-alias ijkl-local-mode-map "E" "C-M-e")

;;;;; deletion
(key-alias  ijkl-local-mode-map "u" "C-M-u" '("dired-mode" "Info-mode"))
(keymap-set    my/keys-mode-map "C-u" 'delete-backward-char)
(keymap-set    my/keys-mode-map "C-M-u" 'my/delete-start-or-previous-line)
(keymap-set    my/keys-mode-map "M-u" 'backward-kill-word)
(keymap-set    my/keys-mode-map "C-o" 'delete-forward-char)
(keymap-set    my/keys-mode-map "C-M-o" 'kill-line)
(key-alias  ijkl-local-mode-map "o" "C-M-o")
(keymap-set    my/keys-mode-map "M-o" 'kill-word)

;;;; Misc
(keymap-set ijkl-local-mode-map "/"     'comment-or-uncomment-region) ; Comment all the lines of the selected area
(keymap-set ijkl-local-mode-map "M-s"   'multi-occur-in-matching-buffers) ; Search in all buffers
(keymap-set ijkl-local-mode-map "<f2>"  'rename-visited-file) ; Rename the current file/buffer
(keymap-set ijkl-local-mode-map "<f5>"  'revert-buffer-quick) ; Refreshes the current file/buffer without confirmation
(keymap-set ijkl-local-mode-map "<f12>" 'my/include-c-header) ; Shortcuts for a #include directive

;;;; Resize the window when split using split screen (C-2 or C-3)
(keymap-set ijkl-local-mode-map "M-S-<right>" 'enlarge-window-horizontally)
(keymap-set ijkl-local-mode-map "M-S-<left>" 'shrink-window-horizontally)
(keymap-set ijkl-local-mode-map "M-S-<down>" 'enlarge-window)
(keymap-set ijkl-local-mode-map "M-S-<up>" 'shrink-window)

;;;; Hydra buffer
;;;;; hydra
(defhydra buffer(:exit t :hint nil)
  "
^Commands^                        ^Shortcuts^
--------------------------------------------
_l_: List buffers                 _e_: Emacs config file
_L_: List buffers other window    _s_: *scratch* buffer
_b_: Go to last buffer            _m_: *messages* buffer
_k_: Kill current buffer
"
  ("l" consult-buffer)              ("e" my/switch-to-emacs-config)
  ("L" consult-buffer-other-window) ("s" scratch-buffer)
  ("b" my/switch-to-last-buffer)    ("m" my/switch-to-messages)
  ("k" kill-current-buffer))
(keymap-set ijkl-local-mode-map "b" 'buffer/body)

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
;;;;; Hydra
(defhydra search(:exit t :hint nil)
  "
^Incremental search^         ^Occurences^                   ^Replace^
------------------------------------------------------------------------
_s_: Forward                 _o_: In file                   _r_: String
_S_: Backward                _b_: In all buffers            _R_: Regexp
_w_: Symbol at point         _p_: In current project        _P_: Project
^ ^                          _a_: In current directory
"
  ("s" isearch-forward)                 ("o" consult-line)                    ("r" query-replace)
  ("S" isearch-backward)                ("b" multi-occur-in-matching-buffers) ("R" query-replace-regexp)
  ("w" isearch-forward-symbol-at-point) ("p" projectile-ag)                   ("P" projectile-replace)
                                        ("a" my/consult-directory))
(keymap-set ijkl-local-mode-map "s" 'search/body)

;;;;; my/consult-directory
(defun my/consult-directory()
  "`consult-grep' in `default-directory'"
  (interactive)
  (consult-grep default-directory))

;;;; Hydra find
(defhydra find(:exit t :hint nil)
  "
^Find files^                      ^Coding^
-------------------------------------------------------------
_d_: Dired - Current directory    _e_: List errors (file)
_f_: By path                      _t_: List errors (project)
_p_: Project                      _r_: Find references (xref)
_P_: Project in other window      _o_: Switch header/cpp
"
  ("d" dired-jump)                        ("e" flycheck-list-errors)
  ("f" find-file)                         ("t" lsp-treemacs-errors-list)
  ("p" project-find-file)                 ("r" xref-find-references)
  ("P" projectile-find-file-other-window) ("o" ff-find-other-file))
(keymap-set ijkl-local-mode-map "f" 'find/body)

;;;; Hydra compile
(defhydra compile(:exit t :hint nil)
  "
^Start compilation^             ^Buffer commands^
--------------------------------------------------------------
_e_: Edit command               _o_: Open *compilation* buffer
_ç_: Recompile                  _n_: Next compilation error
_a_: Compile project            _p_: Prev compilation error
_f_: Compile current file       _l_: Cycle error threshold
_k_: Stop compilation
"
  ("e" compile)             ("o" my/switch-to-compilation-other-window)
  ("ç" my/recompile-switch) ("l" compilation-set-skip-threshold :color red)
  ("a" my/compile-all)      ("n" next-error     :color red)
  ("f" my/compile-file)     ("p" previous-error :color red)
  ("k" kill-compilation))
(keymap-set ijkl-local-mode-map "ç" 'compile/body)

;;;; Hydra go
(defhydra go(:exit t :hint nil)
  "
^Go to^                                 ^LSP Navigation^                  ^Expressions^
-------------------------------------------------------------------------------------------
_l_: Line n°                            _j_: Definition                   _n_: Forward expression
_b_: Bookmark                           _J_: Definition other window      _p_: Backward expression
_,_: Org roam file                      _e_: Next error                   _u_: Upward expression
_r_: Register (see point-to-register)   _E_: Previous error

"
  ("l" goto-line)          ("j" xref-find-definitions)                ("n" forward-sexp     :color red)
  ("b" bookmark-jump)      ("J" xref-find-definitions-other-window)   ("p" backward-sexp    :color red)
  ("," org-roam-node-find) ("e" flycheck-next-error :color red)       ("u" backward-up-list :color red)
  ("r" jump-to-register)   ("E" flycheck-previous-error :color red)
  ("q" nil "Quit"))
(keymap-set ijkl-local-mode-map "g" 'go/body)

;;;; Rectangle
;;;;; my/replace-char-or-rectangle-region
(defun my/replace-char-or-rectangle-region()
  "If mark is active, rectangle actions, otherwise replace-char"
  (interactive)
  (call-interactively
    (if mark-active
        'rectangle/body
      'my/replace-char-at-point)))

(keymap-set ijkl-local-mode-map "r" 'my/replace-char-or-rectangle-region)

;;;;; Hydra
(defhydra rectangle(:exit t :columns 2)
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

(defhydra org(:exit t :hint nil)
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
