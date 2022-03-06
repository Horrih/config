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
;; Install use-package if not installed yet
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t) ; Install the package if not available yet

;;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish)

;;;; esup : Launch the esup command to measure startup time of each emacs plugin
(use-package esup
  :commands esup
  :config (setq esup-depth 0))

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
  :config (load-theme 'vscode-dark-plus t))

;;;; Mode line theme : doom mode line
(use-package doom-modeline
  :custom-face
  (mode-line ((t :background "black")))
  (mode-line-inactive ((t :background "#333333"))) ; Dark grey
  :custom
  (doom-modeline-unicode-fallback t)
  :init
  (doom-modeline-mode))

;;;; All the icons
(use-package all-the-icons
  :if (display-graphic-p))

;;;; Welcome dashboard
(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-items '((projects . 5)
                            (bookmarks . 10)
                            (recents  . 10)
                            ))
    (dashboard-setup-startup-hook))
(add-hook 'dashboard-mode-hook (lambda()(setq show-trailing-whitespace nil)))

;;;; Misc
(progn
  (tool-bar-mode 0) ; Disable the toolbar in GUI mode
  (scroll-bar-mode 0) ; Disable the scroll bar in GUI mode
  (setq inhibit-startup-screen 't) ; Hide the startup screen
  (savehist-mode) ; Save history for commands
  (setq isearch-resume-in-command-history 't) ; Use history for isearch as well
  (global-auto-revert-mode) ; Refresh files automatically when modified from outside emacs
  (setq enable-local-eval 't) ; Enable eval blocks in .dir-locals.el
  (setq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
  (setq ring-bell-function 'ignore) ; Disable the bell for emacs
  (setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
  (column-number-mode 't) ; Display column numbers in the status line
  (global-display-line-numbers-mode 't) ; Display line numbers on the left
  (line-number-mode 't) ; Display line number
  (size-indication-mode 't) ; Display size indication
  (delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
  (show-paren-mode 1) ; Highlight the matching parenthesis
  (setq-default show-trailing-whitespace t) ; Show in red the spaces forgotten at the end of lines
  (setq-default indent-tabs-mode nil) ; Use spaces for indent
  (menu-bar-mode -1) ; Hide Menu bar
  (fset 'yes-or-no-p 'y-or-n-p) ; Abreviate Yes/No
  (setq default-tab-width 4) ; Number of spaces inserted by tab
  (setq-default c-basic-offset  4) ; Base indent size when indented automatically
  (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
  (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
  (setq make-backup-files nil) ; Do not use backup files (filename~)
  (setq create-lockfiles nil)) ; Do not use lock files (.#filename)

;;;; rename-file-and-buffer(name)
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (unless (buffer-file-name)
    (error "Buffer '%s' is not visiting a file!" name))
  (when (get-buffer new-name)
    (error "A buffer named '%s' already exists!" new-name))
  (rename-file (buffer-file-name) new-name 't)
  (rename-buffer new-name)
  (set-visited-file-name new-name)
  (set-buffer-modified-p nil))

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
  "Returns 't if the current buffer has an extension in EXTENSIONS"
  (if (member (file-name-extension (buffer-name)) extensions)
      't))

;;; Compilation options
;;;; Compilation misc
(setq compilation-always-kill t) ; Do not ask for confirmation when I stop current compilation
(add-hook 'compilation-mode-hook (lambda()(setq show-trailing-whitespace nil)))

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

;;;; ansi-color : Handle terminal colors in the compilation buffer
(use-package ansi-color
  :config
  (defun colorize-compile()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compile))

;;;; regexps : Set compilation regex for errors
(let ((enabled-regexps ())
      (custom-error-list '(
        ;; Insert your custom regexps here
        (jest-error "^.*\(\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\).*$" 1 2 3)
        (gcc-error "^[ ]*\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*error:\\|  required from here\\).*$" 1 2 3)
        (gcc-warning "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): warning:.*$" 1 2 3 1)
        (gcc-info "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): note:.*$" 1 2 3 0)
        (qt-test "^   Loc: \\[\\(.*\\)\(\\([0-9]+\\)\)\\]$" 1 2)
      )))
  (dolist (err custom-error-list)
    (add-to-list 'enabled-regexps (car err)))
  (custom-set-variables `(compilation-error-regexp-alist ',enabled-regexps))
  (add-hook 'compilation-mode-hook (lambda()
    (dolist (err custom-error-list)
      (add-to-list 'compilation-error-regexp-alist-alist err)))))

;;; General usage packages
;;;; magit : Git front end (amazing!)
(use-package magit
  :defer t
  :custom-face (magit-filename ((t :foreground "white"))) ; Otherwise untracked files have the same color as title in git status
  :config
  (setq magit-no-confirm t)
  (setq magit-visit-ref-behavior '(checkout-any focus-on-ref)))

;;;; which-key : Displays command shortcuts when typing commands
(use-package which-key
  :config (which-key-mode)
  :diminish)

;;;; key-chord  : Enables combination of keys like zz
(use-package key-chord
  :custom (key-chord-two-keys-delay 0.03)
  :config (key-chord-mode))

;;;; helm : User friendly search of commands/variables etc
;; We rebind some of emacs commands to use helm instead
(use-package helm
  :diminish
  :demand t
  :bind (:map my-keys-mode-map
         ("M-x" . helm-M-x) ; Rebind traditional methods to helm methods
         :map help-map
         ("a" . helm-apropos)
         :map ctl-x-map
         ("f" . helm-find-files)
         ("b" . helm-mini)
         ;; We switch tab and ctrl-z actions to be more "natural"
         :map helm-map
         ("TAB"   . #'helm-execute-persistent-action)
         ("C-z"   . #'helm-select-action))
  :config
  (helm-mode)
  :custom
  (helm-buffer-max-length 40))

;;;; Helpful : nice looking and more complete help buffers
(use-package helpful
  :bind (:map help-map
              ("p" . helpful-at-point)
              ("s" . helpful-symbol)
              ("v" . helpful-variable)
              ("f" . helpful-callable)
              ("k" . helpful-key)))

;;;; Org mode : Base mode for note taking
(use-package org
  :config (require 'org-tempo) ; Useful for using easy templates like <s TAB to insert a source block
  :hook (org-mode . (lambda()
                      (auto-fill-mode)))) ; Wrap lines when longer than fill column

;;;; Org bullets : Pretty mode for org
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;;; org-roam : Notes organizing
(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-return-follows-link t)
  (org-roam-directory "~/.org_roam")
  (org-roam-completion-everywhere t))

;;; Development packages and options
;;;; Projectile : Search files or strings in the current project
(use-package projectile :defer t)

;;;; ag : Front end for the CLI utility ag, Used by projectile-ag
(use-package ag
  :defer t
  :config (setq ag-highlight-search t))


;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; treemacs : Displays the current project on the left as in an IDE
(use-package treemacs
  :defer t
  :config (custom-set-variables '(treemacs-no-delete-other-windows nil)))

;;;; web-mode : Support various web files, used by my custom modes : my-vue-mode & my-ts-mode
(use-package web-mode
  :mode
  ("\\.css\\'" "\\.html\\'" "\\.ts'" "\\.js'" "\\.vue\\'")
  :hook
  (web-mode . (lambda () (setq-local lsp-auto-format (match-buffer-extension "ts" "js" "vue"))))
  :custom
  (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
  (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

;;;; prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
  :defer t
  :custom
  (prettier-js-show-errors nil)
  (prettier-js-args '("--semi" "false"
                      "--single-quote" "false"
                      "--tab-width" "4"
                      "--trailing-comma" "all"
                      "--print-width" "150")))

;;;; Outline mode with package outline-minor-faces and outshine
;;;;; Enable sane bindings and actions for outline mode
(use-package outshine
  :defer t
  :diminish
  :config (define-key outshine-mode-map (kbd "C-M-i") nil))
(add-hook 'outline-minor-mode-hook (lambda()(diminish 'outline-minor-mode)))

;;;;; Pretty colors for outline-mode
(use-package outline-minor-faces
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(diminish 'eldoc-mode)

;;;; yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode :mode "\\.yml\\'")

;;;; hide-show-mode : Hide/show sections of code : current function, class, or if/else section
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'hs-minor-mode-hook (lambda() (diminish 'hs-minor-mode)))

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

;;;; enable c++ mode for headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;; Disable Abbrev in the mode line
(diminish 'abbrev-mode)

;;;; C++ mode line customization
;; Ignore the customization of the mode line C++//l for example becomes C++
(advice-add 'c-update-modeline :override #'ignore)

;;; LSP : completion and linting
;;;; lsp-treemacs : treemacs style views for various lsp results
(use-package lsp-treemacs :defer t)

;;;; company : Completion frontend, used by lsp
(use-package company
  :defer t
  :diminish)

;;;; flycheck : Syntax highlighting, used by lsp
(use-package flycheck :defer t)

;;;; yasnippet : Dependency used by lsp to insert snippets. Used by some lsp commands like completion
(use-package yasnippet
  :hook (lsp-mode . (lambda()
                      (yas-minor-mode)
                      (diminish 'yas-minor-mode))))

;;;; lsp-mode : Completion and syntax highlighting backend API, available for most languages
(use-package lsp-mode
  :hook
  (
   (c++-mode    . lsp-deferred)
   (my-vue-mode . lsp-deferred)
   (my-ts-mode  . lsp-deferred)
   (python-mode . lsp-deferred)
   (lsp-mode    . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (("C-h l" . lsp-describe-thing-at-point))
  :custom
  ;; Formatting options for vue.js (.vue files)
  (lsp-vetur-format-default-formatter-html "js-beautify-html")
  (lsp-vetur-format-default-formatter-options
   '((js-beautify-html
      (wrap_attributes . "preserve")
      (indent_size . 2)
      (wrap_attributes_indent_size . 2))
     (prettier
      (singleQuote . :json-false)
      (printWidth . 100)
      (tabWidth . 4)
      (trailingComma . "all")
      (vueIndentScriptAndStyle . :json-false)
      (semi . :json-false))))
  (lsp-enable-links nil) ; Make links non clickable

  :config
  (setq lsp-headerline-arrow ">") ; Material design icon not working on windows
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable) ; Enable flycheck instead of the builtin flymake

  ;; Custom checkers and client for python
  (when (derived-mode-p 'python-mode)
    (require 'lsp-jedi) ; Using jedi instead of pyls
    (flycheck-add-next-checker 'lsp 'python-flake8)))

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

;;;; lsp-jedi : An LSP backend for python
(use-package lsp-jedi :defer t)

;;; Reimplementation of a mark ring
;;;; Define the global variables used
(defvar global-mark-previous ()
  "List containing previous mark positions, combining the ideas of `mark-ring'  and `global-mark-ring'.
This mark-ring will record all mark positions globally, multiple times per buffer")

(defvar global-mark-next ()
  "List containing next mark positions, used to revert the effects of `global-mark-previous'")

(defvar bidirectional-mark-ring-max 40
  "Maximum size of `global-mark-previous'.  Start discarding off end if gets this big.")

;;;; Override pushmark
(defun bidirectional-push-mark-advice(&optional location nomsg activate)
  (interactive)
  (when (mark t)
    (let ((old (nth bidirectional-mark-ring-max global-mark-previous))
          (history-delete-duplicates nil))
      (add-to-history 'global-mark-previous (copy-marker (mark-marker)) bidirectional-mark-ring-max)
      (setq global-mark-next ()) ; Reset the global mark next when the user performs other actions
      (when old
        (set-marker old nil)))))

(advice-add 'push-mark :after #'bidirectional-push-mark-advice)

;;;; mark manipulation utilities
(defun marker-is-point-p (marker)
  "Tests if MARKER is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun jump-to-marker(marker)
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

;;;; bakckward-mark() : main back function
(defun backward-mark()
  "Records the current position at mark and jump to previous mark"
  (interactive)
  (let* ((target (car global-mark-previous))
         (current target))
    (cond ((not current) (setq target nil))
          ((marker-is-point-p current) (setq target (car (cdr global-mark-previous))))
          (t (push-mark)))
    (if (not target)
        (user-error "No previous mark position")
      (push (copy-marker (mark-marker)) global-mark-next)
      (pop global-mark-previous)
      (jump-to-marker (car global-mark-previous)))))

;;;; forward-mark() : main next function
(defun forward-mark()
  "Records the current position at mark and jump to previous mark"
  (interactive)
  (let* ((target (car global-mark-next))
         (prev (car global-mark-previous)))
    (if (not target)
        (user-error "No next mark position")
      (unless (and prev (marker-is-point-p prev))
        (push-mark))
      (push (copy-marker target) global-mark-previous)
      (pop global-mark-next)
      (jump-to-marker target))))

;;; Modal edition mode using ijkl for movement
;;;; ijkl minor mode definition
(define-minor-mode ijkl-local-mode
  "Minor mode to be able to move using ijkl"
  :lighter " ijkl"
  :keymap '(([t] . ignore))   ; The actual keymaps are defined later below
  (add-to-list 'emulation-mode-map-alists '(ijkl-local-mode . ijkl-local-mode-map))
  )
(key-chord-define ijkl-local-mode-map "sd" 'ijkl-local-mode)

(defun ijkl-local-mode-and-save()
  "Enables ijkl-local-mode and saves the current file if applicable"
  (interactive)
  (ijkl-local-mode)
  (when (and (buffer-modified-p) buffer-file-name)
    (save-buffer)))
(key-chord-define my-keys-mode-map "sd" 'ijkl-local-mode-and-save)
(key-chord-define my-keys-mode-map "qs" 'ijkl-local-mode)
(diminish 'ijkl-local-mode)

;;;; ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode on traditional buffers"
    (unless (or (minibufferp)
                (string-match "[Gg]it" (format "%s" major-mode))
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
;;;; Helper macro key-alias
(defmacro key-alias(keymap from to &optional inside-keymap)
  "Binds the key-binding FROM to the function called by typing TO.

If inside-keymap is not nil, the TO binding will be set to nil inside the KEYMAP, so that
the call to TO will be an alias to the default keymaps"
  `(progn
     (unless ,inside-keymap
       (define-key ,keymap ,to nil))
     (defun ,(intern (format "%s-alias/%s/%s" keymap (eval from) (eval to)))(&optional args)
       ,(format "Forwards the interactive call from %s to %s (bound by default to `%s')" from to (key-binding (eval to)))
       (interactive "P")
       (call-interactively (key-binding ,(eval to))))
       (define-key ,keymap ,from ',(intern (format "%s-alias/%s/%s" keymap (eval from) (eval to))))
    ))

;;;; utility bindings
(define-key ijkl-local-mode-map (kbd "h") help-map) ; Use the help functions
(define-key ijkl-local-mode-map (kbd "C-g") nil) ; Do not override C-g binding
(define-key ijkl-local-mode-map (kbd "C-x") nil) ; Do not override C-x binding
(define-key ijkl-local-mode-map (kbd "C-c") nil) ; Do not override C-x binding
(define-key ijkl-local-mode-map (kbd "M-x") nil) ; Do not override M-x binding
(define-key ijkl-local-mode-map (kbd "TAB") nil) ; Do not override tab binding
(define-key ijkl-local-mode-map (kbd "<tab>") nil) ; Do not override tab binding
(define-key ijkl-local-mode-map (kbd "<backtab>") nil) ; Do not override tab binding
(define-key ijkl-local-mode-map (kbd "C-z") nil) ; Do not override C-z binding
(define-key ijkl-local-mode-map (kbd "C-s") nil) ; Do not override C-s binding
(define-key ijkl-local-mode-map (kbd "C-r") nil) ; Do not override C-r binding
(define-key ijkl-local-mode-map (kbd "x") ctl-x-map) ; Bind x to the ctl-x commands
(define-key ctl-x-map (kbd "e") 'eval-last-sexp) ; Evaluate the lisp expression
(key-chord-define ijkl-local-mode-map "xx" 'helm-M-x) ; Bind xx to M-x
(key-alias  ijkl-local-mode-map (kbd "!"  ) (kbd "M-!")) ; Launch shell commands with !
(key-alias  ijkl-local-mode-map (kbd "m"  ) (kbd "C-m"))
(key-alias  ijkl-local-mode-map (kbd "M-m") (kbd "M-<RET>"))
(key-alias  ijkl-local-mode-map (kbd "&"  ) (kbd "C-x 1"))
(define-key ijkl-local-mode-map (kbd "é"  ) (kbd "C-x 2"))
(key-alias  ijkl-local-mode-map (kbd "\"" ) (kbd "C-x 3"))
(key-alias  ijkl-local-mode-map (kbd "'"  ) (kbd "C-x o"))
(define-key ijkl-local-mode-map (kbd "w"  ) 'lsp-format-and-save)
(key-alias  ijkl-local-mode-map (kbd "b"  ) (kbd "C-x b"))
(define-key ijkl-local-mode-map (kbd "r"  ) 'recenter-top-bottom)
(key-alias  ijkl-local-mode-map (kbd "c"  ) (kbd "M-w"))
(key-chord-define ijkl-local-mode-map "cc" 'kill-region)
(key-alias  ijkl-local-mode-map (kbd "y"  ) (kbd "C-y"))
(key-chord-define ijkl-local-mode-map "yy" 'helm-show-kill-ring)
(key-alias  ijkl-local-mode-map (kbd "_"  ) (kbd "C-_"))
(define-key ijkl-local-mode-map (kbd "p"      ) 'backward-mark) ;; Reimplementation of a mark ring
(define-key ijkl-local-mode-map (kbd "n"      ) 'forward-mark)  ;; Reimplementation of a mark ring
(key-alias  ijkl-local-mode-map (kbd "<SPC>") (kbd "C-@"))

;;;; movement and deletion bindings (accessible in both modes)
;;;;; backwards
(key-alias  ijkl-local-mode-map (kbd "j"  ) (kbd "C-j"))
(key-alias     my-keys-mode-map (kbd "C-j") (kbd "C-b"))
(key-alias     my-keys-mode-map (kbd "M-j") (kbd "M-b"))
(key-alias     my-keys-mode-map (kbd "C-M-j") (kbd "C-a"))
(key-alias  ijkl-local-mode-map (kbd "a") (kbd "C-a"))
(define-key ijkl-local-mode-map (kbd "C-j") nil)
(define-key ijkl-local-mode-map (kbd "M-j") nil)
(define-key ijkl-local-mode-map (kbd "C-M-j") nil)

;;;;; forwards
(key-alias     my-keys-mode-map (kbd "C-l") (kbd "C-f"))
(key-alias  ijkl-local-mode-map (kbd "l"  ) (kbd "C-l"))
(key-alias     my-keys-mode-map (kbd "M-l") (kbd "M-f"))
(key-alias     my-keys-mode-map (kbd "C-M-l") (kbd "C-e"))
(key-alias  ijkl-local-mode-map (kbd "e") (kbd "C-e"))
(define-key ijkl-local-mode-map (kbd "C-l") nil)
(define-key ijkl-local-mode-map (kbd "M-l") nil)
(define-key ijkl-local-mode-map (kbd "C-M-l") nil)

;;;;; upwards
(key-alias  ijkl-local-mode-map (kbd "i") (kbd "C-p"))
(define-key    my-keys-mode-map (kbd "M-i") (lambda() (interactive)(previous-line 7)))
(key-alias     my-keys-mode-map (kbd "C-M-i") (kbd "M-<") t)
(key-chord-define ijkl-local-mode-map "aa" 'beginning-of-buffer)
(define-key ijkl-local-mode-map (kbd "M-i") nil)
(define-key ijkl-local-mode-map (kbd "C-M-i") nil)

;;;;; downwards
(key-alias  ijkl-local-mode-map (kbd "k") (kbd "C-n"))
(define-key    my-keys-mode-map (kbd "M-k") (lambda() (interactive)(next-line 7)))
(key-alias     my-keys-mode-map (kbd "C-M-k") (kbd "M->") t)
(key-chord-define ijkl-local-mode-map "ee" 'end-of-buffer)
(define-key ijkl-local-mode-map (kbd "M-k") nil)
(define-key ijkl-local-mode-map (kbd "C-M-k") nil)

;;;;; deletion
(key-alias  ijkl-local-mode-map (kbd "u"  ) (kbd "C-M-u"))
(define-key    my-keys-mode-map (kbd "C-u") 'delete-backward-char)
(define-key ijkl-local-mode-map (kbd "C-u") nil)
(define-key    my-keys-mode-map (kbd "C-M-u") 'delete-start-or-previous-line)
(define-key    my-keys-mode-map (kbd "M-u") 'backward-kill-word)
(define-key ijkl-local-mode-map (kbd "M-u") nil)
(define-key    my-keys-mode-map (kbd "C-o") 'delete-forward-char)
(define-key ijkl-local-mode-map (kbd "C-o") nil)
(define-key    my-keys-mode-map (kbd "C-M-o") 'kill-line)
(key-alias  ijkl-local-mode-map (kbd "o") (kbd "C-M-o"))
(define-key    my-keys-mode-map (kbd "M-o") 'kill-word)
(define-key ijkl-local-mode-map (kbd "M-o") nil)

;;;; Misc
(key-chord-define ijkl-local-mode-map "bb" 'switch-to-last-buffer)
(define-key ijkl-local-mode-map (kbd "/"   ) 'comment-or-uncomment-region) ; Comment all the lines of the selected area
(define-key ijkl-local-mode-map (kbd "M-s" ) 'multi-occur-in-matching-buffers) ; Search in all buffers
(define-key ijkl-local-mode-map (kbd "<f2>"   ) 'rename-file-and-buffer) ; Rename the current file/buffer
(define-key ijkl-local-mode-map (kbd "<f5>"   ) 'revert-buffer-no-confirm) ; Refreshes the current file/buffer without confirmation
(define-key ijkl-local-mode-map (kbd "<f6>"   ) 'revert-all-file-buffers) ;; Refreshes all the current files/buffers
(define-key ijkl-local-mode-map (kbd "<f12>"  ) 'include-c-header) ;; Shortcuts for a #include directive

;;;; Resize the window when split using split screen (C-2 or C-3)
(define-key ijkl-local-mode-map (kbd "M-S-<right>") 'enlarge-window-horizontally)
(define-key ijkl-local-mode-map (kbd "M-S-<left>") 'shrink-window-horizontally)
(define-key ijkl-local-mode-map (kbd "M-S-<down>") 'enlarge-window)
(define-key ijkl-local-mode-map (kbd "M-S-<up>") 'shrink-window)

;;;; Hydra outline
(defhydra outline(:columns 3)
  "outline"
  ("u" outline-up-heading "up")
  ("TAB" outline-toggle-children "toggle hide/show children")
  ("a" outline-show-all "show all")
  ("n" outline-next-visible-heading "next")
  ("q" outline-hide-sublevels "hide-all")
  ("s" outline-show-subtree "show all subtree")
  ("p" outline-previous-visible-heading "prev")
  ("h" outline-hide-subtree "hide subtree"))
(define-key ijkl-local-mode-map "à" 'outline/body)

;;;; Hydra hide/show
(defhydra hydra-hide-show (:exit t :columns 2)
  "Hydra for hide-show commands"
  ("t" hs-toggle-hiding "Toggle H/S")
  ("l" hs-hide-level "Hide Level")
  ("q" hs-hide-all "Hide all")
  ("s" hs-show-block "Show block")
  ("a" hs-show-all "Show all")
  ("h" hs-hide-block "Hide block"))
(key-chord-define ijkl-local-mode-map "hh" 'hydra-hide-show/body)

;;;; Hydra search text
(defhydra search(:exit t :columns 2)
  "Text search related commands"
  ("o" helm-occur "Occurences in file")
  ("s" isearch-forward "Next occurence in file")
  ("r" query-replace "Next occurence in file")
  ("a" helm-do-grep-ag "Ag in current directory")
  ("p" projectile-ag "Ag in current project")
  ("b" multi-occur-in-matching-buffers "Occur in all buffers"))
(define-key ijkl-local-mode-map "s" 'search/body)

;;;; Hydra find
(defhydra find(:exit t :columns 2)
  "Search related commands"
  ("d" dired-jump "Open current directory in dired")
  ("f" helm-find-files "helm-find-files")
  ("e" lsp-treemacs-errors-list "LSP errors")
  ("r" lsp-find-references "LSP find references")
  ("o" ff-find-other-file "switch header/cpp")
  ("p" projectile-find-file "projectile-find-file"))
(define-key ijkl-local-mode-map "f" 'find/body)

;;;; Hydra compile
(defhydra compile(:exit t :columns 2)
  "Compilation commands"
  ("ç" recompile-switch "Reuse last compilation command")
  ("e" compile "Edit the compilation command")
  ("a" compile-all "Compile STC")
  ("f" compile-file "Compile the current file")
  ("k" kill-compilation "Kill compilation")
  ("o" switch-to-compilation-other-window "Switch to compilation in side window"))
(define-key ijkl-local-mode-map "ç" 'compile/body)

;;;; Hydra go
(defhydra go(:exit t :columns 1)
  "Jump to destination in text"
  ("b" bookmark-jump "Bookmark jump")
  ("j" lsp-find-definition "LSP jump to definition")
  ("g" goto-line "Go to line n°")
  ("p" forward-sexp  "Go to the closing parenthesis/bracket")
  ("n" backward-sexp "Go to the opening parenthesis/bracket"))
(define-key ijkl-local-mode-map "g" 'go/body)

;;;; Hydra org-roam
(defhydra org(:exit t :columns 2)
  "Jump to destination in text"
  ("i" org-roam-node-insert "Insert new org roam file")
  ("g" org-roam-node-find "Go to an org roam file")
  ("t" org-time-stamp "Insert current timestamp")
  ("d" org-todo "Changes the TODO state of the current line")
  ("h" org-roam-buffer-toggle  "Org roam info for current file"))
(define-key ijkl-local-mode-map "," 'org/body)

;;;; Magit hydra
(defhydra magit(:exit t :columns 1)
  "Magit commands"
  ("s" magit-status "Status (Home)")
  ("f" magit-file-dispatch "File commands")
  ("v" magit-dispatch "Global Commands"))
(define-key ijkl-local-mode-map "v" 'magit/body)

;;;; Magit ijkl
(with-eval-after-load "magit"
  (key-chord-define magit-log-select-mode-map "cc" 'magit-log-select-pick)
  (key-chord-define magit-log-select-mode-map "qq" 'magit-log-select-quit)
  (dolist (keymap (list magit-diff-section-base-map magit-mode-map))
    (key-chord-define keymap "bb" 'switch-to-last-buffer)
    (define-key keymap "x" ctl-x-map)
    (define-key keymap "h" help-map)
    (define-key keymap "v" 'magit-dispatch)
    (key-alias keymap "m" (kbd "RET") 't)
    (key-alias keymap "b" (kbd "C-x b"))
    (key-alias keymap "j" (kbd "C-j"))
    (key-alias keymap "l" (kbd "C-l"))
    (key-alias keymap "i" (kbd "C-p"))
    (key-alias keymap "k" (kbd "C-n"))))
(with-eval-after-load "git-rebase"
  (key-alias git-rebase-mode-map "k" (kbd "C-n"))
  (key-alias git-rebase-mode-map "l" (kbd "C-f"))
  (define-key git-rebase-mode-map "d" 'git-rebase-kill-line))
(with-eval-after-load "with-editor"  ; Called for commits
  (key-chord-define with-editor-mode-map "cc" 'with-editor-finish)
  (key-chord-define with-editor-mode-map "qq" 'with-editor-cancel))
