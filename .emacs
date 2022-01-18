;; * Enable lexical binding -*- lexical-binding: t -*-
;;Lexical binding enables using variables defined with let in lambda functions called later
;; * Package management
;; ** Optimize garbage collection : improves the startup time
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()(setq gc-cons-threshold 800000)))

;; ** Enable MELPA : Add the main user repository of packages
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

;; ** use-package : Use package will be used as a package loader in this file
;; Install use-package if not installed yet
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t) ; Install the package if not available yet

;; ** esup : Launch the esup command to measure startup time of each emacs plugin
(use-package esup
  :commands esup
  :config (setq esup-depth 0))

;; * Various customizations options
;; ** Main color theme : vscode-dark-plus-theme
(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))

;; ** Misc
(progn
  (setq ring-bell-function 'ignore)
  (setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
  (column-number-mode 't) ; Display column numbers in the status line
  (global-display-line-numbers-mode 't) ; Display line numbers on the left
  (line-number-mode 't) ; Display line number
  (size-indication-mode 't) ; Display size indication
  (delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
  (show-paren-mode 1) ; Highlight the matching parenthesis
  (setq-default show-trailing-whitespace t) ; Show in red the spaces forgotten at the end of lines
  (setq-default indent-tabs-mode nil) ; Use spaces for indent
  (setq default-tab-width 4) ; Number of spaces inserted by tab
  (setq c-basic-offset    4) ; Base indent size when indented automatically
  (menu-bar-mode -1) ; Hide Menu bar
  (fset 'yes-or-no-p 'y-or-n-p) ; Abreviate Yes/No
  (c-set-offset (quote cpp-macro) 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (setq make-backup-files nil) ; Do not use backup files (filename~)
  (setq create-lockfiles nil) ; Do not use lock files (.#filename)
  (if (file-directory-p "~/.org") ; Use this folder as org mode agenda files location if it exists
      (setq org-agenda-files '("~/.org"))))

;; ** rename-file-and-buffer(name)
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; ** revert-buffer-no-confirm()
(defun revert-buffer-no-confirm(&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

;; ** revert-all-file-buffers
;;Refresh all buffers when all have been modified (e.g. git branch change from CLI)
(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; ** switch-to-last-buffer
(defun switch-to-last-buffer()
  "Use `switch-to-buffer' to visit the last buffer"
  (interactive)
  (switch-to-buffer nil))

;; ** delete-start-or-previous-line
(defun delete-start-or-previous-line()
  "Use `kill-line' to delete either the start of the line, or the previous line if empty"
  (interactive)
  (kill-line (if (= (line-beginning-position) (point)) -1 0)))

;; * Compilation options
;; ** Compilation misc
(setq compilation-always-kill t) ; Do not ask for confirmation when I stop current compilation
(setq compile-command "make -j8") ; Default compilation command

;; ** switch-to-compilation-other-window()
(defun switch-to-compilation-other-window()
  "Switches to the compilation buffer in another window"
  (interactive)
  (switch-to-buffer-other-window "*compilation*"))

;; ** ansi-color : Handle terminal colors in the compilation buffer
(use-package ansi-color
  :config
  (defun colorize-compile()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . colorize-compile))

;; ** regexps : Set compilation regex for errors
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

;; * General usage packages
;; ** magit : Git front end (amazing!)
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom-face (magit-filename ((t :foreground "white"))) ; Otherwise untracked files have the same color as title in git status
  :config
  (setq magit-no-confirm t)
  (setq magit-visit-ref-behavior '(checkout-any focus-on-ref)))

;; ** which-key : Displays command shortcuts when typing commands
(use-package which-key :config (which-key-mode))

;; ** helm : User friendly search of commands/variables etc
;; We rebind some of emacs commands to use helm instead
(use-package helm
  :bind (("M-x"   . helm-M-x) ; Rebind traditional methods to helm methods
         ("C-x f" . helm-find-files)
         ("C-x b" . helm-mini))
  :demand
  :config
  (helm-mode)

  ; We switch tab and ctrl-z actions to be more "natural"
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map (kbd "C-j") nil)
  (setq helm-buffer-max-length 40))

;; * Development packages and options
;; ** Projectile : Search files or strings in the current project
(use-package projectile
  :bind (("C-c f" . projectile-find-file)
         ("C-c s" . projectile-ag)))

;; ** ag : Front end for the CLI utility ag, Used by projectile-ag
(use-package ag
  :defer t
  :config (setq ag-highlight-search t))

;; ** treemacs : Displays the current project on the left as in an IDE
(use-package treemacs :bind ("C-c t" . treemacs))

;; ** web-mode : Support various web files, used by my custom modes : my-vue-mode & my-ts-mode
(use-package web-mode
  :mode ("\\.css\\'" "\\.html\\'")
  :config
  (setq web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
  (setq web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

;; ** prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
  :hook (my-ts-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '("--semi" "false"
                           "--single-quote" "false"
                           "--tab-width" "4"
                           "--trailing-comma" "all"
                           "--print-width" "100")))

;; ** my typescript mode : Custom mode for js/ts files, derived from web-mode  to enable LSP on these files
(define-derived-mode my-ts-mode web-mode "TypeScript(web)"
     "A major mode derived from web-mode, for editing .ts files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.ts\\'" . my-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . my-ts-mode))

;; ** my vue mode : Custom mode for .vue files, derived from web-mode, to enable LSP Vetur (VLS) options
(define-derived-mode my-vue-mode web-mode "Vue(web)"
     "A major mode derived from web-mode, for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . my-vue-mode))

;; ** Outline mode with package outline-minor-faces
 (use-package outline-minor-faces
     :after outline
     :config (add-hook 'outline-minor-mode-hook
                       'outline-minor-faces-add-font-lock-keywords))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (make-local-variable 'outline-regexp)
            (setq outline-regexp "^;; \\*+")
            (make-local-variable 'outline-heading-end-regexp)
            (setq outline-heading-end-regexp "\n")
            (outline-minor-mode 1)))

;; ** yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode :mode "\\.yml\\'")

;; ** hide-show-mode : Hide/show sections of code : current function, class, or if/else section
(defun hide-show-mode-hook()
  (hs-minor-mode)
  (local-set-key (kbd "C-c h") 'hs-toggle-hiding) ; Hide or show the current area
  (local-set-key (kbd "C-c H") 'hs-show-all) ; Reveal all hidden areas
  (local-set-key (kbd "C-c M-h") 'hs-hide-all)) ; Hide all areas

;; Custom hook to enable hide-show-mode on all prog files
(add-hook 'prog-mode-hook 'hide-show-mode-hook)

;; ** include-guards(text) : Add include guards to the current file
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

;; ** include-c-header-val() : Inserts a #include directive for C/C++
(defun include-c-header(val)
  "Adds a #include \"VAL.h\" at point and saves the file"
  (interactive "MHeader file name: ")
  (insert (format "#include \"%s.h\"\n" val))
  (save-buffer))

;; ** enable c++ mode for headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; * LSP : completion and linting
;; ** lsp-treemacs : Displays LSP errors using treemacs
;; ** lsp-treemacs : treemacs style views for various lsp results
(use-package lsp-treemacs :bind ("C-c e" . lsp-treemacs-errors-list))

;; ** company : Completion frontend, used by lsp
(use-package company :defer t)

;; ** flycheck : Syntax highlighting, used by lsp
(use-package flycheck :defer t)

;; ** yasnippet : Dependency used by lsp to insert snippets. Used by some lsp commands
(use-package yasnippet :hook (lsp . yasnippet))

;; ** lsp-mode : Completion and syntax highlighting backend API, available for most languages
(use-package lsp-mode
  :hook
  (
   (python-mode . lsp-deferred)
   (c++-mode    . lsp-deferred)
   (my-vue-mode . lsp-deferred)
   (my-ts-mode  . lsp-deferred)
   (lsp-mode    . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-links nil)
  (yas-minor-mode)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  (require 'flycheck)
  (with-eval-after-load "lsp-mode"
    (when (string-equal major-mode "python-mode")
      (message "Adding flake8")
      (flycheck-add-next-checker 'lsp 'python-flake8)))
  :bind (("C-c j" . lsp-find-definition) ; Jump to a fonction definition
         ("C-c J" . lsp-find-references) ; Find references to this function
         ("C-h l" . lsp-describe-thing-at-point))) ; Display the documentation for this function

;; ** lsp-jedi : Enable JEDI as an LSP backend for python
(use-package lsp-jedi
  :defer t
  :hook (python-mode . (lambda()
                         (require 'lsp-jedi)
                         (with-eval-after-load "lsp-mode"
                           (add-to-list 'lsp-enabled-clients 'jedi)))))

;; ** my-vue-hook : Custom hook for vue SFC to enable various options
(add-hook 'my-vue-mode-hook (lambda()
                              (local-set-key (kbd "C-x C-s") (lambda() ; Call format buffer on save
                                                               (interactive "*")
                                                               (lsp-format-buffer)
                                                               (save-buffer)))
                              (setq lsp-vetur-format-default-formatter-html "js-beautify-html")
                              (setq lsp-vetur-format-default-formatter-options
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
                              ))


;; * Reimplementation of a mark ring
;; ** Define the global variables used
(defvar global-mark-previous ()
  "List containing previous mark positions, combining the ideas of `mark-ring'  and `global-mark-ring'.
This mark-ring will record all mark positions globally, multiple times per buffer")

(defvar global-mark-next ()
  "List containing next mark positions, used to revert the effects of `global-mark-previous'")

(defvar bidirectional-mark-ring-max 40
  "Maximum size of `global-mark-previous'.  Start discarding off end if gets this big.")

;; ** Override pushmark
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

;; ** mark manipulation utilities
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

;; ** bakckward-mark() : main back function
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

;; ** forward-mark() : main next function
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

;; * Modal edition mode using ijkl for movement
;; ** key-alias()
;;(format "Redirects function call from '%s' to '%s' (initially `%s')" from to (key-binding to))

(defun key-alias2(keymap from to &optional inside-keymap)
  "Binds the key-binding FROM to the function called by typing TO.

If inside-keymap is not nil, the TO binding will be set to nil inside the KEYMAP, so that
the call to TO will be an alias to the default keymaps"
  (unless inside-keymap
    (define-key keymap to nil))
  (define-key keymap from
    (lambda (&optional args)
      "Test"
      (interactive "P")
      (call-interactively (key-binding to)))))

;; ** ijkl minor mode definition
(define-minor-mode ijkl-local-mode
  "Minor mode to be able to move using ijkl"
  :lighter " ijkl"
  :init-value nil
  :keymap '(([t] . ignore)   ; The actual keymaps are defined later below
            ("z" . ijkl-local-mode))
  (add-to-list 'emulation-mode-map-alists '(ijkl-local-mode . ijkl-local-mode-map))
  )

;; ** ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode on traditional buffers"
    (unless (or (minibufferp)
                (string-match "[Gg]it" (format "%s" major-mode))
                (string-equal (buffer-name) "COMMIT_EDITMSG"))
      (ijkl-local-mode))))
(ijkl-mode)

;; ** Change color of the mode line according to the mode (command, edit, unsaved)
(let ((default-color (face-background 'mode-line)))
  (add-hook 'post-command-hook
            (lambda ()
              (set-face-background 'mode-line
                                   (cond ((not ijkl-local-mode) "red")
                                         (t default-color))))))

;; * Main keybindings
;; ** Helper macro key-alias
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

;; ** ijkl mode bindings
(define-key ijkl-local-mode-map (kbd "h") help-map) ; Use the help functions
(define-key ijkl-local-mode-map (kbd "C-g") nil) ; Do not override C-g binding
(define-key ijkl-local-mode-map (kbd "C-x") nil) ; Do not override C-x binding
(define-key ijkl-local-mode-map (kbd "C-c") nil) ; Do not override C-x binding
(define-key ijkl-local-mode-map (kbd "M-x") nil) ; Do not override M-x binding
(define-key ijkl-local-mode-map (kbd "TAB") nil) ; Do not override tab binding
(define-key ijkl-local-mode-map (kbd "C-z") nil) ; Do not override C-z binding
(define-key ijkl-local-mode-map (kbd "C-s") nil) ; Do not override C-s binding
(define-key ijkl-local-mode-map (kbd "C-r") nil) ; Do not override C-r binding
(key-alias  ijkl-local-mode-map (kbd "m"  ) (kbd "C-m"))
(key-alias  ijkl-local-mode-map (kbd "M-m") (kbd "M-<RET>"))
(key-alias  ijkl-local-mode-map (kbd "&"  ) (kbd "C-x 1"))
(define-key ijkl-local-mode-map (kbd "é"  ) (kbd "C-x 2"))
(key-alias  ijkl-local-mode-map (kbd "\"" ) (kbd "C-x 3"))
(key-alias  ijkl-local-mode-map (kbd "'"  ) (kbd "C-x o"))
(key-alias  ijkl-local-mode-map (kbd "w"  ) (kbd "C-x C-s"))
(key-alias  ijkl-local-mode-map (kbd "b"  ) (kbd "C-x b"))
(define-key ijkl-local-mode-map (kbd "r"  ) 'recenter-top-bottom)
(key-alias  ijkl-local-mode-map (kbd "c"  ) (kbd "M-w"))
(key-alias  ijkl-local-mode-map (kbd "y"  ) (kbd "C-y"))
(key-alias  ijkl-local-mode-map (kbd "_"  ) (kbd "C-_"))
(key-alias  ijkl-local-mode-map (kbd "j"  ) (kbd "C-b"))
(key-alias  ijkl-local-mode-map (kbd "C-j") (kbd "M-b"))
(key-alias  ijkl-local-mode-map (kbd "M-j") (kbd "C-a"))
(key-alias  ijkl-local-mode-map (kbd "l"  ) (kbd "C-f"))
(key-alias  ijkl-local-mode-map (kbd "C-l") (kbd "M-f"))
(key-alias  ijkl-local-mode-map (kbd "M-l") (kbd "C-e"))
(key-alias  ijkl-local-mode-map (kbd "i") (kbd "C-p"))
(define-key ijkl-local-mode-map (kbd "M-i") (lambda() (interactive)(previous-line 7)))
(key-alias  ijkl-local-mode-map (kbd "C-M-i") (kbd "M-<"))
(key-alias  ijkl-local-mode-map (kbd "k") (kbd "C-n"))
(define-key ijkl-local-mode-map (kbd "M-k") (lambda() (interactive)(next-line 7)))
(key-alias  ijkl-local-mode-map (kbd "C-M-k") (kbd "M->"))
(define-key ijkl-local-mode-map (kbd "u") 'delete-backward-char)
(define-key ijkl-local-mode-map (kbd "C-u") 'backward-kill-word)
(define-key ijkl-local-mode-map (kbd "M-u") 'delete-start-or-previous-line)
(define-key ijkl-local-mode-map (kbd "o") 'delete-forward-char)
(define-key ijkl-local-mode-map (kbd "C-o") 'kill-word)
(define-key ijkl-local-mode-map (kbd "M-o") 'kill-line)
(define-key ijkl-local-mode-map (kbd "g") 'goto-line)
(key-alias  ijkl-local-mode-map (kbd "<SPC>") (kbd "C-@"))

;; ** Misc
(define-key ijkl-local-mode-map (kbd "G"   ) 'magit-status)
(define-key ijkl-local-mode-map (kbd "/"   ) 'comment-or-uncomment-region) ; Comment all the lines of the selected area
(define-key ijkl-local-mode-map (kbd "M-s" ) 'multi-occur-in-matching-buffers) ; Search in all buffers
(define-key ijkl-local-mode-map (kbd "<f8>"      ) 'recompile) ; Recompile the project with the last compilation command
(define-key ijkl-local-mode-map (kbd "S-<f8>"    ) 'compile)   ; Compile the project and ask for compilation command
(define-key ijkl-local-mode-map (kbd "M-[ 3 4 ~" ) 'compile)   ; Compile the project ans ask for compilation command
(define-key ijkl-local-mode-map (kbd "C-S-<f8>"  ) 'switch-to-compilation-other-window) ; Switches to the compilation buffer
(define-key ijkl-local-mode-map (kbd "C-<f8>" ) 'kill-compilation) ; Stop current compilation
(define-key ijkl-local-mode-map (kbd "<f2>"   ) 'rename-file-and-buffer) ; Rename the current file/buffer
(define-key ijkl-local-mode-map (kbd "<f5>"   ) 'revert-buffer-no-confirm) ; Refreshes the current file/buffer without confirmation
(define-key ijkl-local-mode-map (kbd "<f6>"   ) 'revert-all-file-buffers) ;; Refreshes all the current files/buffers
(define-key ijkl-local-mode-map (kbd "<f12>"  ) 'include-c-header) ;; Shortcuts for a #include directive
(define-key ijkl-local-mode-map (kbd "a"      ) 'backward-mark) ;; Reimplementation of a mark ring
(define-key ijkl-local-mode-map (kbd "e"      ) 'forward-mark)  ;; Reimplementation of a mark ring

;; ** Resize the window when split using split screen (C-2 or C-3)
(define-key ijkl-local-mode-map (kbd "M-S-<right>") 'enlarge-window-horizontally)
(define-key ijkl-local-mode-map (kbd "M-S-<left>") 'shrink-window-horizontally)
(define-key ijkl-local-mode-map (kbd "M-S-<down>") 'enlarge-window)
(define-key ijkl-local-mode-map (kbd "M-S-<up>") 'shrink-window)

;; ** Key chords
(use-package key-chord :config (key-chord-mode))
(key-chord-define-global "zz" 'ijkl-local-mode)
(key-chord-define ijkl-local-mode-map "cc" 'kill-region)
(key-chord-define ijkl-local-mode-map "bb" 'switch-to-last-buffer)

;; ** Hydra outline
(defhydra outline(ijkl-local-mode-map "à")
  "outline"
  ("t" outline-hide-body "tree")
  ("u" outline-up-heading "up")
  ("n" outline-next-visible-heading "next")
  ("p" outline-previous-visible-heading "prev")
  ("a" outline-show-all "show all")
  ("s" outline-show-subtree "show all subtree")
  ("TAB" outline-show-children "show direct children")
  ("q" outline-hide-sublevels "hide-all")
  ("h" outline-hide-subtree "hide subtree"))

;; ** Hydra hide/show
(defhydra hydra-hide-show (:exit t)
  "Hydra for hide-show commands"
  ("t" hs-toggle-hiding "Toggle H/S")
  ("a" hs-show-all "Show all")
  ("q" hs-hide-all "Hide all")
  ("l" hs-hide-level "Hide Level")
  ("s" hs-show-block "Show block")
  ("h" hs-hide-block "Hide block"))
(key-chord-define ijkl-local-mode-map "hh" 'hydra-hide-show/body)

;; ** Hydra find
(defhydra find(ijkl-local-mode-map "f" :exit t)
  "find"
  ("d" helm-find-files "helm-find-files")
  ("j" lsp-find-definition "LSP jump to def")
  ("l" lsp-describe-thing-at-point "LSP help")
  ("j" lsp-find-definition "lsp jump to def")
  ("s" projectile-ag "ag")
  ("o" ff-find-other-file "switch header/cpp")
  ("f" projectile-find-file "projectile-find-file"))
