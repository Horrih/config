;;;;;;;;;;;;;;;;;;;;;;;  GESTIONNAIRE D'EXTENSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup optimization time to avoid garbage collection during init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()(setq gc-cons-threshold 800000)))

;; Add the main user repository of packages called MELPA
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

;; Install use-package if not installed yet
;; Use package will be used as a package loader in this file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t) ; Install the package if not available yet

;; Launch the esup command to measure startup time of each emacs plugin
(use-package esup
  :commands esup
  :config (setq esup-depth 0))

;;;;;;;;;;;;;;;;;;;;;;;;;    CUSTOMISATION       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (setq debug-on-error 't) ; Display the stacktrace if error encountered in one of the lisp method
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
  (setq compilation-always-kill t) ; Do not ask for confirmation when I stop current compilation
  (c-set-offset (quote cpp-macro) 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (setq make-backup-files nil) ; Do not use backup files (filename~)
  (setq create-lockfiles nil) ; Do not use lock files (.#filename)
  (if (file-directory-p "~/.org") ; Use this folder as org mode agenda files location if it exists
    (setq org-agenda-files '("~/.org"))))

;; Main color theme
(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))

;; Rename current file
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

;; Revert-buffer without confirmation prompt
(defun revert-buffer-no-confirm(&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

;; Refresh all buffers when all have been modified (e.g. git branch change from CLI)
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

;; My custom shortcuts
(progn
  (global-set-key (kbd "C-c c"  ) 'comment-or-uncomment-region) ; Comment all the lines of the selected area
  (global-set-key (kbd "M-g"    ) 'goto-line) ; New binding for going to the n-th line
  (global-set-key (kbd "M-s"    ) 'multi-occur-in-matching-buffers) ; Search in all buffers
  (global-set-key (kbd "C-j"    ) 'delete-backward-char) ; Delete like backspace
  (global-set-key (kbd "M-j"    ) 'backward-kill-word)   ; Delete like backspace + ctrl
  (global-set-key (kbd "M-p"    ) 'backward-paragraph) ; Previous paragraph
  (global-set-key (kbd "M-n"    ) 'forward-paragraph) ; Next paragraph
  (global-set-key (kbd "M-m"    ) 'exit-minibuffer) ; Enable alt+M to use enter. Useful when you forget to release alt when typing commands
  (global-set-key (kbd "C-c o"  ) 'ff-find-other-file) ; Switch between header and implementation
  (global-set-key (kbd "<f8>"   ) 'recompile) ; Recompile the project with the last compilation command
  (global-set-key (kbd "S-<f8>"    ) 'compile)   ; Compile the project and ask for compilation command
  (global-set-key (kbd "M-[ 3 4 ~" ) 'compile)   ; Compile the project ans ask for compilation command
  (global-set-key (kbd "C-<f8>" ) 'kill-compilation) ; Stop current compilation
  (global-set-key (kbd "<f2>"   ) 'rename-file-and-buffer) ; Rename the current file/buffer
  (global-set-key (kbd "<f5>"   ) 'revert-buffer-no-confirm) ; Refreshes the current file/buffer without confirmation
  (global-set-key (kbd "<f6>"   ) 'revert-all-file-buffers) ;; Refreshes all the current files/buffers

  ;; Resize the window when split using split screen (C-2 or C-3)
  (global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "M-S-<down>") 'enlarge-window)
  (global-set-key (kbd "M-S-<up>") 'shrink-window))

;;;;;;;;;;;;;;;;;;;;;;     PACKAGES GENERAUX     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git front end (amazing!)
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom-face (magit-filename ((t :foreground "white"))) ; Otherwise untracked files have the same color as title in git status
  :config
  (setq magit-no-confirm t)
  (setq magit-visit-ref-behavior '(checkout-any focus-on-ref)))

;; Displays command shortcuts when typing commands
(use-package which-key :config (which-key-mode))

;; User friendly search of commands/variables etc
;; We rebind some of emacs commands to use helm instead
(use-package helm
  :bind (("M-x"   . helm-M-x) ; Rebind traditional methods to helm methods
         ("C-x f" . helm-find-files)
         ("C-x b" . helm-mini))
  :defer 2 ; We always want helm but not to slow the initial startup
  :config (helm-mode)(message "helm-loaded"))

;;;;;;;;;;;;;;;;;;;;;;        DEV PACKAGES       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Search files or strings in the current project
(use-package projectile
  :bind (("C-c f" . projectile-find-file)
         ("C-c s" . projectile-ag)))

;; Used by projectile-ag
(use-package ag :defer t)

;; Displays the current project on the left as in an IDE
(use-package treemacs :bind ("C-c t" . treemacs))

;; Displays LSP errors using treemacs
(use-package lsp-treemacs :bind ("C-c e" . lsp-treemacs-errors-list))

;; Syntax highlighting, used by lsp
(use-package flycheck :defer t)

;; Dependency used by lsp to insert snippets. Used by some lsp commands
(use-package yasnippet :hook (lsp . yasnippet))

;; Support various web files, used by my custom modes : my-vue-mode & my-ts-mode
(use-package web-mode
  :mode ("\\.css\\'" "\\.html\\'")
  :config
  (setq web-mode-script-padding 0) ;; For vue.js SFC : no initial padding in the script section
  (setq web-mode-markup-indent-offset 2)) ;; For html : use an indent of size 2 (default is 4)

;; Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
  :hook (my-ts-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '("--semi" "false"
                           "--single-quote" "false"
                           "--tab-width" "4"
                           "--trailing-comma" "all"
                           "--print-width" "100")))

;; Custom mode for js/ts files, derived from web-mode  to enable LSP on these files
(define-derived-mode my-ts-mode web-mode "TypeScript(web)"
     "A major mode derived from web-mode, for editing .ts files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.ts\\'" . my-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . my-ts-mode))

;; Custom mode for .vue files, derived from web-mode, to enable LSP Vetur (VLS) options
(define-derived-mode my-vue-mode web-mode "Vue(web)"
     "A major mode derived from web-mode, for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . my-vue-mode))

;; Completion and syntax highlighting backend API, available for most languages
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

;; Enable JEDI as an LSP backend
(use-package lsp-jedi
  :defer t
  :hook (python-mode . (lambda()
                         (require 'lsp-jedi)
                         (with-eval-after-load "lsp-mode"
                           (add-to-list 'lsp-enabled-clients 'jedi)))))

;; Support gitlab-ci.yml
(use-package yaml-mode :mode "\\.yml\\'")

;; Hide/show sections of code : current function, class, or if/else section
(defun hide-show-mode-hook()
  (hs-minor-mode)
  (local-set-key (kbd "C-c h") 'hs-toggle-hiding) ; Hide or show the current area
  (local-set-key (kbd "C-c H") 'hs-show-all) ; Reveal all hidden areas
  (local-set-key (kbd "C-c M-h") 'hs-hide-all)) ; Hide all areas

;; Custom hook for python to enable various options
(add-hook 'python-mode-hook (lambda()
                              (hide-show-mode-hook)
                              (setq compile-command "python -m unittest")))
(add-hook 'term-mode-hook (lambda ()
                            (setq show-trailing-whitespace nil)))

;; Add include guards to the current file
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

;; Custom hook for c++ to enable various options
(add-hook 'c++-mode-hook (lambda()
                           (hide-show-mode-hook)
                           (setq compile-command "make -j4")))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Custom hook for lsp to enable various options
(add-hook 'emacs-lisp-mode-hook 'hide-show-mode-hook)

;; Custom hook for vue SFC to enable various options
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
