;;; Compilation misc
(use-package compile
  :straight (:type built-in)
  :hook (compilation-mode . (lambda()(setq show-trailing-whitespace nil)))
  :custom
  (compilation-always-kill t)) ; Do not ask for confirmation when I stop current compilation

;;; Hydra compile
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

;;; my/switch-to-compilation-other-window()
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

;;; my/recompile-switch
(defun my/recompile-switch()
  "Uses the recompile function and switches to the buffer end"
  (interactive)
  (recompile)
  (my/switch-to-compilation-other-window-end))

;;; my/compile-all
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

;;; my/compile-file
(defun my/compile-file(file-name)
  "Compiles the file FILE-NAME using a command to be define `compile-file-command'
  This function should take a filename as parameter and returning the command as output"
  (interactive (list (buffer-file-name)))
  (unless (fboundp 'compile-file-command)
    (error "compile-file expects the compile-file-command function to be defined"))
  (compile (compile-file-command file-name))
  (my/switch-to-compilation-other-window-end))

;;; ansi-color : Translate TTY escape sequences into colors
(defun my/ansi-color-compilation-filter-except-ag()
  "Like `ansi-color-compilation-filter', except on buffers generated by the ag package.
   If we use vanilla ansi-color-compilation-filter, the colors get messed up"
  (unless (string-match "ag search text" (buffer-name))
    (ansi-color-compilation-filter)))

(use-package ansi-color
  :straight (:type built-in)
  :hook (compilation-filter . my/ansi-color-compilation-filter-except-ag)) ; Handle terminal colors in the compilation buffer

;;; Error regexps : Set compilation regex for errors
(let ((enabled-regexps ())
      (custom-error-list '(
        ;; Insert your custom regexps here
        (file-path-col "^\\([0-9a-zA-Z_/.]*\\..*\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3 0)
        (file-path "^\\([0-9a-zA-Z_/.]*\\..*\\):\\([0-9]+\\):.*$" 1 2 nil 0)
        (doctest "^\\(.*\\):\\([0-9]+\\): FATAL ERROR" 1 2 nil)
        (cppcheck "^\\(.*\\..*\\):\\([0-9]+\\):\\([0-9]+\\):.*\\[.*\\]$" 1 2 3 1)
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
