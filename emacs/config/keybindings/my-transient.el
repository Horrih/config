;;; Transient package : use the latest one and not the built-in not supported by magit
(use-package transient :demand)

;;; Transient for buffer switching commands
;;;; transient definition
(transient-define-prefix my/transient-buffer()
  "Transient for buffer switching"
  [["Commands"
    ("l" "List buffers" consult-buffer)
    ("p" "Switch between projects"   project-switch-project)
    ("k" "Kill current buffer"       kill-current-buffer)
    ("w" "Kill current window"       delete-window)
    ("r" "Rename current buffer"     my/rename-buffer)
    ("n" "New buffer"                my/new-buffer)
    ]
   ["Shortcuts"
   ("b" "Go to last buffer" my/switch-to-last-buffer)
   ("e" "Emacs config file" my/switch-to-emacs-config)
   ("s" "*scratch* buffer"  scratch-buffer)
   ("m" "*messages* buffer" my/switch-to-messages)
   ]])
(keymap-set ijkl-local-mode-map "b" 'my/transient-buffer)

;;;; my/rename-buffer
(defun my/rename-buffer()
  "Rename current buffer, and visited file if it exists."
  (interactive)
  (if buffer-file-name
      (call-interactively #'rename-visited-file)
    (call-interactively #'rename-buffer)))

;;;; my/new-buffer
(defun my/new-buffer(buffer-name)
  (interactive "sBuffer name: ")
  (switch-to-buffer buffer-name))

;;;; my/switch-to-messages
(defun my/switch-to-messages()
  "Switch to *Messages* buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;; my/switch-to-emacs-config
(defcustom my/emacs-config "~/.config/emacs/init.el"
  "Path to the emacs main config file. Used by `my/switch-to-emacs-config'
for some direct navigation bindings"
  :type 'string)

(defun my/switch-to-emacs-config()
  "Switch to *Messages* buffer"
  (interactive)
  (find-file my/emacs-config))

;;; Transient search text
;;;; Transient definition
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

;;;; my/consult-directory
(defun my/consult-directory()
  "`consult-grep' in `default-directory'"
  (interactive)
  (consult-grep default-directory))

;;;; my/consult-grep
(defun my/consult-grep()
  "`consult-grep' with current symbol as start string"
  (interactive)
  (funcall #'consult-grep nil (format "%s" (symbol-at-point))))


;;; Transient register
(defun my/dir-to-register(register)
  "Saves the current directory in a register. Opening it will use dired"
  (interactive (list (register-read-with-preview "Directory to register : ")))
  (set-register register (cons 'file default-directory)))

;;;; Transient definition
(transient-define-prefix my/transient-register() "Transient for all register operations"
  [["Register - Save"
   ("p" "Save point" point-to-register)
   ("w" "Save window configuration" window-configuration-to-register)
   ("d" "Save directory" my/dir-to-register)
   ("R" "Load register" jump-to-register)
   ("x" "Clear register" my/clear-register)
   ]])
(keymap-set ijkl-local-mode-map "R" 'my/transient-register)

;;;; Clear register
(defun my/clear-register(char)
  "Clear register currently referred by `CHAR'"
  (interactive "cClear register at : ")
  (set-register char nil))

;;; Transient for find commands
;;;; Transient definition
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
    ("e" "List errors (file)"     my/list-errors-file)
    ("E" "List errors (project)"  flymake-show-project-diagnostics :if (lambda() (featurep 'flymake)))
    ("r" "Find references (xref)" xref-find-references)
    ("o" "Switch header/cpp"      my/cpp-switch-header)
    ("t" "Switch source/tests"    my/switch-tests)
    ]])
(keymap-set ijkl-local-mode-map "f" 'my/transient-find)

;;;; my/project-find-file-other-window()
(defun my/project-find-file-other-window()
  "Like `project-find-file' but opens the file in another window"
  (interactive)
  (cl-letf (((symbol-function 'find-file) 'find-file-other-window))
    (project-find-file)))


;;; Hydra hide/show
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


;;; Hydra go
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

;;; Rectangle
;;;; my/replace-char-or-rectangle-region
(defun my/replace-char-or-rectangle-region()
  "If mark is active, rectangle actions, otherwise replace-char"
  (interactive)
  (call-interactively
    (if mark-active
        'my/hydra-rectangle/body
      'my/replace-char-at-point)))

(keymap-set ijkl-local-mode-map "r" 'my/replace-char-or-rectangle-region)

;;;; Hydra
(defhydra my/hydra-rectangle(:exit t :columns 2)
  "Rectangle operations"
  ("t" string-rectangle "Edition")
  ("k" kill-rectangle "Cut")
  ("c" copy-rectangle-as-kill "Copy")
  ("C" clear-rectangle "Clear")
  ("y" yank-rectangle "Paste")
  ("o" open-rectangle "Insert whitespace")
  ("n" rectangle-number-lines "Number the lines"))


;;; Execute/eval commands
(transient-define-prefix my/transient-commands()
  "Execute commands"
  [["Elisp"
    (":" "Interpret lisp code" eval-expression)
    ("e""Interpret last lisp expression" eval-last-sexp)
    ("E" "Interpret the current buffer as lisp" eval-buffer)
    ("b" "Interpret the whole lisp buffer" eval-buffer)
    ]
   ["bash"
    ("s" "Shell Command" async-shell-command)
    ("t" "Open a terminal" my/term-new)
    ]
   ["Misc"
    ("!" "Emacs commands" execute-extended-command)
   ]])
(keymap-set ijkl-local-mode-map "!" 'my/transient-commands)

;;; Narrow Hydra
(defhydra my/hydra-narrow(:exit t :columns 1)
  "Narrow the displayed buffer to a subsection"
  ("N" narrow-to-region "Narrow to selected region")
  ("d" narrow-to-defun "Narrow to function")
  ("f" narrow-to-defun)
  ("w" widen "Widen (cancel narrowing)"))
(keymap-set ijkl-local-mode-map "N" 'my/hydra-narrow/body)
