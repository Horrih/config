;; Buffer manipulation commands
;;; transient definition
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

;;; my/rename-buffer
(defun my/rename-buffer()
  "Rename current buffer, and visited file if it exists."
  (interactive)
  (if buffer-file-name
      (call-interactively #'rename-visited-file)
    (call-interactively #'rename-buffer)))

;;; my/new-buffer
(defun my/new-buffer(buffer-name)
  (interactive "sBuffer name: ")
  (switch-to-buffer buffer-name))

;;; my/switch-to-messages
(defun my/switch-to-messages()
  "Switch to *Messages* buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;; my/switch-to-emacs-config
(defcustom my/emacs-config "~/.config/emacs/init.el"
  "Path to the emacs main config file. Used by `my/switch-to-emacs-config'
for some direct navigation bindings"
  :type 'string)

(defun my/switch-to-emacs-config()
  "Switch to *Messages* buffer"
  (interactive)
  (find-file my/emacs-config))

;;; Narrow Hydra
(defhydra my/hydra-narrow(:exit t :columns 1)
  "Narrow the displayed buffer to a subsection"
  ("N" narrow-to-region "Narrow to selected region")
  ("d" narrow-to-defun "Narrow to function")
  ("f" narrow-to-defun)
  ("w" widen "Widen (cancel narrowing)"))

;;; hide/show
;;;; hide-show-mode : Hide/show sections of code : current function, class, or if/else section
(use-package hideshow
  :straight (:type built-in)
  :config
  (diminish 'hs-minor-mode)
  :hook
  (prog-mode . hs-minor-mode))

;;;; Keybindings
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

