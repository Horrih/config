;;; Various front-end customizations
(tool-bar-mode 0) ; Disable the toolbar in GUI mode
(setopt cursor-type 'bar)
(setopt scroll-bar-mode nil)
(setopt horizontal-scroll-bar-mode nil)
(menu-bar-mode -1) ; Hide Menu bar
(setopt use-short-answers t) ; Abreviate Yes/No to y or n
(setopt ring-bell-function 'ignore) ; Disable the bell for emacs
(setopt inhibit-startup-screen t) ; Hide the startup screen
(column-number-mode t) ; Display column numbers in the status line
(global-display-line-numbers-mode -1) ; If true, display line numbers on the left
(line-number-mode t) ; Display line number
(size-indication-mode t) ; Display size indication
(delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
(show-paren-mode 1) ; Highlight the matching parenthesis
(setopt completions-detailed t) ; Detailed description for the built in describe symbol etc

;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish
  :demand
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode))

(use-package simple
  :straight (:type built-in)
  :config
  (diminish 'auto-fill-function))
