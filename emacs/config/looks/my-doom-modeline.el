;;; Mode line theme : doom mode line - Must download CaskaydiaCove Nerd Font
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

