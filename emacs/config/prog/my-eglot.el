;;; eglot : Built-in package for completion with LSP. Light-weight alternative to lsp-mode
(use-package eglot
  :straight (:type built-in)
  :hook (eglot-managed-mode . (lambda()
                                (eglot-inlay-hints-mode -1) ; Disable inlay hint for params by default
                                (custom-set-variables
                                 '(help-at-pt-timer-delay 0.5)
                                 '(help-at-pt-display-when-idle '(flymake-diagnostic)))))
  :bind (:map help-map ("h" . eldoc))
  :custom
  (eglot-report-progress nil)
  :custom-face
  (eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow :underline t)))))
