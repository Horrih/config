;;; lsp-mode : Full-featured LSP client
(defun my/lsp-if-active-workspace()
  "Call `lsp-deferred' only if the file is in an active lsp-mode workspace"
  (when (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
    (require 'lsp-mode)
    (lsp-deferred)))

(use-package lsp-mode
  :hook
  (c++-ts-mode . my/lsp-if-active-workspace)
  (python-mode . my/lsp-if-active-workspace))
