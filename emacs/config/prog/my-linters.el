;;; flymake : Syntax highlighting, used by eglot/lsp
(use-package flymake
  :straight (:type built-in)
  :custom(flymake-mode-line-lighter ""))

(defun my/list-errors-file()
  "Wrapper for `consult-flymake' and `flycheck-list-errors' according to which is enabled"
  (interactive)
  (cond
   ((and (featurep 'flycheck) flycheck-mode) (call-interactively #'flycheck-list-errors))
   ((and (featurep 'flymake ) flymake-mode ) (call-interactively #'consult-flymake))
   (t (message "Neither flymake nor flycheck is enabled in current buffer"))))

(defun my/list-errors-project()
  "Wrapper for `consult-flymake' and `flycheck-list-errors' according to which is enabled"
  (interactive)
  (cond
   ((and (featurep 'flycheck) (fboundp #'lsp-treemacs-errors-list) flycheck-mode)
    (call-interactively #'lsp-treemacs-errors-list))
   ((and (featurep 'flymake ) flymake-mode)
    (call-interactively #'flymake-show-project-diagnostics))
   (t (message "Neither flymake nor flycheck is enabled in current buffer"))))
