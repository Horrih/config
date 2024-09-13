;;; yaml-ts-mode : Major mode for yaml files
(use-package yaml-ts-mode
  :if (treesit-language-available-p 'yaml)
  :hook (yaml-ts-mode . (lambda()
                          ;; Tab indentation is traditionnally 2 for yaml
                          (setq-local tab-width 2)
                          ;; Remap string face so that it is displayed as plain text
                          (face-remap-add-relative 'font-lock-string-face :inherit 'default)))
  :mode ("\\.ya?ml\\'"))
