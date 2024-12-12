;;;; yasnippet : Snippets insertion
(use-package yasnippet
  :demand
  :config
  (yas-global-mode)
  (diminish 'yas-minor-mode))

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; aphaleia : Code formatters for various languages
;; Requires to enable apheleia-mode-maybe in the relevant modes
(use-package apheleia)

