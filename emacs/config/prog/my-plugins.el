;;;; yasnippet : Snippets insertion
(use-package yasnippet
  :demand
  :config
  (yas-global-mode)
  (diminish 'yas-minor-mode))

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
