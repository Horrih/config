;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;; aphaleia : Code formatters for various languages
;; Requires to enable apheleia-mode-maybe in the relevant modes
(use-package apheleia)

