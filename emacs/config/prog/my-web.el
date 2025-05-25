;;; React TSX major mode : tsx-ts-mode
(use-package tsx-ts-mode
  :straight (:type built-in)
  :if (treesit-language-available-p 'tsx)
  :mode "\\.tsx\\'")
