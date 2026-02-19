;;; React TSX major mode : tsx-ts-mode
(use-package tsx-ts-mode
  :straight (:type built-in)
  :if (treesit-language-available-p 'tsx)
  :mode "\\.tsx\\'")

;;; web-mode : Support various web files
(use-package web-mode
  :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.vue\\'")
  :custom
  (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
  (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)
