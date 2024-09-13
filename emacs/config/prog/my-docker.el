;;; docker-ts-mode : Major mode for Dockerfiles
(use-package dockerfile-ts-mode
  :if (treesit-language-available-p 'dockerfile)
  :mode ("Dockerfile" "\\.dockerfile\\'" "\\.docker\\'"))
