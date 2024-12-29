;;; term.el configuration
;;;; use-package section
(use-package term
  :straight (:type built-in)
  :hook (ijkl-local-mode . my/term-toggle-line-mode-with-ijkl)
  :bind (:map term-raw-map
              ;; ("C-k" . (lambda()(interactive)(message "ZOB")))
              ("<f5>" . describe-key))
)

;;;; my/term-toggle-line-mode-with-ijkl() : Make it play nicely with ijkl mode
(defun my/term-toggle-line-mode-with-ijkl()
  "Link `ijkl-local-mode' to term `line-mode'"
  (when (string= major-mode "term-mode")
    (if ijkl-local-mode (term-line-mode) (term-char-mode))))

;;;; my/term-new : Open a new term buffer, named appropriately
(defun my/term-new()
  "Open a new term buffer with bash and renmae it with the launch directory."
  (interactive)
  (term "/bin/bash")
  (rename-buffer (format "*terminal %s*" default-directory)))
