;;; Vertico : Completion for commands in a vertical way
(use-package vertico
  :init (vertico-mode)
  :hook (completion-list-mode . (lambda()(setq-local show-trailing-whitespace nil)))  ; Disable whitespace check in completion buffers (e.g M-:)
  :custom-face
  (vertico-current ((t (:background "#264f78")))) ; Current selected item shown as blue
  :custom
  (vertico-cycle t)
  (vertico-count 15))

;;; Marginalia : Display additional completion data (doc strings, file permissions...)
(use-package marginalia
  :init (marginalia-mode)
  :custom-face
  (completions-annotations ((t (:inherit 'shadow))))) ; Disable italic since it is translated to underline in terminal

;;; Orderless : Matching of several patterns without order in completion
(use-package orderless
  :custom-face
  (orderless-match-face-0 ((t (:weight bold :foreground "gold1")))) ; Display the first matching part as yellow gold
  :custom
  ((completion-styles '(orderless basic))
   (completion-category-defaults nil)
   (completion-category-overrides '((file (styles partial-completion))))))

;;; Consult : a collection of commands that improve emacs defaults
(use-package consult
  :bind (:map my/keys-mode-map
         ("M-y" . consult-yank-pop)
         :map help-map
         ("I" . consult-info)
         :map ctl-x-map
         ("b" . consult-buffer))
  :config
  (recentf-mode))

;;; Expand Region : expand or contract selection
(use-package expand-region)

;;; Helpful : nice looking and more complete help buffers
(use-package helpful
  :bind (:map help-map
              ("p" . helpful-at-point)
              ("s" . helpful-symbol)
              ("v" . helpful-variable)
              ("f" . helpful-callable)
              ("k" . helpful-key)))

;;; Dired : built-in navigation of folders
(use-package dired
  :straight (:type built-in)
  :bind (:map dired-mode-map
              ("u" . dired-up-directory)
              ("i" . nil))
  :custom
  (dired-listing-switches "-alh")
  (dired-kill-when-opening-new-dired-buffer t)) ; Auto close previous folder buffer

