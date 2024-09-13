;;; Outline mode
(use-package outline
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . (lambda()
                       (setq-local outline-regexp "^;;;;* ")
                       (outline-minor-mode)))
  :config
  (diminish 'outline-minor-mode)
  :custom
  (outline-minor-mode-cycle t) ; Tab and S-Tab cycle between different visibility settings
  (outline-minor-mode-cycle-filter 'bolp)) ; Cycle only when on line beginning


;;; Pretty colors for headings
;; We don't use (outline-minor-mode-highlight 'override) because it applies to some non headings as well
(use-package outline-minor-faces
   :hook (outline-minor-mode . outline-minor-faces-mode))

;;; Hydra
(defhydra my/hydra-outline(:columns 3)
  "outline"
  ("u" outline-up-heading "up")
  ("TAB" outline-toggle-children "toggle hide/show children")
  ("a" outline-show-all "show all")
  ("n" outline-next-visible-heading "next")
  ("l" outline-hide-sublevels "hide level")
  ("s" outline-show-subtree "show all subtree")
  ("p" outline-previous-visible-heading "prev")
  ("h" outline-hide-subtree "hide subtree"))
(keymap-set ijkl-local-mode-map "à" 'my/hydra-outline/body)
