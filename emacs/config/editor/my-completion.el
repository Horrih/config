;; Various packages providing completion for various commands -*- lexical-binding: t; -*-
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

;;; corfu : Completion frontend, used by lsp
;;;; corfu-terminal : Corfu's UI only works in GUI emacs
(use-package corfu-terminal
  :hook (corfu-mode . (lambda()
                        (unless (display-graphic-p)
                          (corfu-terminal-mode t)))))

;;;; corfu itself
(use-package corfu
  :custom
  (corfu-auto t)  ; Auto completion as you type (can be triggered manyally with completion-at-point)
  (corfu-cycle t) ; Cycle when at bottom of candidates
  :bind (:map corfu-map ("M-c" . corfu-insert-separator))
  :init
  (require 'corfu-info)  ; M-g/M-h to see location/help for candidate
  (global-corfu-mode))   ; Enable corfu everywhere

;;;; corfu in the minibuffer
(defun my/corfu-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'my/corfu-minibuffer)
