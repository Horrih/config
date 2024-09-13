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
