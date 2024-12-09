;;; legacy python mode
(use-package python-mode
  :straight (:type built-in)
  :hook (python-mode . (lambda() (setq-local fill-column 88))))

;;; code coverage
(defun my/toggle-coverage-overlay()
  (interactive)
  (cl-case major-mode
    (c++-mode    (call-interactively #'python-coverage-overlay-mode))
    (c-mode      (call-interactively #'python-coverage-overlay-mode))
    (c++-ts-mode (call-interactively #'python-coverage-overlay-mode))
    (python-mode (call-interactively #'python-coverage-overlay-mode))
    (t (user-error "No coverage overlay specified for mode %s" major-mode))))

(use-package python-coverage
  :config
  (require 'magit))  ; Uses magit's faces

;;; Formatter black
(use-package blacken
  :hook (python-mode . (lambda() (blacken-mode))))
