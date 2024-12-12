;;; legacy python mode
(use-package python-mode
  :straight (:type built-in)
  :hook (python-mode . (lambda()
                         (setq-local fill-column 88)  ; ruff/black default limit
                         (my/apheleia-enable-ruff) ; ruff code formatter
                         )))

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


;;; Ruff formatter through apheleia
(defun my/apheleia-enable-ruff()
  "Register ruff as python formatter"
  (require 'apheleia)
  (unless (assoc 'uv-ruff apheleia-formatters)
    (push '(uv-ruff . ("uvx" "ruff" "format" "--stdin-filename" filepath))
          apheleia-formatters)
    (setf (alist-get 'python-mode apheleia-mode-alist) '(uv-ruff)))
  (apheleia-mode-maybe))



