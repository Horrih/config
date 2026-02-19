;;; legacy python mode
(use-package python-mode
  :straight (:type built-in)
  :custom (lsp-pylsp-plugins-flake8-max-line-length 88)
  :hook (python-mode . my/python-mode-hook))

(defun my/python-mode-hook()
  "Various settings to be set when entering python-mode."
  (setq-local fill-column 88)  ; ruff/black default limit
  (setq-local my/compile-lint-project-command #'my/python-lint-project-command)
  (setq-local my/compile-test-project-command #'my/python-test-project-command)
  (setq-local my/compile-test-file-command #'my/python-test-file-command)
  (setq-local my/compile-test-case-command #'my/python-test-case-command)
  (my/apheleia-enable-ruff)) ; ruff code formatter

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

;;; ruff flymake
(use-package flymake-ruff
  :hook (flymake-mode . flymake-ruff-load)
  :custom
  (flymake-ruff-program-args '("check" "--select" "ALL" "--output-format" "concise" "--exit-zero" "--quiet" "-")))

;;; Test and linting commands
;;;; Test project command
(defun my/python-test-project-command()
  "Callback used by `my/compile-test-project'"
  (list (my/python-root-dir) "uv run pytest"))

;;;; Test file command
(defun my/python-test-file-command()
  "Callback used by `my/compile-test-file'"
  (let* ((this-file (buffer-file-name))
         (command (format "uv run pytest -s -vvv %s" this-file)))
    (list default-directory command)))

;;;; Test single test case command
(defun my/python-test-case-command()
  "Callback used by `my/compile-test-case'"
  (let* ((this-file (buffer-file-name))
         (this-function (which-function))
         (command (format "uv run pytest -s -vvv %s::%s" this-file this-function)))
    (if this-function
        (list default-directory command)
      (error "Cursor is not inside a function : could not deduce test case to run."))))

;;;; Run linters for the current project
(defun my/python-lint-project-command()
  "Callback used by `my/compile-lint-project'"
  (list (my/python-root-dir) "uvx ruff check"))

;;;; my/python-root-dir : Get the python root directory
(defun my/python-root-dir()
  "Return the project's python root directory.
- Return the upmost pyproject.toml
- Else return current directory"
  (let* ((start-dir default-directory)
        (highest-found start-dir)
        (current-match nil))
    ;; Keep climbing as long as we find a pyproject.toml
    (while (setq current-match (locate-dominating-file start-dir "pyproject.toml"))
      (setq highest-found current-match)
      ;; Move start-dir to the parent of the current match to keep climbing
      (setq start-dir (file-name-directory (directory-file-name current-match))))
    highest-found))
