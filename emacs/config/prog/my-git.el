;;;; magit : Git front end (amazing!)
(use-package magit
  :custom-face (magit-filename ((t :foreground "white"))) ; Otherwise untracked files have the same color as title in git status
  :custom
  (magit-no-confirm t) ; Do not ask for confirmation for actions
  (magit-visit-ref-behavior '(checkout-any focus-on-ref))) ; Enter on branch names makes you checkout the branch

;;;; ediff : Built in side by side diffs of files
(use-package ediff
  :straight (:type built-in)
  :hook (ediff-keymap-setup . (lambda()
                        (keymap-set ediff-mode-map "h" 'ediff-status-info)
                        (keymap-set ediff-mode-map "'" 'other-window)
                        (keymap-set ediff-mode-map "4" 'my/other-window-reverse)
                        (keymap-set ediff-mode-map "i" 'ediff-previous-difference)
                        (keymap-set ediff-mode-map "k" 'ediff-next-difference)))
  :custom
  (ediff-split-window-function 'split-window-horizontally)) ; Make ediff split side by side

;;; Magit transient
(transient-define-prefix my/transient-magit()
  "Transient for `magit' commands"
  [["Magit commands"
    ("s" "Status (Home)"        magit-status)
    ("f" "File commands"        magit-file-dispatch)
    ("v" "Global Commands"      magit-dispatch)
    ("l" "Log"                  magit-log)
    ("y" "Show branches"        magit-show-refs)
    ("b" "Browse other branch"  magit-find-file-other-window)
    ("w" "Switch worktree"      magit-worktree-status)
    ("c" "Clone"                magit-clone)
    ]])
(keymap-set ijkl-local-mode-map "v" 'my/transient-magit)

;;; Magit ijkl
(with-eval-after-load "magit"
  (key-chord-define magit-log-select-mode-map "CC" 'magit-log-select-pick)
  (key-chord-define magit-log-select-mode-map "QQ" 'magit-log-select-quit)
  (dolist (keymap (list magit-diff-section-base-map magit-mode-map))
    (key-alias keymap "ç" "M-c ç")
    (key-alias keymap "&" "M-c 1")
    (key-alias keymap "é" "M-c 2")
    (key-alias keymap "\"" "M-c 3")
    (key-alias keymap "'" "M-c '")
    (key-alias keymap "4" "M-c 4")
    (key-alias-fallback keymap "b" "M-c b" "C-c b")
    (key-alias-fallback keymap "v" "M-c v" "C-c v")
    (key-alias-fallback keymap "m" "RET" "C-c m")
    (key-alias-fallback keymap "j" "C-j" "C-c j")
    (key-alias-fallback keymap "i" "C-p" "C-c i")
    (key-alias-fallback keymap "l" "C-l" "C-c l")
    (key-alias-fallback keymap "k" "C-n" "C-c k"))

  ;; hack to define the default flags for magit log
  ;; by default they were different if launched in status or from a buffer
  ;; which I don't like
  (cl-defmethod transient-init-value ((obj magit-log-prefix))
    (oset obj value '("--graph" "-n256" "--decorate")))
)
(with-eval-after-load "git-rebase"
  (key-alias-fallback git-rebase-mode-map "m" "RET" "C-c m")
  (key-alias-fallback git-rebase-mode-map "j" "C-l" "C-c j")
  (key-alias-fallback git-rebase-mode-map "i" "C-p" "C-c i")
  (key-alias-fallback git-rebase-mode-map "k" "C-n" "C-c k")
  (key-alias-fallback git-rebase-mode-map "l" "C-f" "C-c l")
  (keymap-set git-rebase-mode-map "d" 'git-rebase-kill-line))
(with-eval-after-load "with-editor"  ; Called for commits
  (diminish "with-editor-mode")
  (key-chord-define with-editor-mode-map "CC" 'with-editor-finish)
  (key-chord-define with-editor-mode-map "QQ" 'with-editor-cancel))

