;;; org-roam : Notes organizing
(use-package org-roam
  :custom
  (org-roam-directory "~/.config/emacs/org_roam")
  (org-return-follows-link t)
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode))

(defun my/org-roam-pull-commit-push()
  "Git commit and push all the modified files in `org-roam-directory'"
  (interactive)
  (let ((default-directory org-roam-directory))
    (shell-command "git add -u")
    (shell-command "git commit -m 'Automated commit from org-roam-commit-and-push'" )
    (shell-command "git pull --rebase" )
    (shell-command "git push" )))

;;; org-download : Download images directly into org
(use-package org-download
  :commands (org-download-clipboard)
  :custom (org-image-actual-width 900)
  :config
  ;; Override the default dir to have one dir for each org file instead of per heading
  (defun org-download--dir-1 ()
    (or org-download-image-dir (concat (file-name-sans-extension (buffer-file-name)) "-img"))))

;;; Hydra org-roam
(defun my/org-now-time-stamp()
  "Inserts the timestamp of the current time without prompting the user"
  (interactive)
  (org-insert-time-stamp (current-time) t))

(defhydra my/hydra-org(:exit t :hint nil)
  "
^Agenda/TODOs^             ^Edition^                ^Org Roam^
---------------------------------------------------------------------------------
_a_: Show agenda           _l_: Insert link         _g_: Go to an org-roam file
_s_: Set scheduled date    _n_: Now timestamp       _i_: Link to an org-roam file
_d_: Set deadline date     _N_: Pick timestamp      _h_: Info for file
_t_: Cycle TODOs states    _y_: Yank(paste) image   _S_: Sync
_L_: List TODOs            _,_: Toggle images

"
  ("a" org-agenda-list)        ("l" org-insert-link)           ("i" org-roam-node-insert)
  ("s" org-schedule)           ("t" org-todo)                  ("g" org-roam-node-find)
  ("d" org-deadline)           ("y" org-download-clipboard)    ("h" org-roam-buffer-toggle )
  ("n" my/org-now-time-stamp)  ("L" org-todo-list)             ("S" my/org-roam-pull-commit-push)
  ("N" org-time-stamp)         ("," org-toggle-inline-images)

  ("q" nil "Quit"))
