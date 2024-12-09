;;; Org mode : package customizations
(use-package org
  :straight (:type built-in)
  :custom-face
  (org-warning ((t (:underline nil)))) ; Do not underline org-warnings, red is enough
  (org-done ((t (:foreground "lightgreen" :box(:color "lightgreen")) )))
  (org-document-title ((t (:weight bold :height 1.6))))
  (org-level-1        ((t (:height 1.2))))
  (org-level-2        ((t (:height 1.1))))
  (org-block          ((t (:inherit 'fixed-pitch))))
  :custom ((org-agenda-files '("~/.config/emacs/org_roam")) ; For autopopulating todos from notes
           (org-todo-keywords '((sequence "A FAIRE(t)" "EN ATTENTE(w@/!)" "|" "ANNULÉ(c@/!)" "FAIT(d!)")))
           (org-todo-keyword-faces '(("EN ATTENTE" . "gold") ("ANNULÉ" . "grey")))
           (org-agenda-span 'month) ; To have a monthly view by default
           (org-startup-folded 'content)
           (org-agenda-start-on-weekday 1) ; Agenda starts on monday in agenda
           (calendar-week-start-day 1) ; Date picker starts on monday
           (org-capture-bookmark nil)) ; To disable adding a bookmark on each org capture
  :hook
  (org-agenda-mode . (lambda()
                       (keymap-set org-agenda-mode-map "C-c p" 'org-agenda-earlier)
                       (keymap-set org-agenda-mode-map "C-c n" 'org-agenda-later)))
  (org-mode . (lambda()
                (require 'org-tempo) ; For templates like <sTAB to insert a code block
                (key-chord-define org-mode-map "CC" 'org-ctrl-c-ctrl-c) ; Close notes by typing CC
                (org-indent-mode) ; Auto indent lines according to depth
                (auto-fill-mode) ; Wrap lines when longer than fill column

                ;; Ignore org files from recentf due to agenda loading everything
                (require 'recentf)
                (add-to-list 'recentf-exclude ".*org$"))))

(use-package org-indent
  :straight (:type built-in)
  :diminish)

;;; Org bullets : Pretty mode for org
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;; Org ijkl
(with-eval-after-load "org"
  ;; Use ijkl in the date selection buffer
  (key-alias org-read-date-minibuffer-local-map "m" "RET")
  (key-alias org-read-date-minibuffer-local-map "i" "S-<up>")
  (key-alias org-read-date-minibuffer-local-map "j" "S-<left>")
  (key-alias org-read-date-minibuffer-local-map "k" "S-<down>")
  (key-alias org-read-date-minibuffer-local-map "l" "S-<right>"))
