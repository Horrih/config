;; Various commands and settings related to text edition  -*- lexical-binding: t; -*-
;;; my/switch-to-last-buffer
(defun my/switch-to-last-buffer()
  "Use `switch-to-buffer' to visit the last buffer"
  (interactive)
  (switch-to-buffer nil))

;;; my/delete-start-or-previous-line
(defun my/delete-start-or-previous-line()
  "Use `kill-line' to delete either the start of the line, or the previous line if empty"
  (interactive)
  (kill-line (if (= (line-beginning-position) (point)) -1 0)))

;;; my/replace-chat-at-point
(defun my/replace-char-at-point(char)
  "Replaces the caracter at point by `CHAR'"
  (interactive "cReplace character at point with : ")
  (delete-char 1)
  (insert-char char)
  (backward-char 1))

;;; my/delete-char-or-kill-region
(defun my/delete-char-or-kill-region()
  "If mark is active, kill region, otherwise delete-char"
  (interactive)
  (call-interactively
   (if mark-active
       'kill-region
     'delete-char)))

;;; Expand Region : expand or contract selection
(use-package expand-region)

;;; Rectangle
;;;; my/replace-char-or-rectangle-region
(defun my/replace-char-or-rectangle-region()
  "If mark is active, rectangle actions, otherwise replace-char"
  (interactive)
  (call-interactively
   (if mark-active
       'my/hydra-rectangle/body
     'my/replace-char-at-point)))

;;;; Hydra
(defhydra my/hydra-rectangle(:exit t :columns 2)
  "Rectangle operations"
  ("t" string-rectangle "Edition")
  ("k" kill-rectangle "Cut")
  ("c" copy-rectangle-as-kill "Copy")
  ("C" clear-rectangle "Clear")
  ("y" yank-rectangle "Paste")
  ("o" open-rectangle "Insert whitespace")
  ("n" rectangle-number-lines "Number the lines"))

;;; Commenting
(defun my/comment-dwim()
  "Like `comment-dwim', but comment line if cursor at beginning of line"
  (interactive)
  (call-interactively
   (if (or (region-active-p) (/= (line-beginning-position) (point)))
       #'comment-dwim
     #'comment-line)))
