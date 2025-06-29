;;; my/switch-to-last-buffer  -*- lexical-binding: t; -*-
(defun my/switch-to-last-buffer()
  "Use `switch-to-buffer' to visit the last buffer"
  (interactive)
  (switch-to-buffer nil))

;;; my/delete-start-or-previous-line
(defun my/delete-start-or-previous-line()
  "Use `kill-line' to delete either the start of the line, or the previous line if empty"
  (interactive)
  (kill-line (if (= (line-beginning-position) (point)) -1 0)))

;;; Window management
;;;; my/other-window-reverse
(defun my/other-window-reverse()
  "Like `other-window' but in the reverse order"
  (interactive)
  (other-window -1))

;;;; my/pick-window-right
(defun my/pick-window-right()
  "Like `split-window-right' except it lets you pick the buffer on the other side"
  (interactive)
  (let ((split-height-threshold nil)  ; Forbid vertical split
        (split-width-threshold 0))  ; Allow horizontal split up to extreme cases
    (consult-buffer-other-window)))

;;;; my/pick-window-below
(defun my/pick-window-below()
  "Like `split-window-below' except it lets you pick the buffer on the other side"
  (interactive)
  (let ((split-width-threshold nil)  ; Forbid horizontal split
        (split-height-threshold 0)) ; Allow vertical split up to extreme cases
    (consult-buffer-other-window)))

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

;;; my/mode-is-one-of-p helper function
(defun my/mode-is-one-of-p(modes)
  "Returns t if the current modes (minor or major) matches one in the input modes list"
  (let (res)
    (dolist (input-mode modes res)
      (dolist (mode (cons major-mode minor-mode-list))
        (when (string-equal input-mode mode)
          (setq res t))))))
