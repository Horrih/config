;; Various commands and settings related to window management  -*- lexical-binding: t; -*-
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

;;; Window management using registers
(defun my/dir-to-register(register)
  "Saves the current directory in a register. Opening it will use dired"
  (interactive (list (register-read-with-preview "Directory to register : ")))
  (set-register register (cons 'file default-directory)))

;;;; Transient definition
(transient-define-prefix my/transient-register() "Transient for all register operations"
  [["Register - Save"
    ("p" "Save point" point-to-register)
    ("w" "Save window configuration" window-configuration-to-register)
    ("d" "Save directory" my/dir-to-register)
    ("R" "Load register" jump-to-register)
    ("x" "Clear register" my/clear-register)
    ]])

;;;; Clear register
(defun my/clear-register(char)
  "Clear register currently referred by `CHAR'"
  (interactive "cClear register at : ")
  (set-register char nil))

;;; Hydra tabs
;;;; Hydra
(defhydra my/hydra-tabs(:hint nil :exit t)
  "
^Create tabs^          ^Move^                   ^Misc^
------------------------------------------------------------------------
_t_: New tab           _n_: Next tab            _s_: Switch to tab
_r_: Rename tab        _p_: Previous tab        _l_: Switch to most recent
_x_: Close tab         _m_: Move tab right      _h_: Toggle tab-bar-mode
_u_: Restore tab       _M_: Move tab left

"
  ("t" tab-new   )     ("n" tab-next        )   ("s" tab-switch)
  ("r" tab-rename)     ("p" tab-previous    )   ("l" tab-recent)
  ("x" tab-close )     ("m" tab-move        )   ("h" tab-bar-mode)
  ("u" tab-undo  )     ("M" my/tab-move-left)

  ("q" nil "Quit"))

;;;; my/tab-move-left
(defun my/tab-move-left()
  "Like `tab-move' but to the left"
  (interactive)
  (tab-move -1))
