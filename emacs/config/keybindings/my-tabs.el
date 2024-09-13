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
(keymap-set ijkl-local-mode-map "t" 'my/hydra-tabs/body)

;;;; my/tab-move-left
(defun my/tab-move-left()
  "Like `tab-move' but to the left"
  (interactive)
  (tab-move -1))
