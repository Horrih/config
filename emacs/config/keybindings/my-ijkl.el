;;; Modal edition mode using ijkl for movement
;;;; ijkl minor mode definition
(define-minor-mode ijkl-local-mode
  "Minor mode to enable movement using ijkl"
  :lighter " ijkl"
  :keymap '(([remap self-insert-command]  ignore)) ; The actual keymaps are defined later below
  (add-to-list 'emulation-mode-map-alists '(ijkl-local-mode . ijkl-local-mode-map))
  (ijkl-insert-mode (if ijkl-local-mode -1 1))
  )
(diminish 'ijkl-local-mode)

(define-minor-mode ijkl-insert-mode
  "Minor mode for key bindings to be used when `ijkl-local-mode' is disabled"
  :keymap '())

(keymap-set ijkl-local-mode-map "d" 'ijkl-local-mode) ; Leave ijkl mode with d
(key-chord-define ijkl-insert-mode-map "jj" 'ijkl-local-mode-and-save) ; Enter ijkl mode with "jj"
(key-chord-define ijkl-insert-mode-map "qq" 'ijkl-local-mode) ; Leave ijkl mode with "qq"
(keymap-set my/keys-mode-map "C-q" 'ijkl-local-mode) ; Fallback if "kk" key-chord fails (e.g high latency ssh)
(keymap-set my/keys-mode-map "M-q" 'quoted-insert) ; Rebind the command originally bound to C-q
(keymap-set ijkl-insert-mode-map "M-c" ijkl-local-mode-map) ; Make ijkl bindings available in insert mode

(defun ijkl-local-mode-and-save()
  "Enables ijkl-local-mode and saves the current file if applicable"
  (interactive)
  (ijkl-local-mode)
  (when (and (buffer-modified-p) buffer-file-name)
    (save-buffer)))

;;;; ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode on traditional buffers"
    (if (or (minibufferp)
            (string-match "[Gg]it" (format "%s" major-mode))
            (string-match "[Gg]it" (format "%s" major-mode))
            (string-equal (buffer-name) "*Org Note*")
            (string-equal (buffer-name) "*Ediff Control Panel*")
            (and
             (string-equal (buffer-name) "COMMIT_EDITMSG")
             (not (string-match-p "rebase" (buffer-string)))))
        (ijkl-insert-mode)
      (ijkl-local-mode))))
(ijkl-mode)


