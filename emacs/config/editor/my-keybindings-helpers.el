;;; my/keys minor mode for global keybindings overriding to be turned off/on
(define-minor-mode my/keys-mode
  "Minor mode to enable custom keybindings"
  :lighter ""
  :global t
  :keymap '())
(my/keys-mode)

;;; which-key : Displays command shortcuts when typing commands
(use-package which-key
  :demand
  :straight (:type built-in)
  :config (which-key-mode)
  :diminish)

;;; key-chord  : Enables combination of keys like zz
(use-package key-chord
  :demand
  :custom (key-chord-safety-interval-forward 0.1)
  :config (key-chord-mode))

;;; hydra : Keybindings combinations
(use-package hydra)

;;; Transient package : use the latest one and not the built-in not supported by magit
(use-package transient :demand)

;;; Helper functions for keybindings
;;;; Helper function gen-input
(defun my/gen-input(KEYS)
  "Generates a key `KEYS' sequence as if the user typed it"
  (setq unread-command-events (nconc (listify-key-sequence (kbd KEYS)) unread-command-events)))

;;;; Helper macro key-alias
(defmacro key-alias(keymap from to &optional except-modes)
  "Binds the key-binding FROM to the function called by typing TO.

The forwarding will only occur if the current major mode is not in EXCEPT-MODES list"
  `(keymap-set ,keymap ,from
               (defun ,(intern (format "my/%s-alias/%s/%s" keymap from to))(&optional args)
                 ,(format "Forwards the interactive call from %s to %s (bound by default to `%s')" from to (keymap-lookup nil to))
                 (interactive "P")
                 ;; By default, fetch the binding bound to `to'
                 (let ((to-call (keymap-lookup nil ,to))
                       (old-binding (keymap-lookup nil ,from)))
                   ;; If exception : then the appropriate command must be fetched in other keymaps
                   ;; This is done here by temporarily setting the `from' binding to nil in the input keymap
                   (when (my/mode-is-one-of-p ,except-modes)
                     (keymap-set ,keymap ,from nil) ; Disable the keybinding temporarily
                     (setq to-call (keymap-lookup nil ,from)) ; Get the command bound in other keymaps
                     (keymap-set ,keymap ,from old-binding)) ; Restore the original keybinding

                   ;; Call the appropriate function
                   (call-interactively to-call)))))

;;;; remap : new binding in the same keymap
(defun my/remap(keymap from to)
  "Creates a new binding TO in KEYMAP for the command bound to FROM"
  (let ((existing (keymap-lookup keymap from)))
    (when existing (keymap-set keymap to existing))))

;;;; key-alias-fallback : define binding and rebind existing if it exists
(defmacro key-alias-fallback(keymap from to fallback)
  "Like `key-alias' : binds FROM to function called with TO, remapping existing bindings to FALLBACK"
  `(progn
     (my/remap ,keymap ,from ,fallback)
     (key-alias ,keymap ,from ,to)))

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
(keymap-set my/keys-mode-map "M-c" ijkl-local-mode-map) ; Make ijkl bindings available in insert mode

(defun ijkl-local-mode-and-save()
  "Enables ijkl-local-mode and saves the current file if applicable"
  (interactive)
  (ijkl-local-mode)
  (when (and (buffer-modified-p) buffer-file-name)
    (save-buffer)))

;;;; ijkl global mode definition
(define-globalized-minor-mode ijkl-mode ijkl-local-mode
  (lambda()
    "Only enable the ijkl-local-mode by default on traditional buffers"
    (if (or (minibufferp)
            (string= major-mode "term-mode")
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
