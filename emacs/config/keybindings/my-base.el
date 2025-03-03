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

;;; Helper function gen-input
(defun my/gen-input(KEYS)
  "Generates a key `KEYS' sequence as if the user typed it"
  (setq unread-command-events (nconc (listify-key-sequence (kbd KEYS)) unread-command-events)))

;;; Helper macro key-alias
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

;;; remap : new binding in the same keymap
(defun my/remap(keymap from to)
  "Creates a new binding TO in KEYMAP for the command bound to FROM"
  (let ((existing (keymap-lookup keymap from)))
    (when existing (keymap-set keymap to existing))))

;;; key-alias-fallback : define binding and rebind existing if it exists
(defmacro key-alias-fallback(keymap from to fallback)
  "Like `key-alias' : binds FROM to function called with TO, remapping existing bindings to FALLBACK"
  `(progn
     (my/remap ,keymap ,from ,fallback)
     (key-alias ,keymap ,from ,to)))
