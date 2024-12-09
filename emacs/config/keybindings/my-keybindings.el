;;; movement and deletion bindings (accessible in both modes)
;;;; backwards
(keymap-set ijkl-local-mode-map "j"   'backward-char)
(keymap-set    my/keys-mode-map "C-j" 'backward-char)
(key-alias     my/keys-mode-map "M-j" "M-b")
(key-alias     my/keys-mode-map "C-M-j" "C-a")
(key-alias  ijkl-local-mode-map "a" "C-a")

;;;; forwards
(keymap-set ijkl-local-mode-map   "l" 'forward-char)
(keymap-set    my/keys-mode-map "C-l" 'forward-char)
(key-alias     my/keys-mode-map "M-l" "M-f")
(key-alias     my/keys-mode-map "C-M-l" "C-e")
(key-alias  ijkl-local-mode-map "e" "C-e")

;;;; upwards
(keymap-set ijkl-local-mode-map "i" 'previous-line)
;; C-i is bound to TAB in terminals. You need to remap C-i to C-p at your GUI app level
;; For example powertoys on windows, konsole or xterm remapping on linux
(when (display-graphic-p)
  (keymap-set input-decode-map "C-i" "C-<i>") ; Disable C-i -> TAB
  (key-alias my/keys-mode-map "C-<i>" "C-p")) ; Rebind C-i as previous line

(keymap-set   my/keys-mode-map "M-i" (lambda() (interactive)(previous-line 7)))
(key-alias    my/keys-mode-map "C-M-i" "M-<")
(key-alias ijkl-local-mode-map "<" "M-<")
(key-alias ijkl-local-mode-map "A" "C-M-a")

;;;; downwards
(keymap-set    my/keys-mode-map "C-k" 'next-line)
(keymap-set ijkl-local-mode-map   "k" 'next-line)
(keymap-set   my/keys-mode-map "M-k" (lambda() (interactive)(next-line 7)))
(key-alias    my/keys-mode-map "C-M-k" "M->")
(key-alias ijkl-local-mode-map ">" "M->")
(key-alias ijkl-local-mode-map "E" "C-M-e")

;;;; deletion
(key-alias  ijkl-local-mode-map "u" "C-M-u" '("dired-mode" "Info-mode"))
(keymap-set    my/keys-mode-map "C-u" 'delete-backward-char)
(keymap-set    my/keys-mode-map "C-c u" 'universal-argument)
(keymap-set    my/keys-mode-map "C-M-u" 'my/delete-start-or-previous-line)
(keymap-set    my/keys-mode-map "M-u" 'backward-kill-word)
(keymap-set    my/keys-mode-map "C-o" 'delete-forward-char)
(keymap-set    my/keys-mode-map "C-M-o" 'kill-line)
(key-alias  ijkl-local-mode-map "o" "C-M-o")
(keymap-set    my/keys-mode-map "M-o" 'kill-word)

;;; utility bindings
(keymap-set    my/keys-mode-map "C-+" 'text-scale-increase) ; Increase text size with Ctrl +
(keymap-set    my/keys-mode-map "C--" 'text-scale-decrease) ; Decrease text size with Ctrl -
(key-alias  ijkl-local-mode-map "+" "C-+" '("dired-mode")) ; See above
(key-alias  ijkl-local-mode-map "-" "C-\-") ; See above
(keymap-set ijkl-local-mode-map "TAB" nil)    ; Do not override tab binding
(keymap-set ijkl-local-mode-map "<tab>" nil)  ; Do not override tab binding
(keymap-set ijkl-local-mode-map "h" help-map) ; Use the help functions
(keymap-set ijkl-local-mode-map "x" 'my/delete-char-or-kill-region)
(keymap-set ijkl-local-mode-map "X" (lambda()(interactive)(just-one-space -1))) ; Just one space multiline
(keymap-set    my/keys-mode-map "C-c SPC" (lambda()(interactive)(just-one-space -1)))
(keymap-set           ctl-x-map "k" 'kill-current-buffer) ; Replace C-x k (kill buffer) with kill-current-buffer
(keymap-set           ctl-x-map "f" 'find-file) ; Replace C-x f (set-fill-column) with find-file (C-x C-f usually)
(keymap-set         ctl-x-r-map "d" 'bookmark-delete) ; Repace C-x r d (delete-rectangle) with delete bookmark
(key-alias  ijkl-local-mode-map "m"   "C-m")
(key-alias     my/keys-mode-map "C-S-m" "S-<return>")
(key-alias     my/keys-mode-map "M-m" "C-<return>")
(key-alias  ijkl-local-mode-map "1"   "C-x 0")
(key-alias  ijkl-local-mode-map "&"   "C-x 1")
(key-alias  ijkl-local-mode-map "2" "C-x 2")
(key-alias  ijkl-local-mode-map "3" "C-x 3")
(keymap-set ijkl-local-mode-map "é"  'my/pick-window-below)
(keymap-set ijkl-local-mode-map "\"" 'my/pick-window-right)
(keymap-set ijkl-local-mode-map "'" 'other-window)
(keymap-set ijkl-local-mode-map "4" 'my/other-window-reverse)
(key-alias ijkl-local-mode-map "w" "C-x C-s")
(keymap-set ijkl-local-mode-map "z" 'recenter-top-bottom)
(keymap-set ijkl-local-mode-map "r" ctl-x-r-map)
(key-alias  ijkl-local-mode-map "c" "M-w")
(key-alias  ijkl-local-mode-map "y" "C-y")
(key-alias  ijkl-local-mode-map "_" "C-_") ; Undo
(key-alias  ijkl-local-mode-map "8" "C-M-_") ; redo
(keymap-set ijkl-local-mode-map "p" 'asmr-backward) ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "n" 'asmr-forward)  ; Reimplementation of a mark ring
(keymap-set ijkl-local-mode-map "P" 'previous-buffer)
(keymap-set ijkl-local-mode-map "N" 'next-buffer)
(key-alias  ijkl-local-mode-map "<SPC>" "C-@")
(keymap-set ijkl-local-mode-map "I" 'er/expand-region)   ; Expand the selection progressively
(keymap-set ijkl-local-mode-map "K" 'er/contract-region) ; Reduce the selection progressively
(keymap-set ijkl-local-mode-map "." 'completion-at-point) ; Trigger completion

;;; Misc
(keymap-set ijkl-local-mode-map "/"     'my/comment-dwim) ; Comment region or line
(keymap-set ijkl-local-mode-map "M-s"   'multi-occur-in-matching-buffers) ; Search in all buffers
(keymap-set ijkl-local-mode-map "<f2>"  'rename-visited-file) ; Rename the current file/buffer
(keymap-set ijkl-local-mode-map "<f5>"  'revert-buffer-quick) ; Refreshes the current file/buffer without confirmation
(keymap-set ijkl-local-mode-map "<f12>" 'my/include-c-header) ; Shortcuts for a #include directive

;;; Resize the window when split using split screen (C-2 or C-3)
(keymap-set ijkl-local-mode-map "C-M-<right>" 'enlarge-window-horizontally)
(keymap-set ijkl-local-mode-map "C-M-<left>" 'shrink-window-horizontally)
(keymap-set ijkl-local-mode-map "C-M-<down>" 'enlarge-window)
(keymap-set ijkl-local-mode-map "C-M-<up>" 'shrink-window)


;;; isearch ijkl
(with-eval-after-load "isearch"
  ;; Make C-u delete the last character of isearch
  ;; Since there is no isearch-del-word, make M-u delete the last 10 characters
  (keymap-set isearch-mode-map "C-u" 'isearch-del-char)
  (keymap-set isearch-mode-map "M-u" (lambda() (interactive) (isearch-del-char 10))))
