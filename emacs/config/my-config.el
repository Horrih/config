(defcustom my/config-modules
  '((bootstrap   (bootstrap straight))
    (editor      (settings plugins commands asmr))
    (looks       (settings theme auto-margin doom-modeline))
    (keybindings (base ijkl keybindings transient))
    (org         (org-mode notes present))
    (prog        (settings compilation commands git plugins markdown
                  web c++ python yaml docker outline corfu linters eglot lsp-mode)))
  "Enabled init modules. Can be modified to
- disable modules that are enabled by default here
- enable additional modules that are disabled by default here"
  :type '(alist :key-type symbol :value-type (repeat symbol)))

(defvar my/config-dir (file-name-directory load-file-name)
  "The directory containing the various configuration elisp files")

(dolist (pair my/config-modules)
  (dolist (file (cadr pair))
    (let* ((dir (car pair))
           (path (format "%s%s/my-%s.el"  my/config-dir dir file)))
      (load path nil t))))
