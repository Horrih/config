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


(defun my/config-load()
  "Loads the modules defined in `my/config-modules'."
  (interactive)
  (dolist (pair my/config-modules)
    (dolist (file (cadr pair))
      (let* ((dir (car pair))
             (path (format "%s%s/my-%s.el"  my/config-dir dir file)))
        (load path nil t)))))

(defun my/config-list-available-modules()
  "Looks into `my/config-dir' for available config modules"
  (interactive)
  (let* ((found-files (seq-filter
                       (lambda(name)(not (string-match-p "my-config.el" name)))
                       (directory-files-recursively my/config-dir ".*.el")))
         (sorted-files (sort found-files #'string<))
         (parent-dir (lambda(file)
                       (intern
                        (file-name-nondirectory
                         (directory-file-name (file-name-directory file))))))
         (grouped-files (seq-group-by parent-dir sorted-files))
         (base-name (lambda(file) (file-name-sans-extension (file-name-nondirectory file))))
         (module-name (lambda(file) (replace-regexp-in-string "my-" "" (funcall base-name file))))
         (extract-modules (lambda(file-list) (mapcar (lambda(file)
                                                       (intern (funcall module-name file)))
                                                     file-list)))
         (result (mapcar (lambda(dir)
                           (list (car dir) (funcall extract-modules (cdr dir))))
                         grouped-files)))
    (message "Found modules:\n%s" (pp result))
    result))

(defun my/config-list-unused-modules()
  "Looks into `my/config-dir' for available config modules, and compares it to `my/config-modules'"
  (interactive)
  (message
   "The following modules are currently not enabled:\n%s"
   (pp
    (seq-filter
     (lambda(dir-modules) (cadr dir-modules))
     (mapcar
      (lambda(dir-modules)
        (let* ((dir (car dir-modules))
               (available-modules (cadr dir-modules))
               (enabled-modules (cadr (assoc dir my/config-modules))))
          (list dir (seq-filter
                     (lambda(module)
                       (not (seq-contains-p enabled-modules module)))
                     available-modules))))
      (my/config-list-available-modules))))))
