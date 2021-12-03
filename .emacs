;;;;;;;;;;;;;;;;;;;;;;;  GESTIONNAIRE D'EXTENSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ajout du dépot d'extensions MELPA
;; cf Getting Started https://melpa.org/
;; ELPA, le dépot par défaut, n'a pas grand chose...
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat "http" "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;    CUSTOMISATION       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (setq debug-on-error 't) ; Debugger si erreur rencontrée à la lecture de ce fichier
  (column-number-mode 't) ; display column numbers
  (line-number-mode 't) ; display line number
  (size-indication-mode 't) ; display size indication
  (delete-selection-mode 1) ; Si un texte est sélectionné, on s'attend à ce que taper du nouveau texte supprime la sélection
  (show-paren-mode 1) ;surligne en couleur les parentheses correspondantes
  (setq-default show-trailing-whitespace t) ; Afficher les espaces
  (setq-default indent-tabs-mode nil) ; Ne pas utiliser de tabs pour indenter
  (setq default-tab-width 4) ; Taille des tabulations
  (setq c-basic-offset 4)    ; Offset standard entre deux lignes : 4 espaces
  (menu-bar-mode -1) ; Hide Menu bar
  (fset 'yes-or-no-p 'y-or-n-p) ; Abreviate Yes/No
  (setq compilation-always-kill t) ; Ne pas demander si je veux interrompre la compilation en cours
  (c-set-offset (quote cpp-macro) 0 nil) ;Indentation des macros C/C++ comme du code classique
  (c-set-offset 'substatement-open 0) ;Pas d'indentation ajoutée sur les accolades : on veut qu'elles soie
  (setq make-backup-files nil) ; Suppresssion des fichiers de backup (filename~)
  (setq create-lockfiles nil)) ; Suppresssion des fichiers de lock (.#filename)
;; Theme utilisé pour les couleurs générales
(use-package vscode-dark-plus-theme
  :ensure t
  :config (load-theme 'vscode-dark-plus t))

;; Fonction pour renommer le fichier courant
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; F5 : revert-buffer sans confirmation
(defun revert-buffer-no-confirm(&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

;; Fonction qui rafraichit les buffers ouverts (par exemple après changement de branche git)
(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; Raccourcis remaniés
(progn
  (global-set-key (kbd "C-c c"  ) 'comment-or-uncomment-region)
  (global-set-key (kbd "M-g"    ) 'goto-line)
  (global-set-key (kbd "M-s"    ) 'multi-occur-in-matching-buffers) ; Recherche d'un texte/regex
  (global-set-key (kbd "C-j"    ) 'delete-backward-char) ;Possibilité de supprimer comme backspace
  (global-set-key (kbd "M-j"    ) 'backward-kill-word)   ;Possibilité de supprimer comme backspace
  (global-set-key (kbd "M-p"    ) 'backward-paragraph) ; Paragraphe précédent
  (global-set-key (kbd "M-n"    ) 'forward-paragraph) ; Paragraphe suivant
  (global-set-key (kbd "M-m"    ) 'exit-minibuffer) ;Possibilité de valider dans le minibuffer avec Alt-M
  (global-set-key (kbd "C-c o"  ) 'ff-find-other-file) ; to switch between header and implementation
  (global-set-key (kbd "<f8>"   ) 'recompile) ; Recompile le projet
  (global-set-key (kbd "S-<f8>" ) 'compile)   ; Compile le projet
  (global-set-key (kbd "M-[ 3 4 ~" ) 'compile)   ; Compile le projet
  (global-set-key (kbd "C-<f8>" ) 'kill-compilation) ; Interrompt la compilation en cours
  (global-set-key (kbd "<f2>"   ) 'rename-file-and-buffer) ; Renomme le fichier courant
  (global-set-key (kbd "<f5>"   ) 'revert-buffer-no-confirm) ; Rafraichit le fichier courant sans confirmation
  (global-set-key (kbd "<f6>"   ) 'revert-all-file-buffers) ; Rafraichit les fichiers ouverts quand on change de branche
  (global-set-key (kbd "C-c g"  ) 'magit-status) ; affichage du buffer de travail magit
  (global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally) ; Redimensionner les fenêtres horizontalement
  (global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "M-S-<down>") 'enlarge-window)
  (global-set-key (kbd "M-S-<up>") 'shrink-window))

;;;;;;;;;;;;;;;;;;;;;;     PACKAGES GENERAUX     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Front end pour Git
(use-package magit
  :ensure t
  :config
  (setq magit-no-confirm t)
  (setq magit-visit-ref-behavior '(checkout-any focus-on-ref)))

;; Fournit les raccourcis quand on cherche une commande en tapant le début des raccourcis
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Recherche simplifiée de commandes/variables/etc
;; On rebind certaines commandes natives d'emacs vers les fonctions de helm
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

;;;;;;;;;;;;;;;;;;;;;;     PACKAGES DE DEV       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fonctions de recherche dans le projet courant
(use-package projectile
  :ensure t
  :bind (("C-c f" . projectile-find-file)
         ("C-c s" . projectile-ag)))

(use-package treemacs
  :ensure t
  :bind ("C-c t" . treemacs))

(use-package flycheck
  :ensure t
  :hook
  (flycheck-mode . (lambda()
                     (when (string-equal major-mode "python-mode")
                       (flycheck-add-next-checker 'lsp 'python-flake8)))))

;; Fonctions de coloration syntaxique et d'autocomplétion
(use-package lsp-mode
  :ensure t
  :hook
  (
   (python-mode . lsp)
   (c++-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-links nil)
  (yas-global-mode)
  :bind (("C-c j" . lsp-find-definition) ; Jump vers la définition d'une fonction
         ("C-c J" . lsp-find-references) ; Trouver les références d'une fonction
         ("C-c e" . lsp-treemacs-errors-list) ; Trouver les erreurs dans les fichiers analysés
         ("C-h l" . lsp-describe-thing-at-point))) ; Trouver les erreurs dans les fichiers analysés

;; Backend de lsp mode pour code python
(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (when (string-equal major-mode "python-mode")
      (add-to-list 'lsp-disabled-clients 'pyls)
      (add-to-list 'lsp-enabled-clients 'jedi))))

;; Support des fichiers gitlab-ci
(use-package yaml-mode
  :ensure t)


;; Cacher/Montrer le contenu d'accolades ou if/else
(defun hide-show-mode-hook()
  (hs-minor-mode)
  (message "Activation de HS MODE")
  (local-set-key (kbd "C-c h") 'hs-toggle-hiding) ; Cacher/Montrer section courante
  (local-set-key (kbd "C-c H") 'hs-show-all) ;Montrer toutes les zones cachées
  (local-set-key (kbd "C-c M-h") 'hs-hide-all)) ; Tout cacher

(add-hook 'python-mode-hook (lambda()
                              (message "calling python hook")
                              (hide-show-mode-hook)
                              (set (make-local-variable 'compile-command) "python -m unittest")))
(add-hook 'c++-mode-hook (lambda()
                           (message "calling cpp hook")
                           (hide-show-mode-hook)
                           (set (make-local-variable 'compile-command) "make -j4")))
(add-hook 'emacs-lisp-mode-hook 'hide-show-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;      GÉNÉRÉ PAR EMACS     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode vscode-dark-plus-theme helm which-key use-package magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
