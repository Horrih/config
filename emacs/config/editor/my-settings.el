(global-auto-revert-mode) ; Refresh files automatically when modified from outside emacs
(savehist-mode t) ; Save history for commands
(customize-set-variable 'isearch-resume-in-command-history t) ; Use history for isearch as well
(customize-set-variable 'enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(customize-set-variable 'enable-local-variables :all) ; Enable by default variables in .dir-locals.el

;; Display the stacktrace if error encountered in one of the lisp method
(customize-set-variable 'debug-on-error t) ; Enable it at first to troubleshoot our config
(add-hook 'after-init-hook #'toggle-debug-on-error) ; Disable it after startup

(put 'narrow-to-region 'disabled nil) ; Allow narrow to region without prompt
(customize-set-variable 'send-mail-function 'mailclient-send-it) ; E-mail : open default mail client, e.g for report-emacs-bug
(customize-set-variable 'recenter-positions '(top middle bottom)) ; Start recenter on top instead of middle
(customize-set-variable 'make-backup-files nil) ; Do not use backup files (filename~)
(customize-set-variable 'create-lockfiles nil) ; Do not use lock files (.#filename)
