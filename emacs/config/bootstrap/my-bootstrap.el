;;; Optimize garbage collection : improves the startup time
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()(setq gc-cons-threshold 800000)))

;;; Use-package configuration
;;;; Enable use-package imenu integration
;; Needs to be done before any use-package call
(setq use-package-enable-imenu-support t)

;;;; Main configuration
(use-package use-package
  :custom
  (use-package-always-defer t)) ; Lazy load by default, use :demand otherwise
