;;; esup : Launch the esup command to measure startup time of each emacs plugin
(use-package esup
  :disabled
  :custom (esup-depth 0)) ; Sometimes fixes the bug https://github.com/jschaf/esup/issues/54
