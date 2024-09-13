;;; Automatic margin
;;;; Customize variables to tweak margin size
(defcustom my/margin-center-column 45
  "Column that should appear at the center when auto margin mode is enabled"
  :local t
  :type '(integer))

(defcustom my/margin-right-ratio 0.75
  "Ratio of the right margin to the left one"
  :local t
  :type '(float))

;;;; Implementation functions
(defun my/margin--compute(window)
  "Computes the left margin for window `WINDOW' if width big enough, 0 otherwise"
  (let* ((width (window-width window))
         (prev-margin-left (or (car (window-margins window)) 0))
         (prev-margin-right (or (cdr (window-margins window)) 0))
         (total-width (+ width prev-margin-left prev-margin-right)))
    (with-current-buffer (window-buffer window)
      (if (or display-line-numbers-mode (minibufferp))  ; No margin if minibuffer or line numbers
          '(0 . 0)
        (let* ((left-margin (- (/ total-width 2) my/margin-center-column))
               (right-margin (truncate (* left-margin my/margin-right-ratio))))
          `(,(max 0 left-margin) . ,(max 0 right-margin)))))))

(defun my/margin--set()
  "Margin if single window, no margin if split"
  (walk-windows
   (lambda(window)
       (with-current-buffer (window-buffer window)
         (if my/margin-auto-local-mode
             (let* ((margins (my/margin--compute window))
                    (left-margin (car margins))
                    (right-margin (cdr margins)))
               (setq-local split-window-preferred-function #'my/margin--reset-split-window-sensibly)
               (set-window-parameter window 'split-window 'my/margin--reset-split-window)
               (set-window-margins window left-margin right-margin))
           (setq-local split-window-preferred-function #'split-window-sensibly)
           (set-window-parameter window 'split-window nil)
           (set-window-margins window nil))))
   nil t))

(defun my/margin--reset()
  "Reset the margins to 0 for all windows"
  (walk-windows (lambda(w)
                  (set-window-parameter w 'split-window nil)
                  (set-window-margins w nil))
                nil t))

(defun my/margin--reset-split-window (&optional window size side pixelwise)
  "Call `split-window' after resetting margins (which interfer with splitting)"
  (my/margin--reset)
  (split-window window size side pixelwise))

(defun my/margin--reset-split-window-sensibly (&optional window)
  "Call `split-window-sensibly' after resetting margins (which interfer with splitting)"
  (my/margin--reset)
  (split-window-sensibly window))

;;;; Define the auto-margin minor mode, local and global
(define-minor-mode my/margin-auto-local-mode
  "Minor mode to enable/disable left margin"
  :lighter " Margin"
  (if my/margin-auto-local-mode
      (add-hook 'window-configuration-change-hook 'my/margin--set nil t)
    (my/margin--set)
    (remove-hook 'window-configuration-change-hook 'my/margin--set t)))

(define-globalized-minor-mode my/margin-auto-mode my/margin-auto-local-mode
  (lambda()
    (unless (my/mode-is-one-of-p '("magit-log-mode"))
      (my/margin-auto-local-mode t))))
(my/margin-auto-mode t) ; Turn it on
(diminish 'my/margin-auto-local-mode)


