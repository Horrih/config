;;;; my/sexp-other-side
(defun my/sexp-other-side()
  "uses `backward-sexp' and `forward-sexp' to switch position between
start and end of sexp. If inside a sexp, go the the start of the sexp.
Does not work for strings, since they do not have separate start/end characters"
  (interactive)
  (cond
   ((seq-contains "([{" (char-after)) (forward-sexp))
   ((seq-contains ")}]" (char-before)) (backward-sexp))
   (t (backward-up-list))))

;;;; my/comment-dwim
(defun my/comment-dwim()
  "Like `comment-dwim', but comment line if cursor at beginning of line"
  (interactive)
  (call-interactively
    (if (or (region-active-p) (/= (line-beginning-position) (point)))
        #'comment-dwim
      #'comment-line)))
