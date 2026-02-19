;;; Transient search text -*- lexical-binding: t; -*-
;;;; Transient definition
(transient-define-prefix my/transient-search() "Transient for all string query/replace"
  [["Incremental search"
    ("s" "Forward"         isearch-forward)
    ("S" "Backward"        isearch-backward)
    ("w" "Symbol at point" isearch-forward-symbol-at-point)
    ]
   ["Occurences"
    ("o" "In file"              consult-line)
    ("b" "In all buffers"       consult-line-multi)
    ("p" "In current project"   consult-grep)
    ("P" "In current project (point)" my/consult-grep)
    ("a" "In current directory" my/consult-directory)
    ]
   ["Replace"
    ("r" "String"  query-replace)
    ("R" "Regexp"  query-replace-regexp)
    ("P" "Project" project-query-replace-regexp)
    ]])

;;;; my/consult-directory
(defun my/consult-directory()
  "`consult-grep' in `default-directory'"
  (interactive)
  (consult-grep default-directory))

;;;; my/consult-grep
(defun my/consult-grep()
  "`consult-grep' with current symbol as start string"
  (interactive)
  (funcall #'consult-grep nil (format "%s" (symbol-at-point))))

;;; Transient for find commands
;;;; Transient definition
(transient-define-prefix my/transient-find() "Transient for various find commands"
  [["Find files"
    ("d" "Dired - Current director" dired-jump)
    ("f" "By path"                  find-file)
    ("p" "Project"                  project-find-file)
    ("P" "Project in other window"  my/project-find-file-other-window)
    ("l" "Locate file"              consult-locate)
    ("m" "Man page"                 consult-man)
    ]
   ["Coding"
    ("e" "List errors (file)"     my/list-errors-file)
    ("E" "List errors (project)"  flymake-show-project-diagnostics :if (lambda() (featurep 'flymake)))
    ("r" "Find references (xref)" xref-find-references)
    ("o" "Switch header/cpp"      my/cpp-switch-header)
    ("t" "Switch source/tests"    my/switch-tests)
    ]])

;;;; my/project-find-file-other-window()
(defun my/project-find-file-other-window()
  "Like `project-find-file' but opens the file in another window"
  (interactive)
  (cl-letf (((symbol-function 'find-file) 'find-file-other-window))
    (project-find-file)))


;;; Navigation transient
;;;; Keybindings
(transient-define-prefix my/transient-go() "Transient for various navigation commands"
  [["Go to"
    ("l" "Line n°"         consult-goto-line)
    ("p" "Go to project"   project-switch-project)
    ("b" "Bookmark"        bookmark-jump)
    ("," "Org roam file"   org-roam-node-find)
    ("c" "Column n°"       move-to-column)
    ("h" "Outline heading" consult-outline)]
   ["LSP Navigation"
    ("j" "Definition"              xref-find-definitions)
    ("J" "Definition other window" xref-find-definitions-other-window)
    ("m" "Symbol (imenu) in current buffer" consult-imenu)
    ("M" "Symbol (imenu) in all buffers" consult-imenu-multi)
    ("e" "Next error"     flymake-goto-next-error :transient t :if (lambda() (featurep 'flymake)))
    ("E" "Previous error" flymake-goto-prev-error :transient t :if (lambda() (featurep 'flymake)))]
   ["Expressions"
    ("k" "Forward expressionn" forward-sexp     :transient t)
    ("i" "Backward expression" backward-sexp    :transient t)
    ("u" "Upward expression "  backward-up-list :transient t)
    ("g" "Go to opposite end"  my/sexp-other-side)
    ("q" "Quit" ignore)]])


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
