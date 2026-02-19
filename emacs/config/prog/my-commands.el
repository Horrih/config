;;; Execute/eval commands
(transient-define-prefix my/transient-commands()
  "Execute commands"
  [["Elisp"
    (":" "Interpret lisp code" eval-expression)
    ("f" "Interpret current defun expression" eval-defun)
    ("e""Interpret last lisp expression" eval-last-sexp)
    ("E" "Interpret the current buffer as lisp" eval-buffer)
    ("b" "Interpret the whole lisp buffer" eval-buffer)
    ]
   ["bash"
    ("s" "Shell Command" async-shell-command)
    ("t" "Open a terminal" my/term-new)
    ]
   ["Misc"
    ("!" "Emacs commands" execute-extended-command)
    ]])
