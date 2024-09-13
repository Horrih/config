;;; legacy c++ mode
(use-package cc-mode
  :straight (:type built-in)
  :config
  (setq-default c-basic-offset  4) ; Base indent size when indented automatically
  (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
  (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
  (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
  (advice-add 'c-update-modeline :override #'ignore)) ;; Don't use a modeline suffix (i.e C++//l)


;;; c/c++-ts-mode : Major mode for C/C++ files
;;;; Override some treesitter rules
(defun my/cpp-indent-style()
  "Override the built-in K&R indentation style with some additional rules"
  `(
    ((node-is ")") parent-bol 0) ; Otherwise aligned with opening parenthese
    ((node-is "compound_statement") parent-bol 0) ; Blocks with {}
    ((parent-is "declaration_list") parent-bol 0) ; Do not indent namespace blocks
    ((parent-is "template_declaration") parent-bol 0) ; function declaration on the line following the template declaration
    ((parent-is "comment") parent-bol 0) ; Align comments at the start of the comment section
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset) ; Standard indent if argument starts on next-line
    ((parent-is "argument_list") prev-line 0) ; Align argument with previous one systematically
    ((node-is "field_initializer_list") parent-bol ,(/ c-ts-mode-indent-offset 2)) ; class field initializer : use half i
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset) ; Standard indent if parameter starts on next-line
    ((parent-is "parameter_list") prev-line 0) ; Align parameter with previous one systematically
    ,@(alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))

;;;; Configure the C++ treesitter major mode
(use-package c-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style #'my/cpp-indent-style)
  :hook (c-ts-base-mode . (lambda() (setq-local my/margin-line-width 100)))
  :init
  (advice-add 'c-ts-mode-set-modeline :override 'ignore) ; Do not add a // suffix to the modeline
  ;; Remap the standard C/C++ modes
  (add-to-list 'major-mode-remap-alist '(c-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c++-ts-mode)))


;;; cmake-ts-mode : Major mode for CMakeLists.txt
(use-package cmake-ts-mode
  :if (treesit-language-available-p 'cmake)
  :mode ("CMakeLists\\.txt" "\\.cmake\\'")
  :custom (cmake-ts-mode-indent-offset 4)
  :config
  (setq cmake-ts-mode--indent-rules
        `((cmake
           ,@(alist-get 'cmake cmake-ts-mode--indent-rules)
           ((parent-is "source_file") parent-bol 0)   ; If top level, no indent
           ((match nil nil nil 1 nil) prev-line 0) ; Otherwise align with previous-sibiling
           ((match nil nil nil nil nil) parent-bol cmake-ts-mode-indent-offset))))) ; No sibling : parent + offset

;;; Clang format
(use-package clang-format)
(defun my/clang-format-save-hook()
  "Create a buffer local save hook to apply `clang-format-buffer'"
  ;; Only format if .clang-format is found
  (when (locate-dominating-file "." ".clang-format")
    (clang-format-buffer))
  ;; Continue to save
  nil)

(define-minor-mode my/clang-format-on-save-mode
  "Minor mode to enable/disable automated clang format on save"
  :lighter " ClangFormat"
  (if my/clang-format-on-save-mode
      (add-hook 'before-save-hook 'my/clang-format-save-hook nil t)
    (remove-hook 'before-save-hook 'my/clang-format-save-hook t)))

(define-globalized-minor-mode my/clang-format-auto-mode my/clang-format-on-save-mode
  (lambda()(my/clang-format-on-save-mode t))
  :predicate '(c-mode c-ts-mode c++-mode c++-ts-mode c-or-c++-mode c-or-c++-ts-mode))
(my/clang-format-auto-mode t) ; Turn it on

;;; Hydra gdb/gud
(transient-define-prefix my/transient-gdb()
  "Transient for gdb/gud manipulation"
  [["Navigation"
    ("n" "Next line"  gud-next :transient t)
    ("i" "Up frame"   gud-up   :transient t)
    ("k" "Down frame" gud-down :transient t)
    ("s" "Step in"    gud-step :transient t)
    ("m" "Switch to *gud* buffer" gdb-display-gdb-buffer)
    ]
   ["Breakpoints"
   ("b" "Enable breakpoint"  gud-break :transient t)
   ("B" "Disable breakpoint" gud-remove)
   ("v" "Print variable"     gud-print)
   ]
   ["Start/stop"
    ("g" "Start GDB" gud-gdb    :transient t)
    ("p" "Start PDB" pdb        :transient t)
    ("r" "Run"       gud-run    :transient t)
    ("f" "Finish"    gud-finish :transient t)
    ("c" "Continue"  gud-cont   :transient t)
    ]
   ["Misc"
    ("q" "Quit"                   ignore)
    ("&" "Close other windows"    delete-other-windows :transient t)
    ("4" "Switch to other window" other-window         :transient t)
    ]])
