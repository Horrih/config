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
  :hook (c-ts-base-mode . (lambda()
                            (setq-local my/margin-line-width 99)
                            (apheleia-mode-maybe)))
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

;;; Switch header/cpp/tests
;;;; Header switches, improves on ff-find-other-file
(defun my/find-project-filename(filename)
  "Finds a file named `FILENAME' in the current project and open it

If the filename is not found, return nil."
  (let* ((project-files (project-files (project-current)))
         (result (seq-contains-p project-files filename
                                 (lambda(f1 f2)
                                   (when (string= (file-name-nondirectory f1) f2)
                                     f1))))
         )
    (when result
      (find-file result))))

(defun my/cpp-switch-header()
  "Switches between .h and .cpp file, like `ff-find-other-file' but works
well even if the files are not next to each other."
  (interactive)
  (let* ((basename (file-name-base (buffer-file-name)))
         (extension (file-name-extension (buffer-file-name)))
         (other-ext (cond
                     ((string= extension "h") "cpp")
                     ((string= extension "cpp") "h")))
         (other-file (format "%s.%s" basename other-ext)))
    (unless (my/find-project-filename other-file)
      (error "Could not perform .cpp<->.h switch : '%s' does not exist" other-file))))

(defun my/switch-tests()
  "Switches between a source file and its test file (assumed starts with test_)."
  (interactive)
  (let* ((basename (file-name-nondirectory (buffer-file-name)))
         (name-with-tests (format "test_%s" basename))
         (name-wo-tests (string-remove-prefix "test_" basename))
         (other-file (if (string= basename name-wo-tests)
                         name-with-tests
                       name-wo-tests)))
    (unless (my/find-project-filename other-file)
      (error "Could not perform tests<->src switch : '%s' does not exist" other-file))))
